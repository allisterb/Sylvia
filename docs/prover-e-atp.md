# E as an Advisory ATP Backend for Sylvia

Design and implementation notes for `Sylvia.ATP.E` — an **optional, advisory** integration of the
[E theorem prover](https://eprover.org) (Stephan Schulz) into Sylvia. Companion to
[`prover-automation.md`](prover-automation.md) and [`prover-set-theory.md`](prover-set-theory.md).

Runnable demos:

```
dotnet fsi examples/atp/E.fsx              # translation + verdicts against live E
dotnet fsi examples/atp/Sledgehammer.fsx   # the relevance-filter + reconstruction loop
```

Both require E installed; pass its path to `EProver(exePath = …)` or set the `SYLVIA_EPROVER`
environment variable. The bundled Windows build is at `bin/eprover-E-3.3.5/eprover.exe`; the E manual
is `reference/books/eprover.pdf`.

## 1. What this is, and what it is not

E is a saturation-based automated theorem prover (ATP) for full first-order logic with equality, built
on the superposition calculus. Here it is used **purely as an advisory oracle**: it answers two
questions —

- *Does a proof of this goal exist?* (an SZS status)
- *Which of the supplied axioms did the proof use?* (a minimal-ish fact set)

— and it is **never admitted to Sylvia's trusted base**. A `Theorem` verdict from E is **not** a Sylvia
proof. The intended workflow (after Isabelle's *Sledgehammer*) is: let E establish provability and
surface the relevant facts, then let Sylvia's own equational kernel build a human-readable,
kernel-checked proof from that starting point.

This deliberately mirrors, and complements, the existing `Sylvia.Solver.Z3` backend.

## 2. Division of labour: ATP vs SMT vs decision procedures

Three automated-reasoning capabilities now sit under Sylvia, with genuinely different cores. The labels
are not cosmetic — they tell you where to route a goal.

| Engine | Kind | Core | Strong on | Weak on |
|--------|------|------|-----------|---------|
| **`metaset` / `metasubset`** (in-kernel) | Decision procedure | Equational, membership-route + complete ANF prop prover | The Boolean set-algebra fragment (`∪ ∩ ~ ∅ U`, `=`/`⊆`): instant, complete, human-readable | Anything outside that fragment |
| **`Sylvia.Solver.Z3`** | SMT **solver** | DPLL(T), theory decision procedures + model finding | Arithmetic, arrays, bitvectors, quantifier-free theory combinations; counterexamples | General FOL proof search (not refutation-complete) |
| **`Sylvia.ATP.E`** | **ATP** | Superposition / paramodulation, refutation-complete for FOL= | Quantified / relational goals over uninterpreted symbols and equality | Background theories (arithmetic); and — see §6 — the set-identity encoding |

The key point: **E earns its keep exactly where Sylvia's own decision procedures stop** — quantified,
relational first-order goals (e.g. subset-hypothesis reasoning, transitivity/antisymmetry chains,
`∀/∃` chaining). On the decidable Boolean fragment, `metaset` is strictly better (guaranteed
termination, a checked and readable proof); E is not needed there and in fact struggles with it (§6).

"Is Z3 really a solver rather than an ATP?" — keeping the distinction is the accurate choice. Z3 is an
SMT *solver* (theory decision procedures, model finding, not FOL-refutation-complete); E is a classical
*ATP*. The `Solver.Z3` / `ATP.E` naming encodes the routing.

## 3. Architecture

```
   Sylvia goal + named lemmas (Prop)
              │  tptpProblem  (§4)
              ▼
        TPTP FOF problem  ──►  eprover  ──►  stdout
                                               │  parse (§5)
                                               ▼
                              EResult { Status; UsedFacts; Tptp; Raw }
              ┌────────────────────────────────┴───────────────────┐
              ▼ (advisory only — never trusted)                     ▼
   relevance filter: which Sylvia lemmas?              Sledgehammer loop (§7):
                                                       reconstruct a kernel-checked
                                                       Sylvia proof from those facts
```

The project `src/lang/atp/Sylvia.ATP.E/` is a thin, leaf backend mirroring `Sylvia.Solver.Z3`. It
references only `Sylvia.Expressions` (+ the two Runtime projects) — the translator needs `Prop`,
`Formula`/`FsExpr` patterns and `expand`, nothing from `Sylvia.Prover`. Reconstruction (§7), which does
use the prover's automation, lives one layer up (in the example, for now).

`namespace Sylvia; module ATP` exposes:

- `EStatus` — `Theorem | CounterSatisfiable | Unsatisfiable | Satisfiable | GaveUp | ResourceOut | Timeout | NotAvailable | Other of string`
- `EResult = { Status; UsedFacts: string list; Tptp: string; Raw: string }`
- `tptpOfProp : Prop -> string`, `fofLine`, `tptpProblem`
- `EProver(?exePath, ?timeoutMs)` with `.Prove(axioms, goal, ?goalName)`, `.Problem`, `.IsAvailable`, `.ExePath`, `.TimeoutMs`

`exePath` defaults to the `SYLVIA_EPROVER` env var, else `eprover.exe` (PATH). No path is hardcoded in
the library.

## 4. Translation: Sylvia `Prop` → TPTP FOF

`tptpOfProp` recurses over the raw quotation using Sylvia's existing active patterns
(`Formula.(|Not|Equals|Implies|ForAll|Exists|True|False|)`, `FsExpr.(|And|Or|)`).

| Sylvia | TPTP FOF |
|--------|----------|
| `∀x \| R : B` (`forall_expr` / `qall`) | `![X]: (R => B)`  (or `![X]: B` when `R = true`) |
| `∃x \| R : B` (`exists_expr` / `qex`) | `?[X]: (R & B)`  (or `?[X]: B`) |
| `∧ ∨ ¬ ⇒` | `& \| ~ =>` |
| `=` on **bool** operands | `<=>` |
| `=` on **term** operands | `=` |
| predicate / function application | `f(a, b, …)` |

**Symbol vs variable (the subtle part).** A well-formed first-order translation hinges on classifying
each leaf correctly:

- A **base-sort `Var`** (e.g. an `intvar`) is a *term variable* → upper-case. If it is quantifier-bound
  it is bound by that quantifier; if left free it is universally closed with a leading `![…]`.
- A **function-typed `Var`** (type `'t -> …`, i.e. a predicate or function symbol) → lower-case functor.
  Authored as `symbolic_var<int->int> "f"` (function) or `symbolic_var<int->int->bool> "rho"` (binary
  relation), then applied in a quotation (`<@ (%f) %x @>`, `<@ (%rho) %x %y @>`); the translator's
  `Application` flattening turns curried applications into `f(a,b,…)`.
- A **bool-typed `Var`** (a `boolvar`) is a *propositional atom* (0-ary predicate) → lower-case symbol,
  **not** a universally-closed term variable.
- A **named value** `ScalarConst<'t>(n)` (embedded as `ValueWithName`) is a *constant* → lower-case
  nullary functor — distinct from a free variable (which would be universally closed).

The translator is generic over the first-order surface: quantifiers, connectives, term/bool equality,
constants, functions, and n-ary predicates/relations. `examples/atp/E.fsx` exercises all of these (5/5),
including `∀x. f(f x)=x, f a=b ⊢ f b=a` (constants + function term + equality) and the classic
"symmetric + transitive + serial ⇒ reflexive" over a binary relation. The one unhandled edge is bare
*unnamed* numeric literals (arithmetic) — which belong to Z3's lane anyway.

## 5. Runner — and the Windows / MSYS2 findings

The bundled E is built with the **MSYS2 POSIX toolchain** (it links `msys-2.0.dll`). Native MinGW/UCRT
cannot build E — its C uses `fork()`, `getrusage()`, `setrlimit()`, `sys/resource.h` with no `_WIN32`
shims — so the Cygwin-style emulation environment is forced. This drove several runner design choices,
each verified empirically:

- **E is fast — the emulation is not the bottleneck.** A real set-theory FOL goal (subset transitivity)
  proves in **~39 ms**. CPU-bound superposition pays only a modest emulation tax.
- **Enforce the timeout in the wrapper, never with E's flags.** `--cpu-limit` relies on `SIGXCPU`/rlimit,
  which is a no-op under this build; `--soft-cpu-limit` was also unreliable. The runner owns the clock:
  `WaitForExit(timeoutMs)` then `Kill()`. A wrapper timeout is reported as `Timeout` and must **never**
  be read as a verdict.
- **Avoid `fork()`.** E's default `--auto-schedule` forks children to race strategies across cores, and
  `fork()` is Cygwin/MSYS's worst-performing call. The runner uses `--auto` (a single auto-selected
  strategy, no fork). `--auto-schedule=1` (serial, no fork) is the alternative if a strategy portfolio
  is ever wanted.
- **Windows paths.** Pass Windows paths to `eprover.exe`; Git-Bash `/tmp` ≠ the MSYS eprover's `/tmp`.
- **Drain stdout.** After a timed `WaitForExit` returns, the async `OutputDataReceived` handlers may not
  have flushed — a follow-up blocking `WaitForExit()` avoids an intermittently empty capture.

**Parsing.** The status line is `# SZS status <X>` or `% SZS status <X>` (both handled). With
`--proof-object`, E reprints each used input formula as `fof(name, …, file('<path>', <name>))`; the used
facts are extracted with the regex `file\([^,]*,\s*([A-Za-z0-9_]+)\s*\)`, dropping the conjecture name.

If max throughput is ever needed, a **native Linux build under WSL2** (no emulation) is the pragmatic
route — but note it would *not* rescue the diverging goals of §6, which are a search problem, not speed.

## 6. What E is bad at: the set-identity encoding

A natural FOL encoding of set algebra — element+set in one sort, `mem/2`, the operators as **function
symbols** each with a **biconditional** membership axiom (`mem(X, union(S,T)) <=> mem(X,S) | mem(X,T)`,
etc.), plus extensionality and the subset definition — makes E's saturation **diverge** on set
*identities*. De Morgan, commutativity, even a single-direction subset De Morgan all time out, while
pure subset / implication goals (transitivity) are instant. The cause is the generative `⇐` direction of
the biconditionals: with the operators as functions and the set variables universally quantified, E
builds unbounded `union(…)` towers.

This was pinned down conclusively: `--auto-schedule=1` (the full strategy portfolio, serial, **no fork**)
still times out — so it is a *search / encoding* problem, not a fork/scheduling/speed problem, and a
faster or native build would not fix it. E does solve many TPTP `SET`-domain problems, so a better
encoding exists (orienting the membership definitions as directed rewrites, or the formulations those
problems use); that is Phase-1 R&D. In the meantime this is a feature, not a bug: **the identity fragment
is precisely what `metaset` already decides completely and readably.** Point E at the goals beyond it.

## 7. The reconstruction (Sledgehammer) loop

`examples/atp/Sledgehammer.fsx` closes the loop, honestly scoped. `sledgehammer facts goal` returns:

```fsharp
type Outcome =
    | Reconstructed of Theorem * string list   // E filtered; Sylvia certified (∧ used) ⇒ goal
    | ProvableButManual of string list         // E confirmed + filtered; residual beyond Sylvia auto
    | Unproved of EStatus                       // E did not establish it
```

The mechanism:

1. `e.Prove(facts, goal)`; map `res.UsedFacts` names back to the supplied Sylvia lemmas — the
   **relevance filter**.
2. Form `(∧ used-facts) ⇒ goal` and prove it with Sylvia's **complete** propositional prover
   `autoproof_anf`. This succeeds exactly when the residual is propositional (the
   *propositional-modulo-lemmas* fragment), giving a genuine, kernel-checked `Theorem`.
3. If the residual is quantified, `autoproof_anf` throws → `ProvableButManual`, and the narrowed fact set
   is the starting point for a hand / LLM proof.

Demonstrated (3/3): (1) goal `s` buried among seven propositional lemmas → E filters to `{f1, f2, f3}`
and Sylvia certifies `((p⇒q) ∧ (q⇒s) ∧ p) ⇒ s`; (2) an `∀/∃` chain → E filters + confirms, boundary
reported; (3) a non-theorem → `CounterSatisfiable`, not reconstructed.

The boundary in (2) is the current frontier: full automated reconstruction of genuinely first-order
goals awaits Sylvia gaining FOL-level proof automation (a Metis-style internal certifier). Until then, E
delivers the two halves that don't need it — a provability verdict and a relevance filter.

## 8. Files

- `src/lang/atp/Sylvia.ATP.E/E.fs` — the `ATP` module: translator, `EProver`, status/fact parsing.
- `src/lang/atp/Sylvia.ATP.E/Sylvia.ATP.E.fsproj` — thin project (references `Sylvia.Expressions` + Runtime).
- `examples/atp/E.fsx` — translation + verdicts against live E (5/5).
- `examples/atp/Sledgehammer.fsx` — relevance filter + reconstruction loop (3/3).
- `bin/eprover-E-3.3.5/eprover.exe` — the bundled Windows (MSYS2) build; `reference/books/eprover.pdf` — the manual.

## 9. Status and next steps

- **Translation** — general FOL surface done (quantifiers, connectives, term/bool equality, constants,
  functions, n-ary predicates, propositional atoms). Edge left: unnamed numeric literals.
- **Runner** — Windows-correct (no fork, wrapper timeout, drained output, path handling).
- **Loop** — relevance filtering + native certification for the propositional-modulo-lemmas fragment;
  honest boundary otherwise.
- **Open** — (a) a set-algebra encoding E can actually saturate (§6); (b) full FOL reconstruction, gated
  on Sylvia FOL automation (§7); (c) promoting the Sledgehammer orchestration from the example into a
  module (e.g. `Sylvia.ATP.Sledgehammer`) once the reconstruction side matures.
