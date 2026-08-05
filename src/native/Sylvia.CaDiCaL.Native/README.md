# Sylvia.CaDiCaL.Native

A thin `extern "C"` DLL over the CaDiCaL 3.0.1 C++ API, handing back opaque pointers for the .NET
interop layer to hold. Same pattern as `GambitSharp.Api.Native`, different toolchain — see
[Why not MSVC](#why-not-msvc).

## Why this exists

The stock `ccadical.h` C API cannot do three things we need.

**1. In-process proof capture.** `CaDiCaL::Tracer` is an abstract C++ class, so a proof observer
must be a C++ object with a vtable — it cannot live in .NET at all. The file-based route
(`ccadical_trace_proof`) works but writes LRAT text to disk, and a file proof is a *non-incremental*
artifact: one merged proof per session, with no way to attribute steps to the solve that produced
them.

The alternative would be rustsat's design, a struct of 15 C function pointers. That is right for
Rust — no GC, no managed transition. From .NET it would make every derived clause during search a
native→managed transition needing a GC-rooted delegate, on the solver's hot path, in a process where
a fault is a hard kill with no stack trace. `ProofCapture` buffers into C++ vectors instead and
hands the proof back **once** as flat CSR arrays. No managed callbacks during search.

**2. Not aborting the process.** CaDiCaL enforces its API contract with `REQUIRE(...)`, which prints
`cadical: fatal error:` and calls `abort()`. That is not an exception and cannot be caught — it
takes the CLR down. Three ways to trigger it, all measured against `bin/cadical.dll`:

| call | outcome on 3.0.0 |
|---|---|
| `add` before declaring variables | process aborted |
| `set_option` after configuration ended | process aborted |
| `close_proof` after an UNSAT solve closed it | process aborted |

This layer shadows the state machine and returns `SC_ERR_STATE` instead. (3.0.1 defaults `factor`
off, which removes the first trigger — but the class of hazard remains.)

**3. One definition of `--plain`.** `--plain` is a CLI shortcut, **not** an option:
`ccadical_set_option(s, "plain", 1)` is silently accepted and does nothing. Measured on pigeonhole
4→3, that silence costs a **48-step proof instead of 15** — and preprocessing is what introduces the
RAT steps `SAT.rup_chain` cannot replay, which previously made reconstruction fail outright.
`sc_set_plain` is the single definition of the bundle.

## Build

Needs a mingw-w64 g++ — the *same one* that built `bin/libcadical.a`.

MSYS2:

```bash
pacman -S --needed mingw-w64-x86_64-gcc make binutils
```

Then, from an **MSYS2 MinGW 64-bit** shell (not the MSYS shell — that targets the Cygwin-like
`msys-2.0.dll` runtime):

```bash
make -C src/native/Sylvia.CaDiCaL.Native
```

Output is `bin/sylvia_cadical.dll`. `make check` builds it and runs
[`examples/sat/NativeSmoke.fsx`](../../../examples/sat/NativeSmoke.fsx).

The link is `-static -static-libgcc -static-libstdc++`, so the result is self-contained: unlike
`bin/cadical.dll` it needs no `libstdc++-6.dll` / `libgcc_s_seh-1.dll` / `libwinpthread-1.dll`
beside it, and loads from a plain Windows process with nothing on `PATH`.

### Rebuilding libcadical.a

If `make` reports a version mismatch, rebuild the archive from the same tree the headers come from:

```bash
cd reference/projects/cadical-rel-3.0.1 && ./configure && make
```

then copy `build/libcadical.a` to `bin/`.

This check is not bureaucracy. `ProofCapture` subclasses `CaDiCaL::Tracer`, so its vtable is
generated from `tracer.hpp` **at compile time**. Link that against an archive built from a different
`tracer.hpp` and calls dispatch to the wrong slot — memory corruption at solve time, not a link
error. `make check-version` compares `VERSION` against the version string in the archive and refuses
to build on a mismatch.

## Why not MSVC

`GambitSharp.Api.Native` is a `.vcxproj`. That does not transfer:

- `libcadical.a` is a GNU `ar` archive; MSVC cannot link it.
- Subclassing `Tracer` means matching its vtable layout — Itanium C++ ABI, not MSVC's.
- The virtuals take `const std::vector<int> &`, which is **libstdc++'s** vector, with a different
  layout from MSVC's.
- CaDiCaL 3.0.1 has no MSVC support: `_MSC_VER` appears nowhere in the tree, and six sources include
  POSIX headers.

A g++-built DLL exporting `extern "C"` P/Invokes perfectly well — `bin/cadical.dll` already is one.

## Design

Deliberately dumb: it captures and hands back, and performs no reasoning. Everything at the level of
Sylvia `Prop`s stays in F#, and the solver stays out of the trusted base — an `Unsat` verdict is not
a proof until the trace is replayed through the kernel.

- **Opaque handle.** `SylviaCadical *` owns the `Solver`, the `ProofCapture`, the proof buffer and
  the deadline terminator. .NET holds the pointer and never interprets it.
- **Status codes everywhere.** Every entry point returns `SC_OK` or an `SC_ERR_*`; `sc_last_error`
  carries the message. `SC_GUARD` catches every C++ exception at the boundary, since an exception
  crossing into .NET is undefined behaviour rather than a catchable error.
- **CSR proof export.** Literals and antecedents live in two arenas; each step records a half-open
  range into them. Readback is three `memcpy`s and one marshal, not a walk over per-step
  allocations.
- **Cooperative timeout.** `sc_set_timeout_ms` arms a `Terminator` rather than killing a process, so
  a timeout leaves the handle usable and becomes an outcome instead of a loss.

### What the F# side gains

`sc_proof_export` yields the same `(id, literals, antecedents)` triple that
[`SAT.parse_lrat`](../../lang/solvers/Sylvia.Solver.CaDiCaL/CaDiCaL.fs) recovers from text, so
`reconstruction_plan` is unchanged — but with two fragilities retired:

- `SC_STEP_ORIGINAL` reports CaDiCaL's own input-clause ids, replacing the unstated assumption that
  they are `1..m` in DIMACS order.
- `witness` marks RAT clauses directly, replacing the indirect inference from a negative hint.

And `sc_proof_reset` plus `sc_assume` / `sc_failed` are the prerequisites for incremental
reconstruction: one self-contained refutation per round, with a minimized core over hypotheses.
