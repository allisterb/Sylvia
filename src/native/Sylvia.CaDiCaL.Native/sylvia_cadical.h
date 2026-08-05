/* Sylvia native interop layer for CaDiCaL 3.0.1.
 *
 * A thin `extern "C"` facade over the CaDiCaL C++ API, handing back OPAQUE POINTERS the .NET side
 * holds without interpreting. It exists for three reasons, none of which the stock `ccadical.h` C
 * API can serve:
 *
 *   1. PROOF CAPTURE IN PROCESS.  `CaDiCaL::Tracer` is an abstract C++ class; a proof observer has
 *      to be a C++ object with a vtable, so it cannot live in .NET at all. Implementing it here also
 *      avoids the design rustsat uses (a struct of 15 C function pointers): that is fine for Rust,
 *      but from .NET every derived clause during search would become a native->managed transition
 *      needing a GC-rooted delegate, on the solver's hot path, in a process where a fault is a hard
 *      kill. `ProofCapture` buffers the whole proof into C++ vectors instead and hands it back ONCE
 *      as flat CSR arrays. No managed callbacks during search.
 *
 *   2. NO PROCESS ABORTS.  CaDiCaL enforces its API contract with `REQUIRE(...)`, which prints
 *      `cadical: fatal error:` and calls `abort()`. That is not an exception and cannot be caught —
 *      it takes the CLR down with no stack trace. Measured, three separate ways: adding a literal
 *      before declaring variables, setting an option after configuration ended, closing an already
 *      closed proof. This layer tracks the state machine itself and returns `SC_ERR_STATE` where
 *      CaDiCaL would abort. Every entry point returns a status; `sc_last_error` explains it.
 *
 *   3. ONE DEFINITION OF `--plain`.  `--plain` is a CLI shortcut, NOT an option: setting an option
 *      named "plain" through the C API is silently accepted and does nothing. The bundle it stands
 *      for must be reproduced by hand, and getting it wrong is not cosmetic — on pigeonhole 4->3 it
 *      is the difference between a 15-step and a 48-step proof, and preprocessing is what introduces
 *      the RAT steps that `SAT.rup_chain` cannot replay. `sc_set_plain` is the single definition.
 *
 * DELIBERATELY DUMB.  This layer captures and hands back. It performs no reasoning: everything at
 * the level of Sylvia `Prop`s stays in F#, and the solver stays out of the trusted base.
 *
 * See README.md for the build. Must be compiled by the SAME g++ that produced `libcadical.a`, and
 * against the SAME CaDiCaL headers: `ProofCapture`'s vtable is generated from `tracer.hpp` at
 * compile time, so a header/archive mismatch dispatches to the wrong slot — memory corruption at
 * solve time rather than a link error.
 */

#ifndef SYLVIA_CADICAL_H_INCLUDED
#define SYLVIA_CADICAL_H_INCLUDED

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32
#define SC_API __declspec (dllexport)
#else
#define SC_API __attribute__ ((visibility ("default")))
#endif

/*------------------------------------------------------------------------*/
/* Status codes. Every entry point returns one of these unless documented  */
/* otherwise; on anything but SC_OK, `sc_last_error` holds the detail.     */
/*------------------------------------------------------------------------*/

#define SC_OK 0
#define SC_ERR_NULL 1      /* a null solver handle was passed                           */
#define SC_ERR_STATE 2     /* call is illegal in the current state (CaDiCaL would abort) */
#define SC_ERR_ARG 3       /* a bad argument: unknown option, zero literal, short buffer */
#define SC_ERR_INTERNAL 4  /* a C++ exception escaped; message in sc_last_error          */

/* Solve outcomes, matching CaDiCaL/IPASIR. */
#define SC_UNKNOWN 0
#define SC_SAT 10
#define SC_UNSAT 20

/* Proof step kinds, as reported by the tracer. */
#define SC_STEP_ORIGINAL 0   /* an input clause, with the id CaDiCaL assigned it   */
#define SC_STEP_DERIVED 1    /* a derived clause: has antecedents, may have witness */
#define SC_STEP_DELETED 2    /* a deletion (bookkeeping; carries no logical content) */
#define SC_STEP_ASSUMPTION 3 /* negation of a failing-assumption core, with antecedents */

/* Conclusion kinds, mirroring CaDiCaL::ConclusionType. */
#define SC_CONCLUDE_NONE 0
#define SC_CONCLUDE_CONFLICT 1
#define SC_CONCLUDE_ASSUMPTIONS 2
#define SC_CONCLUDE_CONSTRAINT 4

/*------------------------------------------------------------------------*/
/* Lifecycle                                                              */
/*------------------------------------------------------------------------*/

typedef struct SylviaCadical SylviaCadical;

/* The CaDiCaL version string, e.g. "3.0.1". Never null; not owned by the caller. */
SC_API const char *sc_signature (void);

/* Create a solver. Returns null only on allocation failure. Starts in the CONFIGURING state, where
 * options may be set and a proof tracer connected; the first `sc_add_clause` ends configuration. */
SC_API SylviaCadical *sc_create (void);

/* Destroy a solver and everything it owns, including any captured proof. Null is a no-op. After
 * this the handle and every array previously written by `sc_proof_export` are the caller's own
 * copies — no pointer into the solver survives. */
SC_API void sc_destroy (SylviaCadical *);

/* The last error message, or "" if the last call succeeded. Owned by the solver; valid until the
 * next call on the same handle. Returns a static string if the handle is null. */
SC_API const char *sc_last_error (SylviaCadical *);

/*------------------------------------------------------------------------*/
/* Configuration — legal only before the first clause is added            */
/*------------------------------------------------------------------------*/

/* Set a CaDiCaL option by name. SC_ERR_ARG if the option does not exist, so a typo is reported
 * rather than ignored (unlike `ccadical_set_option`, which silently accepts "plain"). */
SC_API int sc_set_option (SylviaCadical *, const char *name, int value);

/* Read a CaDiCaL option. Writes through `value`. SC_ERR_ARG if unknown. Legal at any time. */
SC_API int sc_get_option (SylviaCadical *, const char *name, int *value);

/* Disable all pre- and inprocessing: the exact bundle `--plain` stands for.
 *
 * This is about the PROOF, not the verdict. Preprocessing introduces fresh variables and justifies
 * their defining clauses with RAT steps, which are satisfiability-preserving rather than entailed
 * and so have no reading as resolution — `SAT.rup_chain` is RUP-only by design and cannot replay
 * them. Verified on cadical 3.0.1: with this bundle the captured proof is byte-for-byte identical
 * to `cadical --plain --lrat --no-binary` on the same instance.
 *
 * The list is the UNION of what `--plain` sets on 3.0.0 and 3.0.1 (3.0.1 made `factor` and
 * `preprocesslight` default to off, so they dropped out of its diff). Setting an already-default
 * option is harmless, which keeps this stable across both. */
SC_API int sc_set_plain (SylviaCadical *);

/* Capture the proof in memory: connect a `ProofCapture` tracer with antecedents enabled.
 *
 * Antecedents are what make the trace REPLAYABLE — each derived clause arrives with the ids that
 * entail it by unit propagation, which is precisely LRAT. Enabling them calls `force_lrat()` inside
 * CaDiCaL, so the `lrat` option need not be set separately.
 *
 * Legal only while configuring. Unlike file tracing this survives incremental use: call
 * `sc_proof_reset` between rounds to get one refutation per solve. */
SC_API int sc_capture_proof (SylviaCadical *);

/* Abort the search after `ms` milliseconds of wall clock (0 = no limit), via a `Terminator`.
 *
 * This replaces the CLI wrapper's kill-the-process timeout: the solver stops cooperatively and the
 * handle stays usable, so a timeout is an outcome rather than a lost process. `sc_solve` then
 * returns SC_UNKNOWN. May be called at any time and takes effect on the next solve. */
SC_API int sc_set_timeout_ms (SylviaCadical *, int64_t ms);

/*------------------------------------------------------------------------*/
/* Problem input                                                          */
/*------------------------------------------------------------------------*/

/* Add one clause: `n` literals, no terminating zero, no literal equal to zero. Ends configuration.
 *
 * Takes a whole clause rather than `ccadical_add`'s literal-at-a-time protocol so a half-added
 * clause is not representable. */
SC_API int sc_add_clause (SylviaCadical *, const int32_t *lits, size_t n);

/* Declare variables up front. Optional on 3.0.1, where `factor` defaults off; required on 3.0.0,
 * where omitting it aborted the process. Cheap insurance, and it presizes CaDiCaL's tables. */
SC_API int sc_declare_vars (SylviaCadical *, int32_t n);

/* Assume a literal for the NEXT solve only; assumptions are cleared by each solve.
 *
 * With one selector literal per hypothesis this turns `sc_failed` into an unsat core over
 * hypotheses — the minimized set the refutation actually used. Shrinking the obligation before
 * kernel replay is worth more than solver speed here, since replay is where the seconds go. */
SC_API int sc_assume (SylviaCadical *, int32_t lit);

/*------------------------------------------------------------------------*/
/* Solving                                                                */
/*------------------------------------------------------------------------*/

/* Solve under the current assumptions. Writes SC_SAT / SC_UNSAT / SC_UNKNOWN through `status`. */
SC_API int sc_solve (SylviaCadical *, int *status);

/* The value of `lit` in the model of the last SC_SAT solve: writes +lit or -lit through `value`.
 * SC_ERR_STATE unless the last solve was satisfiable. */
SC_API int sc_val (SylviaCadical *, int32_t lit, int32_t *value);

/* Whether `lit` — which must have been assumed — is in the failed core of the last SC_UNSAT solve.
 * Writes 0/1 through `failed`. SC_ERR_STATE unless the last solve was unsatisfiable. */
SC_API int sc_failed (SylviaCadical *, int32_t lit, int32_t *failed);

/* Number of variables CaDiCaL knows about. */
SC_API int sc_vars (SylviaCadical *, int32_t *vars);

/*------------------------------------------------------------------------*/
/* Proof readback                                                         */
/*------------------------------------------------------------------------*/

/* Buffer sizes for `sc_proof_export`. Each writes through its out-parameter; all return 0 when no
 * proof was captured, which is not an error. */
SC_API int sc_proof_num_steps (SylviaCadical *, int64_t *steps);
SC_API int sc_proof_num_lits (SylviaCadical *, int64_t *lits);
SC_API int sc_proof_num_antes (SylviaCadical *, int64_t *antes);

/* The id of the first DERIVED clause, as announced by `begin_proof`. Ids below it are input
 * clauses. Writes through `id`; 0 if the proof never began. */
SC_API int sc_proof_first_derived_id (SylviaCadical *, int64_t *id);

/* How the last solve concluded (SC_CONCLUDE_*), and the clause ids involved: the empty clause for
 * CONFLICT, the failing assumption/constraint clauses otherwise. `ids` may be null to query the
 * count alone. Writes the count through `n_ids`. */
SC_API int sc_conclusion (SylviaCadical *, int *kind, int64_t *ids, size_t *n_ids);

/* Export the captured proof into caller-allocated arrays, in the order the events occurred.
 *
 * Sizes come from the three `sc_proof_num_*` calls. Per-step arrays hold `steps` entries; the two
 * offset arrays hold `steps + 1` (CSR layout, so step i owns the half-open range
 * `[off[i], off[i+1])`):
 *
 *   kinds[i]     SC_STEP_*
 *   ids[i]       the clause id CaDiCaL assigned
 *   redundant[i] 0/1
 *   witness[i]   nonzero => a RAT clause, and the value is the witness literal
 *   lits[lit_off[i] .. lit_off[i+1])     the clause's literals; empty range = the empty clause
 *   antes[ante_off[i] .. ante_off[i+1])  antecedent ids (SC_STEP_DERIVED / SC_STEP_ASSUMPTION)
 *
 * Any array except the two offset arrays may be null to skip that column. The three capacities are
 * the caller's allocated element counts — `steps_cap` covers the per-step arrays (the offset arrays
 * must hold `steps_cap + 1`). SC_ERR_ARG if any is too small, so a size read before a later solve
 * is caught rather than scribbling past the end of a managed buffer.
 *
 * `kinds`/`ids`/`lits`/`antes` are exactly the `Add(id, literals, hints)` triple that `parse_lrat`
 * recovers from text today, so this feeds `SAT.reconstruction_plan` with no file and no parser.
 * `witness` also replaces inferring RAT from a negative hint, and SC_STEP_ORIGINAL supplies
 * CaDiCaL's real input-clause ids in place of assuming they are 1..m in DIMACS order. */
SC_API int sc_proof_export (SylviaCadical *, int32_t *kinds, int64_t *ids, int32_t *redundant,
                            int32_t *witness, int32_t *lits, int64_t *lit_off, int64_t *antes,
                            int64_t *ante_off, int64_t steps_cap, int64_t lits_cap,
                            int64_t antes_cap);

/* Discard the captured proof, keeping the tracer connected and the solver's learned clauses.
 *
 * This is what makes incremental reconstruction tractable: reset between rounds and each solve
 * yields its own self-contained refutation, instead of one merged proof with no way to attribute
 * steps to the round that produced them. */
SC_API int sc_proof_reset (SylviaCadical *);

#ifdef __cplusplus
}
#endif

#endif
