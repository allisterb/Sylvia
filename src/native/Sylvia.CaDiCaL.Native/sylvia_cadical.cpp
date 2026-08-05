/* Sylvia native interop layer for CaDiCaL 3.0.1 — implementation. See sylvia_cadical.h. */

#include "sylvia_cadical.h"

#include "cadical.hpp"
#include "tracer.hpp"

#include <chrono>
#include <cstring>
#include <exception>
#include <string>
#include <vector>

namespace {

/*------------------------------------------------------------------------*/
/* Proof capture                                                          */
/*------------------------------------------------------------------------*/

/* Every traced event, flattened. Literals and antecedents live in two shared arenas and each step
 * records its half-open range into them — the CSR layout `sc_proof_export` hands to .NET directly,
 * so readback is a memcpy rather than a walk over per-step allocations. */
struct ProofBuffer {
  std::vector<int32_t> kinds;
  std::vector<int64_t> ids;
  std::vector<int32_t> redundant;
  std::vector<int32_t> witness;
  std::vector<int32_t> lits;
  std::vector<int64_t> lit_off;   /* steps + 1 entries */
  std::vector<int64_t> antes;
  std::vector<int64_t> ante_off;  /* steps + 1 entries */

  int64_t first_derived_id = 0;

  int conclusion_kind = SC_CONCLUDE_NONE;
  std::vector<int64_t> conclusion_ids;

  ProofBuffer () { clear (); }

  void clear () {
    kinds.clear ();
    ids.clear ();
    redundant.clear ();
    witness.clear ();
    lits.clear ();
    antes.clear ();
    lit_off.assign (1, 0);
    ante_off.assign (1, 0);
    first_derived_id = 0;
    conclusion_kind = SC_CONCLUDE_NONE;
    conclusion_ids.clear ();
  }

  void push (int32_t kind, int64_t id, bool red, int wit, const std::vector<int> &clause,
             const std::vector<int64_t> *antecedents) {
    kinds.push_back (kind);
    ids.push_back (id);
    redundant.push_back (red ? 1 : 0);
    witness.push_back (wit);
    for (size_t i = 0; i != clause.size (); i++)
      lits.push_back ((int32_t) clause[i]);
    lit_off.push_back ((int64_t) lits.size ());
    if (antecedents)
      for (size_t i = 0; i != antecedents->size (); i++)
        antes.push_back ((*antecedents)[i]);
    ante_off.push_back ((int64_t) antes.size ());
  }

  size_t steps () const { return kinds.size (); }
};

/* The C++ side of the boundary: a `Tracer` that writes into a `ProofBuffer` and never calls back
 * into managed code. Only the events the reconstruction consumes are overridden; the rest keep
 * `Tracer`'s no-op defaults.
 *
 * These signatures must match CaDiCaL 3.0.1's `tracer.hpp` exactly. `override` is deliberate — it
 * turns a header/version mismatch into a compile error instead of a silently wrong vtable slot.
 * Note `add_derived_clause` takes the witness as its THIRD parameter, before the clause, which the
 * header's own comment lists in a different order. */
class ProofCapture : public CaDiCaL::Tracer {
  ProofBuffer &buf;

public:
  explicit ProofCapture (ProofBuffer &b) : buf (b) {}

  void add_original_clause (int64_t id, bool redundant, const std::vector<int> &clause,
                            bool /*restored*/) override {
    buf.push (SC_STEP_ORIGINAL, id, redundant, 0, clause, 0);
  }

  void add_derived_clause (int64_t id, bool redundant, int witness,
                           const std::vector<int> &clause,
                           const std::vector<int64_t> &antecedents) override {
    buf.push (SC_STEP_DERIVED, id, redundant, witness, clause, &antecedents);
  }

  void delete_clause (int64_t id, bool redundant, const std::vector<int> &clause) override {
    buf.push (SC_STEP_DELETED, id, redundant, 0, clause, 0);
  }

  void add_assumption_clause (int64_t id, const std::vector<int> &clause,
                              const std::vector<int64_t> &antecedents) override {
    buf.push (SC_STEP_ASSUMPTION, id, false, 0, clause, &antecedents);
  }

  void begin_proof (int64_t id) override { buf.first_derived_id = id; }

  void conclude_unsat (CaDiCaL::ConclusionType type,
                       const std::vector<int64_t> &clause_ids) override {
    switch (type) {
    case CaDiCaL::CONFLICT: buf.conclusion_kind = SC_CONCLUDE_CONFLICT; break;
    case CaDiCaL::ASSUMPTIONS: buf.conclusion_kind = SC_CONCLUDE_ASSUMPTIONS; break;
    case CaDiCaL::CONSTRAINT: buf.conclusion_kind = SC_CONCLUDE_CONSTRAINT; break;
    default: buf.conclusion_kind = SC_CONCLUDE_NONE; break;
    }
    buf.conclusion_ids = clause_ids;
  }
};

/*------------------------------------------------------------------------*/
/* Cooperative timeout                                                    */
/*------------------------------------------------------------------------*/

/* Wall-clock deadline. The CLI wrapper enforced its budget by killing the process; here the solver
 * stops on its own and the handle stays usable, so a timeout is an outcome rather than a loss. */
class DeadlineTerminator : public CaDiCaL::Terminator {
  std::chrono::steady_clock::time_point deadline;
  bool armed = false;

public:
  void arm (int64_t ms) {
    if (ms > 0) {
      deadline = std::chrono::steady_clock::now () + std::chrono::milliseconds (ms);
      armed = true;
    } else
      armed = false;
  }
  void disarm () { armed = false; }
  bool terminate () override {
    return armed && std::chrono::steady_clock::now () >= deadline;
  }
};

/*------------------------------------------------------------------------*/
/* Solver handle                                                          */
/*------------------------------------------------------------------------*/

/* CaDiCaL's own state machine is enforced with `REQUIRE(...)`, which aborts the process. We shadow
 * the one distinction that matters — whether configuration is still open — and refuse the call
 * instead. Configuration closes on the first clause or variable declaration, matching CaDiCaL's
 * CONFIGURING -> ADDING transition. */
enum Phase { PHASE_CONFIGURING, PHASE_OPEN };

} // namespace

struct SylviaCadical {
  CaDiCaL::Solver solver;
  ProofBuffer buffer;
  ProofCapture *tracer = 0;
  DeadlineTerminator terminator;
  Phase phase = PHASE_CONFIGURING;
  int last_status = SC_UNKNOWN;
  std::string error;

  SylviaCadical () { solver.connect_terminator (&terminator); }

  ~SylviaCadical () {
    if (tracer) {
      solver.disconnect_proof_tracer (tracer);
      delete tracer;
      tracer = 0;
    }
    solver.disconnect_terminator ();
  }
};

namespace {

int fail (SylviaCadical *s, int code, const char *msg) {
  s->error = msg;
  return code;
}

int ok (SylviaCadical *s) {
  s->error.clear ();
  return SC_OK;
}

/* Guard the configuration window. */
int require_configuring (SylviaCadical *s, const char *what) {
  if (s->phase != PHASE_CONFIGURING) {
    s->error = std::string (what) + ": only legal before the first clause is added";
    return SC_ERR_STATE;
  }
  return SC_OK;
}

} // namespace

/* Every entry point funnels through this: a C++ exception must never cross into .NET, where it is
 * undefined behaviour rather than a catchable error.
 *
 * Variadic because the body is one macro argument and the preprocessor only protects commas inside
 * PARENTHESES — braces do not count, so a brace-enclosed initialiser in the body would otherwise be
 * split into several arguments. */
#define SC_GUARD(handle, ...)                                                                     \
  do {                                                                                            \
    if (!(handle))                                                                                \
      return SC_ERR_NULL;                                                                         \
    try {                                                                                         \
      __VA_ARGS__                                                                                 \
    } catch (const std::exception &e) {                                                           \
      return fail (handle, SC_ERR_INTERNAL, e.what ());                                           \
    } catch (...) {                                                                               \
      return fail (handle, SC_ERR_INTERNAL, "unknown C++ exception");                             \
    }                                                                                             \
  } while (0)

extern "C" {

/*------------------------------------------------------------------------*/
/* Lifecycle                                                              */
/*------------------------------------------------------------------------*/

const char *sc_signature (void) { return CaDiCaL::Solver::signature (); }

SylviaCadical *sc_create (void) {
  try {
    return new SylviaCadical ();
  } catch (...) {
    return 0;
  }
}

void sc_destroy (SylviaCadical *s) { delete s; }

const char *sc_last_error (SylviaCadical *s) {
  return s ? s->error.c_str () : "null solver handle";
}

/*------------------------------------------------------------------------*/
/* Configuration                                                          */
/*------------------------------------------------------------------------*/

int sc_set_option (SylviaCadical *s, const char *name, int value) {
  SC_GUARD (s, {
    if (!name)
      return fail (s, SC_ERR_ARG, "sc_set_option: null option name");
    /* CaDiCaL exempts the four logging options from the CONFIGURING requirement, so mirror that
     * rather than being gratuitously stricter than the solver. */
    bool anytime = !std::strcmp (name, "log") || !std::strcmp (name, "quiet") ||
                   !std::strcmp (name, "report") || !std::strcmp (name, "verbose");
    if (!anytime) {
      int rc = require_configuring (s, "sc_set_option");
      if (rc != SC_OK)
        return rc;
    }
    /* `Solver::set` returns false for an unknown option. Report it: the stock C API swallows this,
     * which is how `set_option("plain", 1)` reads as success while doing nothing. */
    if (!s->solver.set (name, value))
      return fail (s, SC_ERR_ARG, (std::string ("sc_set_option: no option '") + name + "'").c_str ());
    return ok (s);
  });
}

int sc_get_option (SylviaCadical *s, const char *name, int *value) {
  SC_GUARD (s, {
    if (!name || !value)
      return fail (s, SC_ERR_ARG, "sc_get_option: null argument");
    /* `Solver::get` reports an unknown option as 0, so check membership first. */
    if (!CaDiCaL::Solver::is_valid_option (name))
      return fail (s, SC_ERR_ARG, (std::string ("sc_get_option: no option '") + name + "'").c_str ());
    *value = s->solver.get (name);
    return ok (s);
  });
}

int sc_set_plain (SylviaCadical *s) {
  SC_GUARD (s, {
    int rc = require_configuring (s, "sc_set_plain");
    if (rc != SC_OK)
      return rc;
    /* The union of the `--plain` bundle across 3.0.0 and 3.0.1 (see header). */
    static const char *const bundle[] = {
        "compact",  "congruence", "decompose",       "deduplicate", "eagersubsume", "elim",
        "factor",   "factorunbump", "fastelim",      "inprobing",   "inprocessing",
        "preprocesslight", "probe", "subsume",       "sweep",       "ternary",
        "transred", "vivify"};
    for (size_t i = 0; i != sizeof (bundle) / sizeof (bundle[0]); i++)
      if (!s->solver.set (bundle[i], 0))
        return fail (s, SC_ERR_ARG,
                     (std::string ("sc_set_plain: this CaDiCaL has no option '") + bundle[i] +
                      "' — the bundle needs re-deriving from `cadical --plain -v`")
                         .c_str ());
    return ok (s);
  });
}

int sc_capture_proof (SylviaCadical *s) {
  SC_GUARD (s, {
    int rc = require_configuring (s, "sc_capture_proof");
    if (rc != SC_OK)
      return rc;
    if (s->tracer)
      return fail (s, SC_ERR_STATE, "sc_capture_proof: a proof tracer is already connected");
    s->tracer = new ProofCapture (s->buffer);
    /* antecedents = true; this calls force_lrat() inside CaDiCaL, so the `lrat` option need not
     * be set separately. Without it the clauses arrive with no hints and cannot be replayed. */
    s->solver.connect_proof_tracer (s->tracer, true);
    return ok (s);
  });
}

int sc_set_timeout_ms (SylviaCadical *s, int64_t ms) {
  SC_GUARD (s, {
    s->terminator.arm (ms);
    return ok (s);
  });
}

/*------------------------------------------------------------------------*/
/* Problem input                                                          */
/*------------------------------------------------------------------------*/

int sc_add_clause (SylviaCadical *s, const int32_t *lits, size_t n) {
  SC_GUARD (s, {
    if (n && !lits)
      return fail (s, SC_ERR_ARG, "sc_add_clause: null literal array");
    /* Validate the whole clause before adding any of it: CaDiCaL aborts on a zero literal, and a
     * partially added clause cannot be taken back. */
    for (size_t i = 0; i != n; i++)
      if (lits[i] == 0)
        return fail (s, SC_ERR_ARG, "sc_add_clause: zero is not a literal");
    s->phase = PHASE_OPEN;
    for (size_t i = 0; i != n; i++)
      s->solver.add ((int) lits[i]);
    s->solver.add (0);
    return ok (s);
  });
}

int sc_declare_vars (SylviaCadical *s, int32_t n) {
  SC_GUARD (s, {
    if (n < 0)
      return fail (s, SC_ERR_ARG, "sc_declare_vars: negative count");
    int have = s->solver.vars ();
    if (n > have) {
      s->phase = PHASE_OPEN;
      s->solver.declare_more_variables (n - have);
    }
    return ok (s);
  });
}

int sc_assume (SylviaCadical *s, int32_t lit) {
  SC_GUARD (s, {
    if (lit == 0)
      return fail (s, SC_ERR_ARG, "sc_assume: zero is not a literal");
    s->phase = PHASE_OPEN;
    s->solver.assume ((int) lit);
    return ok (s);
  });
}

/*------------------------------------------------------------------------*/
/* Solving                                                                */
/*------------------------------------------------------------------------*/

int sc_solve (SylviaCadical *s, int *status) {
  SC_GUARD (s, {
    if (!status)
      return fail (s, SC_ERR_ARG, "sc_solve: null status");
    s->phase = PHASE_OPEN;
    s->last_status = s->solver.solve ();
    /* `conclude` is what drives the tracer's conclude_unsat callback. CaDiCaL also permits it after
     * an UNKNOWN solve, but there it appends the currently entailed literals to the proof — noise
     * in the buffer for a search that never finished — so it is skipped on a timeout. */
    if (s->last_status == SC_SAT || s->last_status == SC_UNSAT)
      s->solver.conclude ();
    *status = s->last_status;
    return ok (s);
  });
}

int sc_val (SylviaCadical *s, int32_t lit, int32_t *value) {
  SC_GUARD (s, {
    if (!value)
      return fail (s, SC_ERR_ARG, "sc_val: null value");
    if (s->last_status != SC_SAT)
      return fail (s, SC_ERR_STATE, "sc_val: the last solve was not satisfiable");
    if (lit == 0)
      return fail (s, SC_ERR_ARG, "sc_val: zero is not a literal");
    *value = (int32_t) s->solver.val ((int) lit);
    return ok (s);
  });
}

int sc_failed (SylviaCadical *s, int32_t lit, int32_t *failed) {
  SC_GUARD (s, {
    if (!failed)
      return fail (s, SC_ERR_ARG, "sc_failed: null out-parameter");
    if (s->last_status != SC_UNSAT)
      return fail (s, SC_ERR_STATE, "sc_failed: the last solve was not unsatisfiable");
    if (lit == 0)
      return fail (s, SC_ERR_ARG, "sc_failed: zero is not a literal");
    *failed = s->solver.failed ((int) lit) ? 1 : 0;
    return ok (s);
  });
}

int sc_vars (SylviaCadical *s, int32_t *vars) {
  SC_GUARD (s, {
    if (!vars)
      return fail (s, SC_ERR_ARG, "sc_vars: null out-parameter");
    *vars = (int32_t) s->solver.vars ();
    return ok (s);
  });
}

/*------------------------------------------------------------------------*/
/* Proof readback                                                         */
/*------------------------------------------------------------------------*/

int sc_proof_num_steps (SylviaCadical *s, int64_t *steps) {
  SC_GUARD (s, {
    if (!steps)
      return fail (s, SC_ERR_ARG, "sc_proof_num_steps: null out-parameter");
    *steps = (int64_t) s->buffer.steps ();
    return ok (s);
  });
}

int sc_proof_num_lits (SylviaCadical *s, int64_t *lits) {
  SC_GUARD (s, {
    if (!lits)
      return fail (s, SC_ERR_ARG, "sc_proof_num_lits: null out-parameter");
    *lits = (int64_t) s->buffer.lits.size ();
    return ok (s);
  });
}

int sc_proof_num_antes (SylviaCadical *s, int64_t *antes) {
  SC_GUARD (s, {
    if (!antes)
      return fail (s, SC_ERR_ARG, "sc_proof_num_antes: null out-parameter");
    *antes = (int64_t) s->buffer.antes.size ();
    return ok (s);
  });
}

int sc_proof_first_derived_id (SylviaCadical *s, int64_t *id) {
  SC_GUARD (s, {
    if (!id)
      return fail (s, SC_ERR_ARG, "sc_proof_first_derived_id: null out-parameter");
    *id = s->buffer.first_derived_id;
    return ok (s);
  });
}

int sc_conclusion (SylviaCadical *s, int *kind, int64_t *ids, size_t *n_ids) {
  SC_GUARD (s, {
    if (!kind || !n_ids)
      return fail (s, SC_ERR_ARG, "sc_conclusion: null out-parameter");
    const std::vector<int64_t> &src = s->buffer.conclusion_ids;
    if (ids && *n_ids < src.size ())
      return fail (s, SC_ERR_ARG, "sc_conclusion: id buffer too small");
    *kind = s->buffer.conclusion_kind;
    if (ids)
      for (size_t i = 0; i != src.size (); i++)
        ids[i] = src[i];
    *n_ids = src.size ();
    return ok (s);
  });
}

int sc_proof_export (SylviaCadical *s, int32_t *kinds, int64_t *ids, int32_t *redundant,
                     int32_t *witness, int32_t *lits, int64_t *lit_off, int64_t *antes,
                     int64_t *ante_off, int64_t steps_cap, int64_t lits_cap, int64_t antes_cap) {
  SC_GUARD (s, {
    const ProofBuffer &b = s->buffer;
    if (!lit_off || !ante_off)
      return fail (s, SC_ERR_ARG, "sc_proof_export: the offset arrays are required");

    const size_t n = b.steps ();
    /* Sizes are read before the call, so a solve in between would silently grow the buffer. Refuse
     * rather than overrun a managed array. */
    if (steps_cap < (int64_t) n)
      return fail (s, SC_ERR_ARG, "sc_proof_export: step buffers too small");
    if (lits && lits_cap < (int64_t) b.lits.size ())
      return fail (s, SC_ERR_ARG, "sc_proof_export: literal buffer too small");
    if (antes && antes_cap < (int64_t) b.antes.size ())
      return fail (s, SC_ERR_ARG, "sc_proof_export: antecedent buffer too small");

    /* `data()` on an empty vector may be null, and memcpy with a null source is undefined even for
     * a zero count — so every copy is guarded on non-emptiness. The offset arrays always hold at
     * least the leading 0, so they need no guard. */
    if (n) {
      if (kinds)
        std::memcpy (kinds, b.kinds.data (), n * sizeof (int32_t));
      if (ids)
        std::memcpy (ids, b.ids.data (), n * sizeof (int64_t));
      if (redundant)
        std::memcpy (redundant, b.redundant.data (), n * sizeof (int32_t));
      if (witness)
        std::memcpy (witness, b.witness.data (), n * sizeof (int32_t));
    }
    if (lits && !b.lits.empty ())
      std::memcpy (lits, b.lits.data (), b.lits.size () * sizeof (int32_t));
    if (antes && !b.antes.empty ())
      std::memcpy (antes, b.antes.data (), b.antes.size () * sizeof (int64_t));
    std::memcpy (lit_off, b.lit_off.data (), (n + 1) * sizeof (int64_t));
    std::memcpy (ante_off, b.ante_off.data (), (n + 1) * sizeof (int64_t));
    return ok (s);
  });
}

int sc_proof_reset (SylviaCadical *s) {
  SC_GUARD (s, {
    s->buffer.clear ();
    return ok (s);
  });
}

} /* extern "C" */
