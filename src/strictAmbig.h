#include <stdlib.h>

// With MONOLIX2RX_STRICT_AMBIGUITY set (the tests do), parse without dparser's
// greediness/height tie-breaking and error on any ambiguity left, so a grammar
// change cannot quietly bring back the superlinear parse of issue #51.
static const char *monolix2rxAmbigAt = NULL;

// # nocov start: only reached through an ambiguous grammar
static inline struct D_ParseNode *monolix2rxAmbigFn(struct D_Parser *p, int n,
                                                    struct D_ParseNode **v) {
  (void)p;
  (void)n;
  if (monolix2rxAmbigAt == NULL) monolix2rxAmbigAt = v[0]->start_loc.s;
  return v[0];
}
// # nocov end

static inline void monolix2rxStrictAmbig(D_Parser *p) {
  monolix2rxAmbigAt = NULL;
  if (getenv("MONOLIX2RX_STRICT_AMBIGUITY") == NULL) return;
  p->dont_use_greediness_for_disambiguation = 1;
  p->dont_use_height_for_disambiguation = 1;
  p->ambiguity_fn = monolix2rxAmbigFn;
}

// call after parseFree(); the location points into the caller's R string
static inline void monolix2rxCheckAmbig(const char *what) {
  if (monolix2rxAmbigAt != NULL) {
    // # nocov start: only reached through an ambiguous grammar
    const char *at = monolix2rxAmbigAt;
    monolix2rxAmbigAt = NULL;
    Rf_errorcall(R_NilValue, "%s: ambiguous parse at: %.40s", what, at);
    // # nocov end
  }
}
