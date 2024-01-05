#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <Rcpp.h>
#include <R.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("monolix2rx", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

using namespace Rcpp;
Function loadNamespace("loadNamespace", R_BaseNamespace);

extern "C" SEXP monolix2rxSingle(const char *var, const char *fn) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function f(fn, monolix2rxNs);
  f(var);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxDouble(const char *var, const char *var2, const char *fn) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function f(fn, monolix2rxNs);
  f(var, var2);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxTriple(const char *v1, const char *v2, const char *v3, const char *fn) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function f(fn, monolix2rxNs);
  f(v1, v2, v3);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxSetDistTypicalEst(const char *var, int isMean) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function setTypicalEst(".setTypicalEst", monolix2rxNs);
  setTypicalEst(var, isMean);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxDoubleI(const char *var, int isMean, const char *fn) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function fun(fn, monolix2rxNs);
  fun(var, isMean);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxPushCoefList(void) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function pushCoefList(".pushCoefList", monolix2rxNs);
  pushCoefList();
  return R_NilValue;
  END_RCPP
}
