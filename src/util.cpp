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

extern "C" SEXP monolix2rxAddVar(const char *var) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function addVar(".addVar", monolix2rxNs);
  addVar(var);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxSetDist(const char *var) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function setDist(".setDist", monolix2rxNs);
  setDist(var);
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

extern "C" SEXP monolix2rxSetDistTypicalFixed(const char *var, int isMean) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function setTypicalFixed(".setTypicalFixed", monolix2rxNs);
  setTypicalFixed(var, isMean);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxSetVar(const char *var) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function setVar(".setVar", monolix2rxNs);
  setVar(var);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxSetSd(const char *var) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function setSd(".setSd", monolix2rxNs);
  setSd(var);
  return R_NilValue;
  END_RCPP
}
