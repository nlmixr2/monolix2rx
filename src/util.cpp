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
