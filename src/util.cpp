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

extern "C" SEXP monolix2rxSetMax(const char *var) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function setMax(".setMax", monolix2rxNs);
  setMax(var);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxSetMin(const char *var) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function setMin(".setMin", monolix2rxNs);
  setMin(var);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxSetIov(const char *var) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function setIov(".setIov", monolix2rxNs);
  setIov(var);
  return R_NilValue;
  END_RCPP
}


extern "C" SEXP monolix2rxAddCov(const char *var) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function addCov(".addCov", monolix2rxNs);
  addCov(var);
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

extern "C" SEXP monolix2rxAddCoefSingle(const char *var) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function addCoefSingle(".addCoefSingle", monolix2rxNs);
  addCoefSingle(var);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxAddCoefMult(const char *var) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function addCoefMult(".addCoefMult", monolix2rxNs);
  addCoefMult(var);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxAddCor(const char *v1, const char *v2, const char *v3) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function addCor(".addCor", monolix2rxNs);
  addCor(v1, v2, v3);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxSetCorLevel(const char *v) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function setCorLevel(".setCorLevel", monolix2rxNs);
  setCorLevel(v);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxParameterMethod(const char *v) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function parameterMethod(".parameterMethod", monolix2rxNs);
  parameterMethod(v);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxParameterValue(const char *v) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function parameterValue(".parameterValue", monolix2rxNs);
  parameterValue(v);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxParameterName(const char *v) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function parameterName(".parameterName", monolix2rxNs);
  parameterName(v);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxInputAdd(const char *v) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function inputAdd(".indAdd", monolix2rxNs);
  inputAdd(v);
  return R_NilValue;
  END_RCPP
}

//
extern "C" SEXP monolix2rxInputCatItem(const char *v) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function inputCatItem(".indCatItem", monolix2rxNs);
  inputCatItem(v);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxInputCat(const char *v) {
  BEGIN_RCPP
  Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function inputCat(".indCat", monolix2rxNs);
  inputCat(v);
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP monolix2rxIndReg(const char *v) {
  BEGIN_RCPP
    Environment monolix2rxNs = loadNamespace("monolix2rx");
  Function indReg(".indReg", monolix2rxNs);
  indReg(v);
  return R_NilValue;
  END_RCPP
}
