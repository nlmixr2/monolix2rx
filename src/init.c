#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

void R_init_nonmem2rx(DllInfo *info) {
  R_CallMethodDef callMethods[]  = {
    {"_nonmem2rx_trans_", (DL_FUNC) &_monolix2rx_trans_longEq, 1},
    {NULL, NULL, 0}
  };
  // log likelihoods used in calculations
  static const R_CMethodDef cMethods[] = {
    {NULL, NULL, 0, NULL}
  };
  R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  nonmem2rx_full_ini();
}

void R_unload_nonmem2rx(DllInfo *info) {
  /* nonmem2rx_full_parseFree(1); */
}
