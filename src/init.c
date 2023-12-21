#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

#include "util.h"

void R_init_monolix2rx(DllInfo *info) {
  R_CallMethodDef callMethods[]  = {
    {"_monolix2rx_trans_longdef", (DL_FUNC) &_monolix2rx_trans_longdef, 1},
    {"_monolix2rx_trans_content", (DL_FUNC) &_monolix2rx_trans_content, 1},
    {"_monolix2rx_trans_fileinfo", (DL_FUNC) &_monolix2rx_trans_fileinfo, 1},
    {"_monolix2rx_trans_individual", (DL_FUNC) &_monolix2rx_trans_individual, 2},
    {"_monolix2rx_trans_indDef", (DL_FUNC) &_monolix2rx_trans_indDef, 1},
    {"_monolix2rx_trans_parameter", (DL_FUNC) &_monolix2rx_trans_parameter, 1},
    {"_monolix2rx_trans_longEq", (DL_FUNC) &_monolix2rx_trans_longEq, 1},
    {NULL, NULL, 0}
  };
  // log likelihoods used in calculations
  static const R_CMethodDef cMethods[] = {
    {NULL, NULL, 0, NULL}
  };
  R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  nonmem2rx_full_ini();
  /* monolixr2rx_full_ini(); */
}

void R_unload_monolix2rx(DllInfo *info) {
  /* monolixr2rx_full_parseFree(1); */
}
