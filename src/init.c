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

SEXP _monolix2rxlixoftConnectors(void);
SEXP _monolix2rx_iniDparserPtr(SEXP);
SEXP _monolix2rxInitializeLixoftConnectors(SEXP software, SEXP force);
SEXP _monolix2rxGetLibraryModelContent(SEXP filename);

void R_init_monolix2rx(DllInfo *info) {
  R_CallMethodDef callMethods[]  = {
    {"_monolix2rxGetLibraryModelContent", (DL_FUNC) &_monolix2rxGetLibraryModelContent, 1},
    {"_monolix2rxInitializeLixoftConnectors", (DL_FUNC) &_monolix2rxInitializeLixoftConnectors, 2},
    {"_monolix2rxlixoftConnectors", (DL_FUNC) &_monolix2rxlixoftConnectors, 0},
    {"_monolix2rx_iniDparserPtr", (DL_FUNC) &_monolix2rx_iniDparserPtr, 1},
    {"_monolix2rx_r_parseIni", (DL_FUNC) &_monolix2rx_r_parseIni, 0},
    {"_monolix2rx_r_parseFree", (DL_FUNC) &_monolix2rx_r_parseFree, 0},
    {"_monolix2rx_trans_data_settings", (DL_FUNC) &_monolix2rx_trans_data_settings, 1},
    {"_monolix2rx_trans_summaryData", (DL_FUNC) &_monolix2rx_trans_summaryData, 1},
    {"_monolix2rx_trans_equation", (DL_FUNC) &_monolix2rx_trans_equation, 2},
    {"_monolix2rx_trans_mlxtrantask", (DL_FUNC) &_monolix2rx_trans_mlxtrantask, 1},
    {"_monolix2rx_trans_longoutput", (DL_FUNC) &_monolix2rx_trans_longoutput, 1},
    {"_monolix2rx_trans_mlxtran_op", (DL_FUNC) &_monolix2rx_trans_mlxtran_op, 2},
    {"_monolix2rx_trans_fit", (DL_FUNC) &_monolix2rx_trans_fit, 1},
    {"_monolix2rx_trans_longdef", (DL_FUNC) &_monolix2rx_trans_longdef, 2},
    {"_monolix2rx_trans_content", (DL_FUNC) &_monolix2rx_trans_content, 1},
    {"_monolix2rx_trans_fileinfo", (DL_FUNC) &_monolix2rx_trans_fileinfo, 1},
    {"_monolix2rx_trans_individual", (DL_FUNC) &_monolix2rx_trans_individual, 2},
    {"_monolix2rx_trans_indDef", (DL_FUNC) &_monolix2rx_trans_indDef, 1},
    {"_monolix2rx_trans_parameter", (DL_FUNC) &_monolix2rx_trans_parameter, 1},
    {NULL, NULL, 0}
  };
  // log likelihoods used in calculations
  static const R_CMethodDef cMethods[] = {
    {NULL, NULL, 0, NULL}
  };
  R_registerRoutines(info, cMethods, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  monolix2rx_full_ini();
}

void R_unload_monolix2rx(DllInfo *info) {
  /* monolixr2rx_full_parseFree(1); */
}
