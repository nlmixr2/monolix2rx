SEXP monolix2rxAddVar(const char *var);
SEXP monolix2rxSetDist(const char *var);
SEXP _monolix2rx_trans_longEq(SEXP in);
SEXP _monolix2rx_trans_indDef(SEXP in);
SEXP monolix2rxSetDistTypicalEst(const char *var, int isMean);
SEXP monolix2rxSetDistTypicalFixed(const char *var, int isMean);
