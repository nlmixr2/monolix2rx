SEXP _monolix2rx_trans_longEq(SEXP in);
SEXP _monolix2rx_trans_indDef(SEXP in);
SEXP _monolix2rx_trans_parameter(SEXP in);
SEXP _monolix2rx_trans_individual(SEXP in, SEXP what);
void nonmem2rx_full_ini(void);

SEXP monolix2rxSetDistTypicalEst(const char *var, int isMean);
SEXP monolix2rxSetDistTypicalFixed(const char *var, int isMean);
SEXP monolix2rxPushCoefList(void);
SEXP monolix2rxAddCor(const char *v1, const char *v2, const char *v3);
#define monolix2rxAddVar(v) monolix2rxSingle(v, ".addVar")
#define monolix2rxSetDist(v) monolix2rxSingle(v, ".setDist")
#define monolix2rxSetVar(v) monolix2rxSingle(v, ".setVar")
#define monolix2rxSetSd(v) monolix2rxSingle(v, ".setSd")
#define monolix2rxSetMax(v) monolix2rxSingle(v, ".setMax")
#define monolix2rxSetMin(v) monolix2rxSingle(v, ".setMin")
#define monolix2rxSetIov(v) monolix2rxSingle(v, ".setIov")
#define monolix2rxAddCov(v) monolix2rxSingle(v, ".addCov")
#define monolix2rxAddCoefSingle(v) monolix2rxSingle(v, ".addCoefSingle")
#define monolix2rxAddCoefMult(v) monolix2rxSingle(v, ".addCoefMult")
#define monolix2rxSetCorLevel(v) monolix2rxSingle(v, ".setCorLevel")
#define monolix2rxParameterMethod(v) monolix2rxSingle(v, ".parameterMethod")
#define monolix2rxParameterValue(v)  monolix2rxSingle(v, ".parameterValue")
#define monolix2rxParameterName(v) monolix2rxSingle(v, ".parameterName")
#define monolix2rxInputAdd(v) monolix2rxSingle(v, ".indAdd")
#define monolix2rxInputCatItem(v) monolix2rxSingle(v, ".indCatItem")
#define monolix2rxInputCat(v) monolix2rxSingle(v, ".indCat")
#define monolix2rxIndReg(v) monolix2rxSingle(v, ".indReg")
