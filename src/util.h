SEXP _monolix2rx_trans_indDef(SEXP in);
SEXP _monolix2rx_trans_parameter(SEXP in);
SEXP _monolix2rx_trans_individual(SEXP in, SEXP what);
SEXP _monolix2rx_trans_fileinfo(SEXP in);
SEXP _monolix2rx_trans_content(SEXP in);
SEXP _monolix2rx_trans_longdef(SEXP in);
SEXP _monolix2rx_trans_fit(SEXP in);
SEXP _monolix2rx_trans_mlxtran_op(SEXP in, SEXP what);
SEXP _monolix2rx_trans_longoutput(SEXP in);
SEXP _monolix2rx_trans_mlxtrantask(SEXP in);
SEXP _monolix2rx_trans_equation(SEXP in, SEXP what);
SEXP _monolix2rx_trans_summaryData(SEXP in);

void monolix2rx_full_ini(void);

SEXP monolix2rxSetDistTypicalEst(const char *var, int isMean);
SEXP monolix2rxSetDistTypicalFixed(const char *var, int isMean);
SEXP monolix2rxPushCoefList(void);
SEXP monolix2rxSingle(const char *var, const char *fn);
SEXP monolix2rxDouble(const char *var, const char *v2, const char *fn);
SEXP monolix2rxDoubleI(const char *var, int isMean, const char *fn);
SEXP monolix2rxTriple(const char *var, const char *v2, const char *v3, const char *fn);

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
#define monolix2rxSetCorLevel(v)     monolix2rxSingle(v, ".setCorLevel")
#define monolix2rxParameterMethod(v) monolix2rxSingle(v, ".parameterMethod")
#define monolix2rxParameterValue(v)  monolix2rxSingle(v, ".parameterValue")
#define monolix2rxParameterName(v) monolix2rxSingle(v, ".parameterName")
#define monolix2rxInputAdd(v) monolix2rxSingle(v, ".indAdd")
#define monolix2rxInputCatItem(v) monolix2rxSingle(v, ".indCatItem")
#define monolix2rxInputCat(v) monolix2rxSingle(v, ".indCat")
#define monolix2rxIndReg(v) monolix2rxSingle(v, ".indReg")
#define monolix2rxFileinfoFile(v) monolix2rxSingle(v, ".fileinfoFile")
#define monolix2rxFileinfoHeader(v) monolix2rxSingle(v, ".fileinfoHeader")
#define monolix2rxContentSetUse1(v1, v2) monolix2rxDouble(v1, v2,".contSetUse1")
#define monolix2rxContentSetNbdoses(v) monolix2rxSingle(v, ".contentNbdoses")
#define monolix2rxContentContentContCov(v) monolix2rxSingle(v, ".contentContCov")
#define monolix2rxContentContentYname(v) monolix2rxSingle(v, ".contentYname")
#define monolix2rxContentContentName(v) monolix2rxSingle(v, ".contentName")
#define monolix2rxContentContentType(v) monolix2rxSingle(v, ".contentType")
#define monolix2rxLongDefAddEndpoint(v) monolix2rxSingle(v, ".addEndpoint")
#define monolix2rxLongDefAddPrediction(v) monolix2rxSingle(v,".addPrediction")
#define monolix2rxLongDefSetCombined1(v1, v2) monolix2rxDouble(v1, v2,".setCombined1")
#define monolix2rxLongDefSetCombined2(v1, v2) monolix2rxDouble(v1, v2,".setCombined2")
#define monolix2rxLongDefSetCombined1c(v1, v2, v3) monolix2rxTriple(v1, v2, v3,".setCombined1c")
#define monolix2rxLongDefSetCombined2c(v1, v2, v3) monolix2rxTriple(v1, v2, v3, ".setCombined2c")
#define monolix2rxAddCor(v1, v2, v3) monolix2rxTriple(v1, v2, v3, ".addCor")
#define monolix2rxLongDefSetConstant(v1) monolix2rxSingle(v1, ".setConstant")
#define monolix2rxLongDefSetProportional(v1) monolix2rxSingle(v1, ".setProp")
#define monolix2rxLongDefSetEventType(v1) monolix2rxSingle(v1, ".setEventType")
#define monolix2rxLongDefSetMaxEventNumber(v1) monolix2rxSingle(v1, ".setMaxEventNumber")
#define monolix2rxLongDefSetRightCensoringTime(v1) monolix2rxSingle(v1, ".setRightCensoringTime")
#define monolix2rxLongDefSetIntervalLength(v1) monolix2rxSingle(v1, ".setIntervalLength")
#define monolix2rxLongDefSetCategoriesInt(v1) monolix2rxSingle(v1,".setCategoriesInt")
#define monolix2rxLongDefSetCodeLine(v1) monolix2rxSingle(v1, ".setCodeLine")
