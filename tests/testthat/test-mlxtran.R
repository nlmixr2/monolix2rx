test_that("mlxtran initial list", {

  withr::with_dir(tempdir(), {

    modelLines <- c("DESCRIPTION:",
                    "model translated from `babelmixr2` and `nlmixr2` function pk.turnover.emax3 to pk.turnover.emax3-monolix.txt",
                    "",
                    "[LONGITUDINAL]",
                    "input={ktr,ka,cl,v,emax,ec50,kout,e0}",
                    "",
                    "PK:",
                    "; Define compartments with administrations",
                    "compartment(cmt=1, amount=depot)",
                    "; Define PK macros",
                    "depot(type=1, target=depot, Tlag=0, p=1)",
                    "",
                    "EQUATION:",
                    "DCP = center/v",
                    "PD = 1-emax*DCP/(ec50+DCP)",
                    "effect_0 = e0",
                    "kin = e0*kout",
                    "ddt_depot = - ktr*depot",
                    "ddt_gut = ktr*depot-ka*gut",
                    "ddt_center = ka*gut-cl/v*center",
                    "ddt_effect = kin*PD-kout*effect",
                    "cp = center/v",
                    "rx_pred_cp = cp",
                    "rx_pred_effect = effect",
                    "",
                    "OUTPUT:",
                    "output={rx_pred_cp, rx_pred_effect}",
                    "")

    writeLines(modelLines, "pk.turnover.emax3-monolix.txt")

    lines <- c("<DATAFILE>",
               "",
               "[FILEINFO]",
               "file='pk.turnover.emax3-monolix.csv'",
               "delimiter = comma",
               "header = {ID, TIME, EVID, AMT, DV, ADM, YTYPE, nlmixrRowNums}",
               "",
               "[CONTENT]",
               "ID = {use=identifier}",
               "TIME = {use=time}",
               "EVID = {use=eventidentifier}",
               "AMT = {use=amount}",
               "DV = {use=observation, name={y1, y2}, yname={'1', '2'}, type={continuous, continuous}}",
               "ADM = {use=administration}", "YTYPE = {use=observationtype}",
               "",
               "<MODEL>",
               "",
               "[INDIVIDUAL]",
               "input={ktr_pop, omega_ktr, ka_pop, omega_ka, cl_pop, omega_cl, v_pop, omega_v, emax_pop, omega_emax, ec50_pop, omega_ec50, kout_pop, omega_kout, e0_pop, omega_e0}",
               "",
               "DEFINITION:",
               "ktr = {distribution=logNormal, typical=ktr_pop, sd=omega_ktr}",
               "ka = {distribution=logNormal, typical=ka_pop, sd=omega_ka}",
               "cl = {distribution=logNormal, typical=cl_pop, sd=omega_cl}",
               "v = {distribution=logNormal, typical=v_pop, sd=omega_v}",
               "emax = {distribution=logitNormal, min=0, max=1, typical=emax_pop, sd=omega_emax}",
               "ec50 = {distribution=logNormal, typical=ec50_pop, sd=omega_ec50}",
               "kout = {distribution=logNormal, typical=kout_pop, sd=omega_kout}",
               "e0 = {distribution=logNormal, typical=e0_pop, sd=omega_e0}",
               "",
               "[LONGITUDINAL]",
               "input={pkadd__err, prop__err, pdadd__err}", "file='pk.turnover.emax3-monolix.txt'",
               "",
               "DEFINITION:",
               "rx_prd_cp={distribution = normal, prediction = rx_pred_cp, errorModel=combined2(pkadd__err,prop__err)}",
               "rx_prd_effect={distribution = normal, prediction = rx_pred_effect, errorModel=constant(pdadd__err)}",
               "",
               "<FIT>",
               "data={y1, y2}",
               "model={rx_prd_cp, rx_prd_effect}",
               "",
               "<PARAMETER>",
               "ktr_pop={value=1, method=MLE}",
               "ka_pop={value=1, method=MLE}",
               "cl_pop={value=0.1, method=MLE}",
               "v_pop={value=10, method=MLE}",
               "prop__err={value=0.1, method=MLE}",
               "pkadd__err={value=0.1, method=MLE}",
               "emax_pop={value=0.8, method=MLE}", "ec50_pop={value=0.5, method=MLE}",
               "kout_pop={value=0.05, method=MLE}",
               "e0_pop={value=100, method=MLE}",
               "pdadd__err={value=10, method=MLE}",
               "omega_ktr={value=1, method=MLE}",
               "omega_ka={value=1, method=MLE}",
               "omega_cl={value=1.4142135623731, method=MLE}",
               "omega_v={value=1, method=MLE}",
               "omega_emax={value=0.707106781186548, method=MLE}",
               "omega_ec50={value=0.707106781186548, method=MLE}",
               "omega_kout={value=0.707106781186548, method=MLE}",
               "omega_e0={value=0.707106781186548, method=MLE}",
               "",
               "<MONOLIX>",
               "",
               "[TASKS]",
               "populationParameters()",
               "individualParameters(method = {conditionalMode})",
               "fim(method = Linearization)",
               "logLikelihood(method = Linearization)",
               "plotResult(method = {outputplot, indfits, obspred, residualsscatter, residualsdistribution, parameterdistribution, covariatemodeldiagnosis, randomeffects, covariancemodeldiagnosis, saemresults})",
               "",
               "[SETTINGS]",
               "GLOBAL:",
               "exportpath = 'pk.turnover.emax3-monolix'",
               "POPULATION:",
               "exploratoryautostop = no",
               "smoothingautostop = no",
               "burniniterations = 5",
               "exploratoryiterations = 250",
               "simulatedannealingiterations = 250",
               "smoothingiterations = 200",
               "exploratoryalpha = 0",
               "exploratoryinterval = 200",
               "omegatau = 0.95",
               "errormodeltau = 0.95")

    v <- .mlxtran(lines)

    expect_true(inherits(v, "monolix2rxMlxtran"))
    expect_true(inherits(v$DATAFILE$FILEINFO$FILEINFO, "monolix2rxFileinfo"))
    expect_true(inherits(v$DATAFILE$CONTENT$CONTENT, "monolix2rxContent"))
    expect_true(inherits(v$MODEL$INDIVIDUAL$INDIVIDUAL, "monolix2rxInd"))
    expect_true(inherits(v$MODEL$INDIVIDUAL$DEFINITION, "monolix2rxIndDef"))
    expect_true(inherits(v$MODEL$LONGITUDINAL$LONGITUDINAL, "monolix2rxLongitudinal"))
    expect_true(inherits(v$MODEL$LONGITUDINAL$DEFINITION, "monolix2rxLongDef"))
    expect_true(inherits(v$MODEL$LONGITUDINAL$PK, "monolix2rxPk"))
    expect_true(inherits(v$PARAMETER$PARAMETER, "monolix2rxParameter"))
    expect_true(inherits(v$MONOLIX$SETTINGS$GLOBAL, "monolix2rxOp"))
    expect_true(inherits(v$MONOLIX$SETTINGS$POPULATION, "monolix2rxOp"))
    expect_true(inherits(v$FIT$FIT, "monolix2rxFit"))
  })

})
