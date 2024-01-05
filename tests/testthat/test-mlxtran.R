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

    cov <- c("ktr_pop,0.032,-0.000357,-0.000198,-3.2e-05,-0.00207,-1.15e-05,-7.6e-05,1.12e-05,0,0,0,0,0,0,0,0,0,0,0",
      "ka_pop,-0.000357,0.0136,-0.000112,4.38e-05,-0.000144,0.000177,7.26e-07,1.83e-06,0,0,0,0,0,0,0,0,0,0,0",
      "cl_pop,-0.000198,-0.000112,0.00268,6.31e-05,0.000337,-0.00018,-5.72e-05,-6.07e-06,0,0,0,0,0,0,0,0,0,0,0",
      "v_pop,-3.2e-05,4.38e-05,6.31e-05,0.00192,-9.3e-05,0.000102,0.000112,6.81e-06,0,0,0,0,0,0,0,0,0,0,0",
      "emax_pop,-0.00207,-0.000144,0.000337,-9.3e-05,0.278,0.0107,-0.005,4.22e-05,0,0,0,0,0,0,0,0,0,0,0",
      "ec50_pop,-1.15e-05,0.000177,-0.00018,0.000102,0.0107,0.00791,3.67e-05,-5.18e-05,0,0,0,0,0,0,0,0,0,0,0",
      "kout_pop,-7.6e-05,7.26e-07,-5.72e-05,0.000112,-0.005,3.67e-05,0.00068,3.58e-05,0,0,0,0,0,0,0,0,0,0,0",
      "e0_pop,1.12e-05,1.83e-06,-6.07e-06,6.81e-06,4.22e-05,-5.18e-05,3.58e-05,0.00013,0,0,0,0,0,0,0,0,0,0,0",
      "omega_ktr,0,0,0,0,0,0,0,0,0.0191,-0.00133,-2.77e-06,-8.63e-07,-9.8e-05,-1.36e-07,-7.42e-06,-1.62e-07,2.72e-06,-2.51e-05,-5.21e-05",
      "omega_ka,0,0,0,0,0,0,0,0,-0.00133,0.00827,-1.68e-06,-9.16e-07,-4.75e-05,-1.75e-05,2.58e-06,5.56e-07,3.98e-06,-1.16e-05,-0.000231",
      "omega_cl,0,0,0,0,0,0,0,0,-2.77e-06,-1.68e-06,0.00144,-1.86e-05,-3.94e-06,-4.24e-06,-8.57e-07,1.3e-07,-4.19e-05,-1.8e-07,-2.76e-05",
      "omega_v,0,0,0,0,0,0,0,0,-8.63e-07,-9.16e-07,-1.86e-05,0.00113,7.11e-06,-3.8e-08,-1.06e-05,2.07e-07,2.38e-06,-1.79e-05,-2.41e-05",
      "omega_emax,0,0,0,0,0,0,0,0,-9.8e-05,-4.75e-05,-3.94e-06,7.11e-06,0.45,-0.00108,-0.000557,5.95e-05,-0.00019,-1.31e-05,-0.0104",
      "omega_ec50,0,0,0,0,0,0,0,0,-1.36e-07,-1.75e-05,-4.24e-06,-3.8e-08,-0.00108,0.00414,-2.19e-05,-1.03e-06,-4.29e-05,1.13e-06,-0.000396",
      "omega_kout,0,0,0,0,0,0,0,0,-7.42e-06,2.58e-06,-8.57e-07,-1.06e-05,-0.000557,-2.19e-05,0.000858,7.46e-06,-2e-05,-5.84e-07,-0.00193",
      "omega_e0,0,0,0,0,0,0,0,0,-1.62e-07,5.56e-07,1.3e-07,2.07e-07,5.95e-05,-1.03e-06,7.46e-06,0.000101,1.85e-06,-3.93e-08,-0.000428",
      "pkadd__err,0,0,0,0,0,0,0,0,2.72e-06,3.98e-06,-4.19e-05,2.38e-06,-0.00019,-4.29e-05,-2e-05,1.85e-06,0.00391,-0.000418,-0.000395",
      "prop__err,0,0,0,0,0,0,0,0,-2.51e-05,-1.16e-05,-1.8e-07,-1.79e-05,-1.31e-05,1.13e-06,-5.84e-07,-3.93e-08,-0.000418,0.000132,9.24e-06",
      "pdadd__err,0,0,0,0,0,0,0,0,-5.21e-05,-0.000231,-2.76e-05,-2.41e-05,-0.0104,-0.000396,-0.00193,-0.000428,-0.000395,9.24e-06,0.0629")

    ver2019 <- "*                          Monolix version : 5.1.1                             *"
    ver2020 <- "*                          Monolix version : 2020R1                            *"
    ver2021 <- "*                          Monolix version : 2021R1                            *"

    di1 <- c("DATASET INFORMATION",
             "Number of individuals: 32",
             "Number of observations (obsid 1): 251",
             "Number of observations (obsid 2): 232",
             "Number of doses: 32")

    di2 <- c("DATASET INFORMATION",
             "Number of individuals: 45",
             "Number of observations (DV): 176",
             "Number of doses: 51")

    v <- .mlxtran(lines)

    expect_equal(attr(v, "version"), NULL)
    expect_equal(attr(v, "dfSub"), 0L)
    expect_equal(attr(v, "dfObs"), 0L)
    expect_equal(attr(v, "ndose"), 0L)
    expect_equal(attr(v, "obsLst"), list())
    expect_equal(attr(v, "covSaTransformed"), NULL)
    expect_equal(attr(v, "covSaUntransformed"), NULL)
    expect_equal(attr(v, "covLinTransformed"), NULL)
    expect_equal(attr(v, "covLinUntransformed"), NULL)

    dir.create(v$MONOLIX$SETTINGS$GLOBAL$exportpath)
    dir.create(file.path(v$MONOLIX$SETTINGS$GLOBAL$exportpath,
                         "FisherInformation"))
    writeLines(cov, file.path(v$MONOLIX$SETTINGS$GLOBAL$exportpath,
                              "FisherInformation", "covarianceEstimatesLin.txt"))
    writeLines(cov, file.path(v$MONOLIX$SETTINGS$GLOBAL$exportpath,
                              "FisherInformation", "covarianceEstimatesSA.txt"))
    .df <- as.data.frame(v$PARAMETER$PARAMETER)
    .df$value <- .df$value * 2
    .df$parameter <- .df$name
    .dff <- file.path(v$MONOLIX$SETTINGS$GLOBAL$exportpath, "populationParameters.txt")
    write.csv(.df, .dff, quote=FALSE, row.names=FALSE)

    v2 <- .mlxtran(lines, update=TRUE)

    expect_equal(attr(v2, "version"), NULL)
    expect_equal(attr(v2, "dfSub"), 0L)
    expect_equal(attr(v2, "dfObs"), 0L)
    expect_equal(attr(v2, "ndose"), 0L)
    expect_equal(attr(v2, "obsLst"), list())
    expect_true(inherits(attr(v2, "covSaTransformed"), "matrix"))
    expect_true(inherits(attr(v2, "covSaUntransformed"), "matrix"))
    expect_true(inherits(attr(v2, "covLinTransformed"), "matrix"))
    expect_true(inherits(attr(v2, "covLinUntransformed"), "matrix"))

    writeLines(ver2019,
               file.path(v$MONOLIX$SETTINGS$GLOBAL$exportpath,"summary.txt"))

    v3 <- .mlxtran(lines, update=TRUE)

    expect_equal(attr(v3, "version"), "5.1.1")

    expect_equal(attr(v3, "dfSub"), 0L)
    expect_equal(attr(v3, "dfObs"), 0L)
    expect_equal(attr(v3, "ndose"), 0L)
    expect_equal(attr(v3, "obsLst"), list())
    expect_true(inherits(attr(v3, "covSaTransformed"), "matrix"))
    expect_true(inherits(attr(v3, "covSaUntransformed"), "matrix"))
    expect_true(inherits(attr(v3, "covLinTransformed"), "matrix"))
    expect_true(inherits(attr(v3, "covLinUntransformed"), "matrix"))

    # The default is to assume everything is untransformed; v2 and v3
    # matrices should be the same
    expect_equal(attr(v2, "covSaTransformed"), attr(v3, "covSaTransformed"))
    expect_equal(attr(v2, "covSaUntransformed"), attr(v3, "covSaUntransformed"))
    expect_equal(attr(v2, "covLinTransformed"), attr(v3, "covLinTransformed"))
    expect_equal(attr(v2, "covLinUntransformed"), attr(v3, "covLinUntransformed"))

    writeLines(ver2020,
               file.path(v$MONOLIX$SETTINGS$GLOBAL$exportpath,"summary.txt"))
    v4 <- .mlxtran(lines, update=TRUE)

    expect_equal(attr(v4, "version"),
                 "2020R1")
    expect_equal(attr(v4, "dfSub"), 0L)
    expect_equal(attr(v4, "dfObs"), 0L)
    expect_equal(attr(v4, "ndose"), 0L)
    expect_equal(attr(v4, "obsLst"), list())
    expect_true(inherits(attr(v4, "covSaTransformed"), "matrix"))
    expect_true(inherits(attr(v4, "covSaUntransformed"), "matrix"))
    expect_true(inherits(attr(v4, "covLinTransformed"), "matrix"))
    expect_true(inherits(attr(v4, "covLinUntransformed"), "matrix"))

    # 2020 SA is transformed and Lin is untransformed
    expect_equal(attr(v2, "covLinTransformed"), attr(v4, "covLinTransformed"))
    expect_equal(attr(v2, "covLinUntransformed"), attr(v4, "covLinUntransformed"))

    expect_false(identical(attr(v2, "covSaTransformed"), attr(v4, "covSaTransformed")))
    expect_false(identical(attr(v2, "covSaUntransformed"), attr(v4, "covSaUntransformed")))

    writeLines(ver2021,
               file.path(v$MONOLIX$SETTINGS$GLOBAL$exportpath,"summary.txt"))
    v5 <- .mlxtran(lines, update=TRUE)

    expect_equal(attr(v5, "version"),
                 "2021R1")
    expect_equal(attr(v5, "dfSub"), 0L)
    expect_equal(attr(v5, "dfObs"), 0L)
    expect_equal(attr(v5, "ndose"), 0L)
    expect_equal(attr(v5, "obsLst"), list())
    expect_true(inherits(attr(v5, "covSaTransformed"), "matrix"))
    expect_true(inherits(attr(v5, "covSaUntransformed"), "matrix"))
    expect_true(inherits(attr(v5, "covLinTransformed"), "matrix"))
    expect_true(inherits(attr(v5, "covLinUntransformed"), "matrix"))

    # 2021 all cov are transformed
    expect_equal(attr(v4, "covSaTransformed"), attr(v5, "covLinTransformed"))
    expect_equal(attr(v4, "covSaUntransformed"), attr(v5, "covLinUntransformed"))

    expect_equal(attr(v4, "covSaTransformed"), attr(v5, "covSaTransformed"))
    expect_equal(attr(v4, "covSaUntransformed"), attr(v5, "covSaUntransformed"))

    writeLines(c(ver2021, di1),
               file.path(v$MONOLIX$SETTINGS$GLOBAL$exportpath,"summary.txt"))

    v6 <- .mlxtran(lines, update=TRUE)

    expect_equal(attr(v6, "version"),
                 "2021R1")
    expect_equal(attr(v6, "dfSub"), 32L)
    expect_equal(attr(v6, "dfObs"), 483L)
    expect_equal(attr(v6, "ndose"), 32L)
    expect_equal(attr(v6, "obsLst"), list(`obsid 1`=251L, `obsid 2`=232L))
    expect_true(inherits(attr(v6, "covSaTransformed"), "matrix"))
    expect_true(inherits(attr(v6, "covSaUntransformed"), "matrix"))
    expect_true(inherits(attr(v6, "covLinTransformed"), "matrix"))
    expect_true(inherits(attr(v6, "covLinUntransformed"), "matrix"))

    # 2021 all cov are transformed
    expect_equal(attr(v4, "covSaTransformed"), attr(v6, "covLinTransformed"))
    expect_equal(attr(v4, "covSaUntransformed"), attr(v6, "covLinUntransformed"))

    expect_equal(attr(v4, "covSaTransformed"), attr(v6, "covSaTransformed"))
    expect_equal(attr(v4, "covSaUntransformed"), attr(v6, "covSaUntransformed"))

    writeLines(c(ver2021, di2),
               file.path(v$MONOLIX$SETTINGS$GLOBAL$exportpath,"summary.txt"))

    v7 <- .mlxtran(lines, update=TRUE)

    expect_equal(attr(v7, "version"),
                 "2021R1")
    expect_equal(attr(v7, "dfSub"), 45L)
    expect_equal(attr(v7, "dfObs"), 176L)
    expect_equal(attr(v7, "ndose"), 51L)
    expect_equal(attr(v7, "obsLst"), list(`DV`=176L))
    expect_true(inherits(attr(v7, "covSaTransformed"), "matrix"))
    expect_true(inherits(attr(v7, "covSaUntransformed"), "matrix"))
    expect_true(inherits(attr(v7, "covLinTransformed"), "matrix"))
    expect_true(inherits(attr(v7, "covLinUntransformed"), "matrix"))

    # 2021 all cov are transformed
    expect_equal(attr(v4, "covSaTransformed"), attr(v7, "covLinTransformed"))
    expect_equal(attr(v4, "covSaUntransformed"), attr(v7, "covLinUntransformed"))

    expect_equal(attr(v4, "covSaTransformed"), attr(v7, "covSaTransformed"))
    expect_equal(attr(v4, "covSaUntransformed"), attr(v7, "covSaUntransformed"))



    unlink(file.path(v$MONOLIX$SETTINGS$GLOBAL$exportpath,
                     "FisherInformation", "covarianceEstimatesLin.txt"))
    unlink(file.path(v$MONOLIX$SETTINGS$GLOBAL$exportpath,
                     "FisherInformation", "covarianceEstimatesSA.txt"))
    unlink(.dff)
    unlink(v$MONOLIX$SETTINGS$GLOBAL$exportpath, recursive = TRUE)

    expect_equal(v$PARAMETER$PARAMETER$value * 2,
                 v2$PARAMETER$PARAMETER$value)

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

    v <- .mlxtran(lines, equation=TRUE)

    expect_snapshot(print(v))

    expect_error(as.list(v), NA)

    unlink("pk.turnover.emax3-monolix.txt")

    v7$PARAMETER$PARAMETER <- v$PARAMETER$PARAMETER

    if (requireNamespace("rxode2", quietly=TRUE)) {
      expect_error(monolix2rx(v6, update=FALSE)) # emax out of range in updated parameters
      rx <- monolix2rx(v7, update=FALSE)
      expect_true(inherits(rx, "rxUi"))
      expect_true(inherits(rx$thetaMat, "matrix"))
      expect_equal(rx$dfSub, 45)
      expect_equal(rx$dfObs, 176)
      expect_equal(rx$description,
                   "model translated from `babelmixr2` and `nlmixr2` function pk.turnover.emax3 to pk.turnover.emax3-monolix.txt")
    }

  })

})
