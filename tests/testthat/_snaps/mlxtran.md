# mlxtran initial list

    Code
      print(v)
    Output
      DESCRIPTION:
      model translated from `babelmixr2` and `nlmixr2` function pk.turnover.emax3 to pk.turnover.emax3-monolix.txt
      
      <DATAFILE>
      [FILEINFO]
      ; parsed: $DATAFILE$FILEINFO$FILEINFO
      file = 'pk.turnover.emax3-monolix.csv'
      delimiter = comma
      header = {ID, TIME, EVID, AMT, DV, ADM, YTYPE, nlmixrRowNums}
      
      [CONTENT]
      ; parsed: $DATAFILE$CONTENT$CONTENT
      ID = {use=identifier}
      TIME = {use=time}
      EVID = {use=eventidentifier}
      AMT = {use=amount}
      YTYPE = {use=observationtype}
      ADM = {use=administration}
      DV = {use=observation, name={y1, y2}, yname={'1', '2'}, type={continuous, continuous}}
      
      <MODEL>
      [INDIVIDUAL]
      ; parsed: $MODEL$INDIVIDUAL$INDIVIDUAL
      input = {ktr_pop, omega_ktr, ka_pop, omega_ka, cl_pop, omega_cl, v_pop, omega_v, emax_pop, omega_emax, ec50_pop, omega_ec50, kout_pop, omega_kout, e0_pop, omega_e0}
      
      DEFINITION:
      ; parsed: $MODEL$INDIVIDUAL$DEFINITION
      ktr = {distribution=lognormal, typical=ktr_pop, sd=omega_ktr}
      ka = {distribution=lognormal, typical=ka_pop, sd=omega_ka}
      cl = {distribution=lognormal, typical=cl_pop, sd=omega_cl}
      v = {distribution=lognormal, typical=v_pop, sd=omega_v}
      emax = {distribution=logitnormal, typical=emax_pop, sd=omega_emax, min=0, max=1}
      ec50 = {distribution=lognormal, typical=ec50_pop, sd=omega_ec50}
      kout = {distribution=lognormal, typical=kout_pop, sd=omega_kout}
      e0 = {distribution=lognormal, typical=e0_pop, sd=omega_e0}
      
      [LONGITUDINAL]
      ; parsed: $MODEL$LONGITUDINAL$LONGITUDINAL
      input = {pkadd__err, prop__err, pdadd__err, ktr, ka, cl, v, emax, ec50, kout, e0}
      
      DEFINITION:
      ; parsed: $MODEL$LONGITUDINAL$DEFINITION
      rx_prd_cp = {distribution=normal, prediction=rx_pred_cp, errorModel=combined2(pkadd__err, prop__err)}
      rx_prd_effect = {distribution=normal, prediction=rx_pred_effect, errorModel=constant(pdadd__err)}
      
      PK:
      ; parsed: $MODEL$LONGITUDINAL$PK
      compartment(cmt = 1, amount = depot, volume = 1.0)
      
      depot(adm = 1, target = depot, Tlag = 0, p = 1)
      
      EQUATION:
      ; parsed: $MODEL$LONGITUDINAL$EQUATION
      DCP = center/v
      PD = 1-emax*DCP/(ec50+DCP)
      effect_0 = e0
      kin = e0*kout
      ddt_depot = - ktr*depot
      ddt_gut = ktr*depot-ka*gut
      ddt_center = ka*gut-cl/v*center
      ddt_effect = kin*PD-kout*effect
      cp = center/v
      rx_pred_cp = cp
      rx_pred_effect = effect
      
      OUTPUT:
      ; parsed: $MODEL$LONGITUDINAL$OUTPUT
      output = {rx_pred_cp, rx_pred_effect}
      
      <FIT>
      ; parsed: $FIT$FIT
      data = {y1, y2}
      model = {rx_prd_cp, rx_prd_effect}
      
      <PARAMETER>
      ; parsed: $PARAMETER$PARAMETER
      ktr_pop = {value=1, method=MLE}
      ka_pop = {value=1, method=MLE}
      cl_pop = {value=0.1, method=MLE}
      v_pop = {value=10, method=MLE}
      prop__err = {value=0.1, method=MLE}
      pkadd__err = {value=0.1, method=MLE}
      emax_pop = {value=0.8, method=MLE}
      ec50_pop = {value=0.5, method=MLE}
      kout_pop = {value=0.05, method=MLE}
      e0_pop = {value=100, method=MLE}
      pdadd__err = {value=10, method=MLE}
      omega_ktr = {value=1, method=MLE}
      omega_ka = {value=1, method=MLE}
      omega_cl = {value=1.4142135623731, method=MLE}
      omega_v = {value=1, method=MLE}
      omega_emax = {value=0.707106781186548, method=MLE}
      omega_ec50 = {value=0.707106781186548, method=MLE}
      omega_kout = {value=0.707106781186548, method=MLE}
      omega_e0 = {value=0.707106781186548, method=MLE}
      
      <MONOLIX>
      [TASKS]
      ; parsed: $MONOLIX$TASKS$TASKS
      populationParameters()
      individualParameters(method = {conditionalMode})
      fim(method = Linearization)
      logLikelihood(method = Linearization)
      plotResult(method = {outputplot, indfits, obspred, residualsscatter, residualsdistribution, parameterdistribution, covariatemodeldiagnosis, randomeffects, covariancemodeldiagnosis, saemresults})
      
      [SETTINGS]
      GLOBAL:
      ; parsed: $MONOLIX$SETTINGS$GLOBAL
      exportpath = 'pk.turnover.emax3-monolix'
      
      POPULATION:
      ; parsed: $MONOLIX$SETTINGS$POPULATION
      exploratoryautostop = no
      smoothingautostop = no
      burniniterations = 5
      exploratoryiterations = 250
      simulatedannealingiterations = 250
      smoothingiterations = 200
      exploratoryalpha = 0
      exploratoryinterval = 200
      omegatau = 0.95
      errormodeltau = 0.95

