# input categorical covariates and regressors

    Code
      print(.tmp)
    Output
      input = {V_pop, omega_V, ka_pop, omega_ka, Cl_pop, omega_Cl, logtAge, Race, Sex, logtWeight, beta_Cl_Race_Caucasian, beta_Cl_Race_Latin, beta_Cl_Smoke_yes, beta_Cl_logtAge, beta_V_logtWeight, E0}
      E0 = {use = regressor}
      Race = {type=categorical, categories={Caucasian, Black, Latin}}
      Sex = {type=categorical, categories={M, F}}

---

    Code
      print(.tmp)
    Output
      input = {AGE, DOSE, SEX}
      DOSE = {type=categorical, categories={'50 mg', '100 mg'}}
      SEX = {type=categorical, categories={Female, Male}}

