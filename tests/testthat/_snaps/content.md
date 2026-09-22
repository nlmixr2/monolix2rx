# content

    Code
      print(tmp)
    Output
      ID = {use=identifier}
      TIME = {use=time}
      EVID = {use=eventidentifier}
      AMT = {use=amount}
      YTYPE = {use=observationtype}
      ADM = {use=administration}
      DV = {use=observation, name={y1, y2}, yname={'1', '2'}, type={continuous, continuous}}
      E0 = {use = regressor}
      Emax = {use = regressor}
      WT = {use=covariate, type=continuous}
      CRCL = {use=covariate, type=continuous}
      Race = {type=categorical, categories={Caucasian, Black, Latin}}
      Sex = {type=categorical, categories={M, F}}

