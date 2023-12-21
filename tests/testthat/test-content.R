tmp <- .content("ID = {use=identifier}
TIME = {use=time}
EVID = {use=eventidentifier}
AMT = {use=amount}
DV = {use=observation, name={y1, y2}, yname={'1', '2'}, type={continuous, continuous}}
ADM = {use=administration}
YTYPE = {use=observationtype}
WT={use=covariate, type=continuous}
CRCL={use=covariate, type=continuous}
E0 = {use = regressor}
Emax = {use = regressor}
Race = {type=categorical, categories={Caucasian, Black, Latin}}
Sex = {type=categorical, categories={M, F}}")
