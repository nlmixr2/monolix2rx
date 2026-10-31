## MAP priors of the other shapes the import writes as ini() priors: a
## covariate coefficient (normal), a bounded logitNormal parameter and a
## probitNormal one; each case checks the imported prior

## dryData: the imported iniDf carries `prior` (name -> dnorm() text)
.mapCheck <- function(prior) {
  force(prior)
  function(m, sim) {
    .i <- m$iniDf
    if (is.null(.i$prior)) return("no prior column in iniDf")
    .got <- .i$prior[match(names(prior), .i$name)]
    .bad <- names(prior)[is.na(.got) | .got != prior]
    if (length(.bad)) paste("prior not imported as expected:", paste(.bad, collapse=", "))
  }
}

## an untransformed covariate, which babelmixr2 also writes back
kitVariant("cov-untransformed", "param-map-coef",
           "a normal MAP prior on a covariate coefficient (beta_Cl_WT)",
           tags=c("params", "prior", "covariate"),
           mlxtran=.mapProject("cov-untransformed",
                               "beta_Cl_WT = {distribution=normal, typical=0.01, sd=0.005}",
                               "beta_Cl_WT"),
           dryData=.mapCheck(c(beta_Cl_WT="dnorm(0.01, 0.005)")))

kitVariant("param-probit", "param-map-probit",
           "a probitNormal MAP prior on p_pop",
           tags=c("params", "prior", "probit"),
           mlxtran=.mapProject("param-probit",
                               "p_pop = {distribution=probitNormal, typical=0.7, sd=0.3}", "p_pop"),
           dryData=.mapCheck(c(p_pop=paste0("dnorm(", signif(qnorm(0.7), 15), ", 0.3)"))))

kitVariant("param-logit-bounds", "param-map-logit-bounds",
           "a logitNormal MAP prior with the parameter's min=0.5, max=3 on ka_pop",
           tags=c("params", "prior", "logit"),
           mlxtran=.mapProject("param-logit-bounds",
                               "ka_pop = {distribution=logitNormal, min=0.5, max=3, typical=1.2, sd=0.3}",
                               "ka_pop"),
           dryData=.mapCheck(c(ka_pop=paste0("dnorm(", signif(qlogis((1.2 - 0.5) / 2.5), 15), ", 0.3)"))))
