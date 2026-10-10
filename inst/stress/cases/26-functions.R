## Monolix functions and operators no other case uses: rem(a, b), the
## hyperbolic and inverse trigonometric functions, ceil(), the negation
## ~a / !a; an explicit odeType=nonStiff and the older mean= spelling

.oralFunPars <- list(ka=.mlxPar(1.2, 0.3), V=.mlxPar(30, 0.2), Cl=.mlxPar(3, 0.3))

## the oral pkmodel() with EQUATION: lines `eq` after Cc, output Y
.oralFunModel <- function(eq) {
  paste0("DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

EQUATION:
Cc = pkmodel(ka, V, Cl)
", eq, "

OUTPUT:
output = Y
")
}

## a circadian input: rem(t, 24) is the time of day over five days
kitVariant("ode-turnover", "fun-rem-circadian",
           "rem(t, 24) (time of day) in a circadian turnover input",
           tags=c("ode", "pd", "function"),
           sim=function() {
             ini({
               ka_pop <- 1.2; V_pop <- 30; Cl_pop <- 3
               Kin_pop <- 10; Kout_pop <- 0.1; Imax_pop <- 0.8; IC50_pop <- 1
               omega_V ~ 0.04; omega_Cl ~ 0.09; omega_Kout ~ 0.04; omega_Imax ~ 0.25
               a <- 1; b <- 0.05
             })
             model({
               ka <- ka_pop
               V <- V_pop * exp(omega_V)
               Cl <- Cl_pop * exp(omega_Cl)
               Kin <- Kin_pop
               Kout <- Kout_pop * exp(omega_Kout)
               Imax <- expit(logit(Imax_pop) + omega_Imax)
               IC50 <- IC50_pop
               R(0) <- Kin / Kout
               d/dt(depot) <- -ka * depot
               d/dt(central) <- ka * depot - Cl / V * central
               Cc <- central / V
               tod <- time %% 24
               d/dt(R) <- Kin * (1 + 0.3 * sin(2 * 3.14159265 * tod / 24)) * (1 - Imax * Cc / (Cc + IC50)) - Kout * R
               R ~ add(a) + prop(b) + combined1()
             })
           },
           model=local({
             .m <- sub("ddt_R = Kin*(1 - E) - Kout*R",
                       "tod = rem(t, 24)\nddt_R = Kin*(1 + 0.3*sin(2*3.14159265*tod/24))*(1 - E) - Kout*R",
                       .kitEnv$cases[["ode-turnover"]]$model, fixed=TRUE)
             if (!grepl("rem(t, 24)", .m, fixed=TRUE)) stop("fun-rem-circadian: the base model changed")
             .m
           }))

kitVariant("pkmodel-oral-1cmt", "fun-hyperbolic",
           "sinh(), cosh(), tanh(), atan2(), asin() and ceil() in an output",
           tags=c("function"),
           model=.oralFunModel("u = Cc/10
Y = 10*tanh(u) + sinh(u) - cosh(u) + 1 + atan2(Cc, 10) + asin(u/(1 + u)) + ceil(t/1000)"),
           sim=.oralErrTruth(quote(add(a) + prop(b) + combined1()),
                             list(quote(a <- 0.05), quote(b <- 0.1)),
                             out=quote(Y),
                             extra=list(quote(u <- Cc / 10),
                                        quote(Y <- 10 * tanh(u) + sinh(u) - cosh(u) + 1 + atan2(Cc, 10) +
                                                asin(u / (1 + u)) + ceil(time / 1000)))),
           mlxtran=.mlxProject(.oralFunPars, pred="Y"))

## the output switches once t >= 12 and Cc <= 2
kitVariant("pkmodel-oral-1cmt", "fun-not-operator",
           "the negations ~a and !a in an if condition",
           tags=c("function", "ifelse"),
           model=.oralFunModel("if ~(t < 12) & !(Cc > 2)
  f = 1
else
  f = 0
end
Y = Cc*(1 + 0.5*f)"),
           sim=.oralErrTruth(quote(add(a) + prop(b) + combined1()),
                             list(quote(a <- 0.05), quote(b <- 0.1)),
                             out=quote(Y),
                             extra=list(quote(f <- 0),
                                        quote(if (!(time < 12) && !(Cc > 2)) f <- 1),
                                        quote(Y <- Cc * (1 + 0.5 * f)))),
           mlxtran=.mlxProject(.oralFunPars, pred="Y"))

kitVariant("ode-turnover", "ode-nonstiff-explicit",
           "odeType = nonStiff written out (the default)",
           tags=c("ode"),
           model=local({
             .m <- sub("EQUATION:\n", "EQUATION:\nodeType = nonStiff\n",
                       .kitEnv$cases[["ode-turnover"]]$model, fixed=TRUE)
             if (!grepl("odeType = nonStiff", .m, fixed=TRUE)) stop("ode-nonstiff-explicit: the base model changed")
             .m
           }))

## mean= is the mean of the transformed parameter (log(ka) ~ N(ka_pop, sd)),
## so the values are on the log scale; whether a Monolix project (not only
## Simulx) accepts it is to confirm in run mode
kitVariant("pkmodel-oral-1cmt", "param-mean-keyword",
           "[INDIVIDUAL] DEFINITION: mean= (the log-scale mean) instead of typical=",
           tags=c("params", "syntax"),
           mlxtran=local({
             .m <- gsub("typical=", "mean=",
                        .mlxProject(list(ka=.mlxPar(log(1.2), 0.3), V=.mlxPar(log(30), 0.2),
                                         Cl=.mlxPar(log(3), 0.3))), fixed=TRUE)
             if (grepl("typical=", .m, fixed=TRUE) || !grepl("mean=ka_pop", .m, fixed=TRUE)) {
               stop("param-mean-keyword: the project template changed")
             }
             .m
           }))
