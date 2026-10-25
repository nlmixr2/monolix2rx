## Probes from the first Monolix 2024R1 run: rewrites of the cases Monolix
## refused, to find what it accepts, and a factorial() of a non-integer

## the empty()/reset() targets are ODE states, not compartment() amounts
.macroEventOde <- function(event) {
  paste0("DESCRIPTION: {{PROBLEM}}

[LONGITUDINAL]
input = {ka, V, Cl}

PK:
depot(adm=1, target=Ad)
", event, "

EQUATION:
ddt_Ad = -ka*Ad
ddt_Ac = ka*Ad - Cl/V*Ac
Cc = Ac/V

OUTPUT:
output = Cc
")
}

kitVariant("macro-empty", "macro-empty-ode",
           "empty(adm=2, target=Ac) on an ODE state, the dose by depot(adm=1, target=Ad)",
           model=.macroEventOde("empty(adm=2, target=Ac)"))

kitVariant("macro-reset", "macro-reset-ode",
           "reset(adm=3) with ODE states, the dose by depot(adm=1, target=Ad)",
           model=.macroEventOde("reset(adm=3)"))

## Monolix ignored an EVID=3 line with AMT=.; with AMT=0 it resets
kitVariant("dose-evid3-reset", "dose-evid3-amt0",
           "EVID=3 line (reset, no dose) written with AMT=0",
           data=function(nSub) {
             .id <- seq_len(nSub)
             mlxBind(mlxDose(.id, 0, amt=100, cmt=1),
                     mlxOther(.id, 8, cmt=1, evid=3L),
                     mlxDose(.id, 10, amt=100, cmt=1),
                     mlxObs(.id, c(1, 2, 4, 7.5, 9, 11, 12, 14, 18, 24), cmt=2))
           },
           write=function(d) {
             .w <- .kitMonolixRows(d)
             .w$AMT[.w$EVID == 3L] <- 0
             .w[, c("ID", "TIME", "AMT", "EVID", "DV")]
           })

kitVariant("fun-not-signed", "fun-not-parens",
           "a negation in parentheses after an operator and as an exponent (0.5*(-~f), 2^(~f))",
           knownRun="Monolix 2024R1 refuses -~f, even in parentheses (a syntax error at (-~f))",
           model=.oralFunModel("if t > 12
  f = 1
else
  f = 0
end
Y = Cc*(2 + 0.5*(-~f))*2^(~f)"))

kitVariant("macro-iv-peripheral", "macro-iv-peripheral-after",
           "iv(adm=2, cmt=2) after the peripheral() macro that defines compartment 2",
           model=.kitSub("macro-iv-peripheral", "model", "iv(adm=2, cmt=2)\nperipheral(k12, k21)",
                         "peripheral(k12, k21)\niv(adm=2, cmt=2)"))

## x is in (1, 2): a floor gives 1! and log(2!), a rounding 2! and log(3!)
## once Cc > 1; Monolix 2024R1 rounds
kitVariant("pkmodel-oral-1cmt", "fun-factorial-fraction",
           "factorial(x) and factln(1 + x) of a non-integer x in (1, 2): the argument is rounded",
           tags=c("function"),
           model=.oralFunModel("x = 1 + Cc/(1 + Cc)
Y = Cc + factorial(x) + factln(1 + x)"),
           sim=.oralErrTruth(quote(add(a) + prop(b) + combined1()),
                             list(quote(a <- 0.05), quote(b <- 0.1)),
                             out=quote(Y),
                             extra=list(quote(x <- 1 + Cc / (1 + Cc)),
                                        quote(Y <- Cc + factorial(round(x)) + lfactorial(round(1 + x))))),
           mlxtran=.mlxProject(.oralFunPars, pred="Y"))
