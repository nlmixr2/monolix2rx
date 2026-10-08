## Monolix tasks and settings: what Monolix writes (and where) changes,
## the model does not.  Mostly checked in run mode; nbSSDoses is in
## 03-dosing.R and odeType in 02-ode.R.

.tasksCase <- function(name, covers, est="default", ...) {
  kitVariant("pkmodel-oral-1cmt", name, covers, tags=c("tasks"), est=est, ...)
}

.tasksEst <- function(...) {
  paste(c("[TASKS]", "populationParameters()", ..., .kitPlots), collapse="\n")
}

.tasksCase("tasks-no-exportpath",
           "no <MONOLIX> [SETTINGS] exportpath: results are in the directory named like the project",
           exportpath=NA)

.tasksCase("tasks-exportpath-subdir",
           "exportpath in a subdirectory (results/fit1)",
           exportpath="results/fit1")

.tasksCase("tasks-fim-lin",
           "FIM and log-likelihood by linearization (covarianceEstimatesLin.txt, no SA)",
           est=.tasksEst("individualParameters(method = {conditionalMean, conditionalMode })",
                         "fim(method = Linearization)",
                         "logLikelihood(method = Linearization)"))

.tasksCase("tasks-cond-mode",
           "individual parameters by the conditional mode only, no FIM",
           est=.tasksEst("individualParameters(method = conditionalMode)"))

.tasksCase("tasks-pop-only",
           "population parameters only: no individual parameters, FIM or likelihood tasks",
           est=.tasksEst())
