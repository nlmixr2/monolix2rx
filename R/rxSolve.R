# This is built from buildParser.R, edit there
#'@export
rxSolve.monolix2rx <- function(object, params = NULL, events = NULL, 
    inits = NULL, scale = NULL, method = c("liblsoda", "lsoda", 
        "dop853", "indLin"), sigdig = NULL, atol = 1e-08, rtol = 1e-06, 
    maxsteps = 70000L, hmin = 0, hmax = NA_real_, hmaxSd = 0, 
    hini = 0, maxordn = 12L, maxords = 5L, ..., cores, covsInterpolation = c("locf", 
        "linear", "nocb", "midpoint"), nStud = 1L, dfSub = 0, 
    dfObs = 0, thetaMat = NULL, ssAtol = 1e-08, ssRtol = 1e-06, 
    minSS = 10L, maxSS = 10000L, envir = parent.frame()) {
    if (missing(cores)) {
        cores <- 0L
    }
    if (missing(covsInterpolation)) {
        covsInterpolation <- "locf"
        .minfo("using locf interpolation like Monolix, specify directly to change")
    }
    if (!missing(nStud)) {
        if (missing(dfSub)) {
            if (!is.null(object$meta$dfSub)) {
                dfSub <- object$meta$dfSub
                .minfo(paste0("using dfSub=", dfSub, " from Monolix"))
            }
            else if (!is.null(object$dfSub)) {
                dfSub <- object$dfSub
                .minfo(paste0("using dfSub=", dfSub, " from Monolix"))
            }
        }
        if (missing(dfObs)) {
            if (!is.null(object$meta$dfObs)) {
                dfObs <- object$meta$dfObs
                .minfo(paste0("using dfObs=", dfObs, " from Monolix"))
            }
            else if (!is.null(object$dfObs)) {
                dfObs <- object$dfObs
                dfObs <- object$meta$dfObs
                .minfo(paste0("using dfObs=", dfObs, " from Monolix"))
            }
        }
        if (missing(thetaMat)) {
            if (!is.null(object$meta$thetaMat)) {
                thetaMat <- object$meta$thetaMat
                .minfo(paste0("using thetaMat from Monolix"))
            }
            else if (!is.null(object$thetaMat)) {
                thetaMat <- object$meta$thetaMat
                .minfo(paste0("using thetaMat from Monolix"))
            }
        }
    }
    if ((missing(events) && missing(params))) {
        if (!is.null(object$monolixData)) {
            events <- object$monolixData
            .minfo(paste0("using Monolix's data for solving"))
        }
    }
    .atol <- .rtol <- .getRtolAtol(object)
    if (missing(atol)) {
        atol <- .atol
        .minfo(paste0("using Monolix specified atol=", atol))
    }
    if (missing(rtol)) {
        rtol <- .rtol
        .minfo(paste0("using Monolix specified rtol=", rtol))
    }
    if (missing(ssRtol)) {
        ssRtol <- 100
        .minfo(paste0("Since Monolix doesn't use ssRtol, set ssRtol=", 
            ssRtol))
    }
    if (missing(ssAtol)) {
        ssAtol <- 100
        .minfo(paste0("Since Monolix doesn't use ssRtol, set ssAtol=", 
            ssAtol))
    }
    .nss <- .getNbdoses(object)
    if (missing(maxSS) && missing(maxSS)) {
        maxSS <- .nss + 1
        minSS <- .nss
        .minfo(paste0("Since Monolix uses a set number of doses for steady state use maxSS=", 
            maxSS, ", minSS=", minSS))
    }
    .cls <- class(object)
    class(object) <- .cls[-which(.cls == "monolix2rx")]
    rxode2::rxSolve(object = object, params = params, events = events, 
        inits = inits, scale = scale, method = method, sigdig = sigdig, 
        atol = atol, rtol = rtol, maxsteps = maxsteps, hmin = hmin, 
        hmax = hmax, hmaxSd = hmaxSd, hini = hini, maxordn = maxordn, 
        maxords = maxords, ..., cores = cores, covsInterpolation = covsInterpolation, 
        nStud = nStud, dfSub = dfSub, dfObs = dfObs, thetaMat = thetaMat, 
        ssAtol = ssAtol, ssRtol = ssRtol, minSS = minSS, maxSS = 10000L, 
        envir = envir)
}
