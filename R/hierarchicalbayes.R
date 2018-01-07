#' @importFrom rstan rstan_options stan extract sampling
#' @importFrom flipU InterceptWarnings
hierarchicalBayesChoiceModel <- function(dat, n.iterations = 500, n.chains = 8,
                                         max.tree.depth = 10,
                                         adapt.delta = 0.8, seed = 123,
                                         keep.samples = FALSE, n.classes = 1,
                                         include.stanfit = TRUE,
                                         normal.covariance = "Full",
                                         show.stan.warnings = TRUE,
                                         max.draws = 100, ...)
{
    # We want to replace this call with a proper integration of rstan into this package
    require(rstan)

    # allows Stan chains to run in parallel on multiprocessor machines
    options(mc.cores = parallel::detectCores())

    stan.dat <- createStanData(dat, n.classes, normal.covariance)

    if (IsRServer()) # R servers
    {
        stan.model <- stanModel(n.classes, normal.covariance)
        stan.file <- NULL
    }
    else
    {
        stan.model <- NULL
        stan.file <- stanFileName(n.classes, normal.covariance)
    }

    on.warnings <- GetStanWarningHandler(show.stan.warnings)

    InterceptWarnings({
        stan.fit <- RunStanSampling(stan.dat, n.iterations, n.chains,
                                    max.tree.depth, adapt.delta, seed,
                                    stan.model, stan.file, ...)},
                                    on.warnings)

    result <- list()
    result$respondent.parameters <- ComputeRespPars(stan.fit, dat$var.names,
                                                    dat$subset,
                                                    dat$variable.scales)
    result$parameter.statistics <- GetParameterStatistics(stan.fit)
    if (include.stanfit)
    {
        result$stan.fit <- if (keep.samples) stan.fit else ReduceStanFitSize(stan.fit)
        result$beta.draws <- ExtractBetaDraws(stan.fit, max.draws)
    }
    class(result) <- "FitChoice"
    result
}

#' @title RunStanSampling
#' @description Wrapper function for \code{rstan:stan} and
#' \code{rstan:sampling} to run Stan HB analysis.
#' @param stan.dat The data to be passed to Stan.
#' @param n.iterations The number of iterations in the analysis.
#' @param n.chains The number of chains in the analysis.
#' @param max.tree.depth Maximum tree depth setting. See Stan documentation.
#' @param adapt.delta Adapt delta setting. See Stan documentation.
#' @param seed Random seed.
#' @param stan.model Complied Stan model (if running on the R server).
#' @param stan.file Path to Stan file (if not running on the R server).
#' @param ... Additional parameters to pass on to \code{rstan::stan} and
#' \code{rstan::sampling}.
#' @return A stanfit object.
#' @export
RunStanSampling <- function(stan.dat, n.iterations, n.chains,
                            max.tree.depth, adapt.delta,
                            seed, stan.model, stan.file, ...)
{
    pars <- c("theta", "sigma", "beta")

    if (IsRServer()) # R servers
    {
        # Loads a precompiled stan model called mod from sysdata.rda to avoid recompiling.
        # The R code used to generate mod on a linux machine is:
        # mod <- rstan::stan_model(model_code = model.code)
        # devtools::use_data(mod, internal = TRUE, overwrite = TRUE)
        # where model.code is the stan code as a string.
        # Ideally we would want to recompile when the package is built (similar to Rcpp)
        result <- sampling(stan.model, data = stan.dat, chains = n.chains,
                           pars = pars, iter = n.iterations, seed = seed,
                           control = list(max_treedepth = max.tree.depth,
                                          adapt_delta = adapt.delta), ...)
    }
    else # Not R servers
    {
        result <- stan(file = stan.file, data = stan.dat, iter = n.iterations,
                       chains = n.chains, seed = seed, pars = pars,
                       control = list(max_treedepth = max.tree.depth,
                                      adapt_delta = adapt.delta), ...)
    }
    result
}

createStanData <- function(dat, n.classes, normal.covariance)
{
    stan.dat <- list(C = dat$n.choices,
                     R = dat$n.respondents,
                     S = dat$n.questions.left.in,
                     A = dat$n.attributes,
                     V = dat$n.variables,
                     V_raw = dat$n.raw.variables,
                     V_attribute = dat$n.attribute.variables,
                     Y = dat$Y.in,
                     X = dat$X.in,
                     prior_mean = dat$prior.mean,
                     prior_sd = dat$prior.sd)

    if (n.classes > 1)
        stan.dat$P <- n.classes

    if (normal.covariance == "Diagonal")
        stan.dat$U <- dat$n.variables
    else if (normal.covariance == "Spherical")
        stan.dat$U <- 1

    stan.dat
}

#' @title ReduceStanFitSize
#' @description This function reduces the size of the stan.fit object to reduce the time
#' it takes to return it from the R server.
#' @param stan.fit A stanfit object.
#' @return A stanfit object with a reduced size.
#' @export
ReduceStanFitSize <- function(stan.fit)
{
    # Replace stanmodel with a dummy as stanmodel makes the output many times larger,
    # and is not required for diagnostic plots.
    dummy.stanmodel <- ""
    class(dummy.stanmodel) <- "stanmodel"
    stan.fit <- removeBeta(stan.fit)
    stan.fit@stanmodel <- dummy.stanmodel

    for (i in 1:stan.fit@sim$chains)
    {
        attr(stan.fit@sim$samples[[i]], "inits") <- NULL
        attr(stan.fit@sim$samples[[i]], "mean_pars") <- NULL
    }
    stan.fit@inits <- list()
    stan.fit@.MISC <- new.env()
    stan.fit
}

#' @title ComputeRespPars
#' @description Compute respondent parameters from a stanfit object.
#' @param stan.fit A stanfit object.
#' @param var.names Variable names
#' @param subset Subset vector
#' @param variable.scales Scale factors for numeric parameters.
#' @return A matrix of respondent parameters
#' @export
ComputeRespPars <- function(stan.fit, var.names, subset,
                            variable.scales = NULL)
{
    beta <- extract(stan.fit, pars=c("beta"))$beta
    resp.pars.subset <- colMeans(beta, dims = 1)

    if (!is.null(variable.scales))
        resp.pars.subset <- t(t(resp.pars.subset) / variable.scales)

    result <- matrix(NA, nrow = length(subset), ncol = ncol(resp.pars.subset))
    result[subset, ] <- resp.pars.subset
    colnames(result) <- var.names
    result
}

stanFileName <- function(n.classes, normal.covariance)
{
    if (n.classes == 1)
    {
        if (normal.covariance == "Full")
            result <- "choicemodel.stan"
        else
            result <- "diagonal.stan"
    }
    else
    {
        if (normal.covariance == "Full")
            result <- "mixtureofnormals.stan"
        else
            result <- "diagonalmixture.stan"
    }

    result <- file.path(system.file("stan", package = "flipChoice",
                                    mustWork = TRUE), result)

    result
}

stanModel <- function(n.classes, normal.covariance)
{
    if (n.classes == 1)
    {
        if (normal.covariance == "Full")
            mod
        else
            mod.diag
    }
    else
    {
        if (normal.covariance == "Full")
            mod.mix
        else
            mod.mix.diag
    }
}

#' @title ExtractBetaDraws
#' @description This function extracts beta draws from a stanfit object.
#' @param stan.fit A stanfit object.
#' @param max.draws Maximum draws per respondent per parameter.
#' @return A 3D array of beta draws.
#' @export
ExtractBetaDraws <- function(stan.fit, max.draws = 100)
{
    raw.betas <- extract(stan.fit, pars=c("beta"))$beta
    n.draws <- dim(raw.betas)[1]
    if (n.draws > max.draws)
    {
        fact <- floor(n.draws / max.draws)
        ind <- fact * (1:max.draws)
        raw.betas[ind, , ]
    }
    else
        raw.betas
}

#' @title IsRServer
#' @description This function indicates if it is being run on an R server.
#' @return TRUE if running on an R server. False otherwise.
#' @export
IsRServer <- function()
{
    node.name <- Sys.info()[["nodename"]]
    node.name == "reusdev" ||
        grepl("^reustest.*", node.name) ||
        grepl("^reusprod.*", node.name)
}

#' @title IsTestRServer
#' @description This function indicates if it is being run on the test R
#' server.
#' @return TRUE if running on the test R server. False otherwise.
#' @export
IsTestRServer <- function()
{
    node.name <- Sys.info()[["nodename"]]
    node.name == grepl("^reustest.*", node.name)
}

removeBeta <- function(stan.fit)
{
    nms <- stan.fit@sim$fnames_oi
    beta.nms <- nms[grepl("beta", nms)]
    non.beta.nms <- nms[!grepl("beta", nms)]
    stan.fit@sim$fnames_oi <- non.beta.nms
    stan.fit@sim$n_flatnames <- length(non.beta.nms)
    stan.fit@sim$pars_oi <- stan.fit@sim$pars_oi[stan.fit@sim$pars_oi != "beta"]
    stan.fit@sim$dims_oi$beta <- NULL
    for (i in 1:stan.fit@sim$chains)
        stan.fit@sim$samples[[i]][beta.nms] <- NULL
    stan.fit
}

#' @title GetStanWarningHandler
#' @description This function returns functions that handle Stan warnings.
#' @param show.stan.warnings Whether to return a function that shows
#' user-friendly Stan warnings.
#' @return A function that takes a warning object.
#' @export
GetStanWarningHandler <- function(show.stan.warnings)
{
    if (show.stan.warnings)
        onStanWarning
    else
        function(x) {}
}

onStanWarning <- function(warn)
{
    msg <- warn$message
    if (grepl("Increasing adapt_delta above", msg))
        warning("Results may be due inaccurate due to insufficient",
                " iteratations. Rerun the analysis with more",
                " iterations.", call. = FALSE)
    else if (grepl("Examine the pairs\\(\\) plot", msg))
        warning("Examine the Diagnostic plots to diagnose sampling problems",
                call. = FALSE)
    else if (grepl("exceeded the maximum treedepth", msg))
        warning("Results may be due inaccurate as the maximum tree depth",
                " is too low. Rerun the analysis with a higher",
                " maximum tree depth.")
    else
        warning(warn)
}

#' @title GetParameterStatistics
#' @description This function returns functions that handle Stan warnings.
#' @param stan.fit Whether to return a function that shows
#' user-friendly Stan warnings.
#' @return A matrix containing parameter summary statistics.
#' @importFrom rstan summary
#' @export
GetParameterStatistics <- function(stan.fit)
{
    rstan::summary(stan.fit, pars = c("theta", "sigma"))$summary
}
