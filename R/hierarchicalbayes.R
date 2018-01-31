#' @importFrom rstan rstan_options stan extract sampling
#' @importFrom flipU InterceptExceptions
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
    on.error <- GetStanErrorHandler()

    InterceptExceptions({
        stan.fit <- RunStanSampling(stan.dat, n.iterations, n.chains,
                                    max.tree.depth, adapt.delta, seed,
                                    stan.model, stan.file, ...)},
                                    warning.handler = on.warnings,
                                    error.handler = on.error)

    matched <- MatchChainClasses(stan.fit, n.chains, n.classes, stan.dat$V)
    stan.fit <- matched$stan.fit
    class.match.fail <- matched$match.fail

    result <- list()
    result$respondent.parameters <- ComputeRespPars(stan.fit, dat$var.names,
                                                    dat$subset,
                                                    dat$variable.scales)
    result$class.match.fail <- class.match.fail
    if (!class.match.fail)
        result$parameter.statistics <- GetParameterStatistics(stan.fit,
                                                              dat$var.names,
                                                              n.classes)
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
    pars <- stanParameters(stan.dat)
    init <- initialParameterValues(stan.dat)

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
                                          adapt_delta = adapt.delta),
                           init = init, ...)
    }
    else # Not R servers
    {
        result <- stan(file = stan.file, data = stan.dat, iter = n.iterations,
                       chains = n.chains, seed = seed, pars = pars,
                       control = list(max_treedepth = max.tree.depth,
                                      adapt_delta = adapt.delta),
                       init = init, ...)
    }
    result
}

stanParameters <- function(stan.dat)
{
    full.covariance <- is.null(stan.dat$U)
    multiple.classes <- !is.null(stan.dat$P)

    pars <- c("theta", "sigma", "beta")
    if (full.covariance)
        pars <- c(pars, "L_omega")
    if (multiple.classes)
        pars <- c(pars, "class_weights")

    pars
}

initialParameterValues <- function(stan.dat)
{
    full.covariance <- is.null(stan.dat$U)
    multiple.classes <- !is.null(stan.dat$P)

    init <- function () structure(list(), .Names = character(0))
    if (full.covariance)
    {
        n.pars <- if (!is.null(stan.dat$K))
            stan.dat$K
        else
            stan.dat$V

        if (multiple.classes)
        {
            n.classes <- stan.dat$P
            init <- function () {
                L_omega <- array(NA, dim = c(n.classes, n.pars, n.pars))
                for (i in 1:n.classes)
                    L_omega[i, , ] <- diag(n.pars)
                list(L_omega = L_omega)
            }
        }
        else
            init <- function () list(L_omega = diag(n.pars))
    }
    init
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
    grepl("^reustest.*", node.name)
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
#' @description This function returns a function that handles Stan warnings.
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

#' @title GetStanErrorHandler
#' @description This function returns a function that handles Stan errors.
#' @return A function that takes an error object.
#' @export
GetStanErrorHandler <- function()
{
    function(error)
    {
        msg <- error$message
        if (grepl("missing value where", msg) ||
            grepl("unable to fork", msg))
        {
            stop("The R server has reached maximum capacity. ",
                 "Please rerun the calculation later or contact ",
                 "support@q-researchsoftware.com for assistance.")
        }
        else
            stop(msg)
    }
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
#' @param stan.fit A stanfit object.
#' @param parameter.names Names of the parameters.
#' @param n.classes The number of classes.
#' @return A matrix containing parameter summary statistics.
#' @importFrom rstan summary
#' @export
GetParameterStatistics <- function(stan.fit, parameter.names, n.classes)
{
    pars <- c('theta', 'sigma')

    ex <- rstan::extract(stan.fit, pars = pars, permuted = FALSE,
                         inc_warmup = TRUE)
    result <- suppressWarnings(rstan::monitor(ex, probs = c(), print = FALSE))
    lbls <- c(rep(paste0('Mean (', parameter.names, ')'), each = n.classes),
              rep(paste0('St. Dev. (', parameter.names, ')'),
                  each = n.classes))
    if (n.classes > 1)
        lbls <- paste0(lbls, rep(paste0(', Class ', 1:n.classes),
                                 2 * length(parameter.names)))
    row.names(result) <- lbls
    result
}

#' @title MatchChainClasses
#' @description This function attempts to match classes generated by different
#' chains in a stanfit object.
#' @param stan.fit A stanfit object.
#' @param n.chains The number of chains in the analysis.
#' @param n.classes The number of classes in the analysis.
#' @param n.variables The number of variables in the analysis.
#' @return A stanfit object with classes reordered if matching is successful.
#' Otherwise a warning is thrown and the original stanfit object is returned.
#' @export
MatchChainClasses <- function(stan.fit, n.chains, n.classes, n.variables)
{
    if (n.classes == 1 || n.chains == 1)
        result <- list(stan.fit = stan.fit, match.fail = FALSE)
    else
    {
        samples <- stan.fit@sim$samples
        means <- computeThetaMeans(samples, n.classes, n.variables)
        match.fail <- FALSE
        for (i in 2:n.chains)
        {
            mapping <- rep(NA, n.classes)
            for (j in 1:n.classes)
            {
                norms <- rep(NA, n.classes)
                for (k in 1:n.classes)
                    norms[k] <- parameterDistance(means[, j, 1], means[, k, i])
                ind <- which.min(norms)
                if (norms[ind] > 0.5)
                {
                    match.fail <- TRUE
                    break
                }
                mapping[j] <- ind
            }
            if (!match.fail && length(unique(mapping)) < length(mapping))
                match.fail <- TRUE
            if (match.fail)
                break

            samples[[i]] <- permuteClasses(samples[[i]], mapping, n.variables)
        }

        if (!match.fail)
            stan.fit@sim$samples <- samples
        else
            warning("Classes could not be matched between chains. ",
                    "Parameter statistics will not be available.")

        result <- list(stan.fit = stan.fit, match.fail = match.fail)
    }
    result
}

permuteClasses <- function(chain.samples, mapping, n.variables)
{
    n.classes <- length(mapping)
    result <- chain.samples
    for (j in 1:n.classes)
    {
        for (k in 1:n.variables)
        {
            theta.previous <- paste0("theta[", mapping[j], ",", k, "]")
            theta.new <- paste0("theta[", j, ",", k, "]")
            result[[theta.new]] <- chain.samples[[theta.previous]]

            sigma.previous <- paste0("sigma[", mapping[j], ",", k, "]")
            sigma.new <- paste0("sigma[", j, ",", k, "]")
            result[[sigma.new]] <- chain.samples[[sigma.previous]]
        }
    }
    result
}

# 2 * |p1-p2|^2/(|p1|^2 + |p2|^2) where |*| is the Euclidean norm.
parameterDistance <- function(p1, p2)
{
    2 * sum((p1 - p2) * (p1 - p2)) / (sum(p1 * p1) + sum(p2 * p2))
}

computeThetaMeans <- function(samples, n.classes, n.variables)
{
    n.chains <- length(samples)
    means <- array(NA, dim = c(n.variables, n.classes, n.chains))
    for (i in 1:n.chains)
    {
        for (j in 1:n.classes)
        {
            for (k in 1:n.variables)
            {
                parameter.name <- paste0("theta[", j, ",", k, "]")
                parameter.samples <- samples[[i]][[parameter.name]]
                means[k, j, i] <- mean(parameter.samples)
            }
        }
    }
    means
}
