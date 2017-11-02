#' @importFrom rstan rstan_options stan extract sampling
hierarchicalBayesChoiceModel <- function(dat, n.iterations = 500, n.chains = 8,
                                         max.tree.depth = 10,
                                         adapt.delta = 0.8, seed = 123,
                                         keep.samples = FALSE, n.classes = 1,
                                         include.stanfit = TRUE,
                                         normal.covariance = "Full",
                                         prior.sd = NULL,
                                         stan.warnings = TRUE,
                                         max.draws = 100)
{
    # We want to replace this call with a proper integration of rstan into this package
    require(rstan)

    # allows Stan chains to run in parallel on multiprocessor machines
    options(mc.cores = parallel::detectCores())

    stan.dat <- createStanData(dat, n.classes, normal.covariance, prior.sd)

    if (stan.warnings)
        stan.fit <- runStanSampling(stan.dat, n.classes, n.iterations,
                                    n.chains, normal.covariance,
                                    max.tree.depth, adapt.delta, seed)
    else
        suppressWarnings(stan.fit <- runStanSampling(stan.dat, n.classes,
                                                     n.iterations, n.chains,
                                                     normal.covariance,
                                                     max.tree.depth,
                                                     adapt.delta, seed))

    result <- list()
    result$respondent.parameters <- ComputeRespPars(stan.fit, dat$var.names, dat$subset,
                                                    dat$variable.scales)
    if (include.stanfit)
    {
        result$stan.fit <- if (keep.samples) stan.fit else ReduceStanFitSize(stan.fit)
        result$beta.draws <- ExtractBetaDraws(stan.fit, max.draws)
    }
    class(result) <- "FitChoice"
    result
}

runStanSampling <- function(stan.dat, n.classes, n.iterations, n.chains,
                            normal.covariance, max.tree.depth, adapt.delta,
                            seed)
{
    if (IsRServer()) # R servers
    {
        # Loads a precompiled stan model called mod from sysdata.rda to avoid recompiling.
        # The R code used to generate mod on a linux machine is:
        # mod <- rstan::stan_model(model_code = model.code)
        # devtools::use_data(mod, internal = TRUE, overwrite = TRUE)
        # where model.code is the stan code as a string.
        # Ideally we would want to recompile when the package is built (similar to Rcpp)
        m <- stanModel(n.classes, normal.covariance)
        result <- sampling(m, data = stan.dat, chains = n.chains,
                             iter = n.iterations, seed = seed,
                             control = list(max_treedepth = max.tree.depth,
                                            adapt_delta = adapt.delta))
    }
    else # Not R servers
    {
        stan.file <- stanFileName(n.classes, normal.covariance)
        result <- stan(file = stan.file, data = stan.dat, iter = n.iterations,
                         chains = n.chains, seed = seed,
                         control = list(max_treedepth = max.tree.depth, adapt_delta = adapt.delta))
    }
    result
}

createStanData <- function(dat, n.classes, normal.covariance, prior.sd)
{
    stan.dat <- list(C = dat$n.choices,
                     R = dat$n.respondents,
                     S = dat$n.questions.left.in,
                     A = dat$n.attributes,
                     V = dat$n.variables,
                     V_raw = dat$n.raw.variables,
                     V_attribute = dat$n.attribute.variables,
                     Y = dat$Y.in,
                     X = dat$X.in)

    if (n.classes > 1)
        stan.dat$P <- n.classes

    if (normal.covariance == "Diagonal")
        stan.dat$U <- dat$n.variables
    else if (normal.covariance == "Spherical")
        stan.dat$U <- 1

    if (is.null(prior.sd))
        stan.dat$prior_sd <- rep(2, dat$n.raw.variables) # default prior mean parameter SD
    else if (!is.numeric(prior.sd) || length(prior.sd) != dat$n.raw.variables)
        stop("The supplied parameter prior.sd is inappropriate.")
    else
    {
        stan.dat$prior_sd <- prior.sd

        # Need to scale the prior SD of numeric variables
        n.attribute.variables <- dat$n.attribute.variables
        n.attributes <- dat$n.attributes
        n.attribute.raw.variables <- pmax(dat$n.attribute.variables - 1, 1)
        for (i in 1:n.attributes)
        {
            if (n.attribute.variables[i] == 1)
            {
                index.all <- sum(n.attribute.variables[1:i])
                index.raw <- sum(n.attribute.raw.variables[1:i])
                stan.dat$prior_sd[index.raw] <- prior.sd[index.raw] *
                                                dat$variable.scales[index.all]
            }
        }
    }

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
    stan.fit@stanmodel <- dummy.stanmodel

    # Set samples to zero to save space
    nms <- names(stan.fit@sim$samples[[1]])
    re <- paste(c("XB", "beta", "standard_normal", "theta_raw", "sigma_unif", "L_omega", "L_sigma",
                  "posterior_prob", "class_weights"), collapse = "|")
    nms <- nms[grepl(re, nms)]
    for (i in 1:stan.fit@sim$chains)
    {
        for (nm in nms)
            stan.fit@sim$samples[[i]][[nm]] <- 0
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
    if (length(dim(beta)) == 4) # n.classes > 1
    {
        n.respondents <- dim(beta)[2]
        n.variables <- dim(beta)[4]
        resp.pars.subset <- matrix(NA, n.respondents, n.variables)
        pp <- exp(extract(stan.fit, pars=c("posterior_prob"))$posterior_prob)
        for (i in 1:n.respondents)
            pp[, i, ] <- pp[, i, ] / rowSums(pp[, i, ])

        for (j in 1:n.variables)
            for (i in 1:n.respondents)
                resp.pars.subset[i, j] <- mean(beta[, i, , j] * pp[, i, ])
            resp.pars.subset
    }
    else
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
            result <- "exec/choicemodel.stan"
        else
            result <- "exec/diagonal.stan"
    }
    else
    {
        if (normal.covariance == "Full")
            result <- "exec/mixtureofnormals.stan"
        else
            result <- "exec/diagonalmixture.stan"
    }

    if (!dir.exists("exec")) # not unit testing
        result <- paste0("tests/testthat/", result)

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
