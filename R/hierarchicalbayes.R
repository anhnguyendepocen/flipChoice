#' @importFrom rstan rstan_options stan extract sampling
hierarchicalBayesChoiceModel <- function(dat, n.iterations = 500, n.chains = 8, max.tree.depth = 10,
                                         adapt.delta = 0.8, seed = 123, keep.samples = FALSE,
                                         n.classes = 1)
{
    # We want to replace this call with a proper integration of rstan into this package
    require(rstan)

    # allows Stan chains to run in parallel on multiprocessor machines
    options(mc.cores = parallel::detectCores())

    stan.dat <- list(C = dat$n.choices,
                     R = dat$n.respondents,
                     S = dat$n.questions.left.in,
                     A = dat$n.attributes,
                     V = dat$n.variables,
                     V_raw = dat$n.raw.variables,
                     V_attribute = dat$n.attribute.variables,
                     Y = dat$Y.in,
                     X = dat$X.in)

    if (.Platform$OS.type == "unix")
    {
        # Loads a precompiled stan model called mod from sysdata.rda to avoid recompiling.
        # The R code used to generate mod on a linux machine is:
        # mod <- rstan::stan_model(model_code = model.code)
        # devtools::use_data(mod, internal = TRUE, overwrite = TRUE)
        # where model.code is the stan code as a string.
        # Ideally we would want to recompile when the package is built (similar to Rcpp)
        stan.fit <- sampling(mod, data = stan.dat, chains = n.chains, iter = n.iterations, seed = seed,
                             control = list(max_treedepth = max.tree.depth, adapt_delta = adapt.delta))
    }
    else # windows
    {
        stan.file <- "exec/choicemodel.stan"
        stan.fit <- stan(file = stan.file, data = stan.dat, iter = n.iterations,
                         chains = n.chains, seed = seed,
                         control = list(max_treedepth = max.tree.depth, adapt_delta = adapt.delta))
    }

    result <- list()
    result$respondent.parameters <- computeRespPars(stan.fit, dat)
    result$stan.fit <- stan.fit
    class(result) <- "FitChoice"
    result
}

computeRespPars <- function(stan.fit, dat)
{
    subset <- dat$subset

    beta <- extract(stan.fit, pars=c("beta"))$beta
    resp.pars.subset <- colMeans(beta, dims = 1)

    result <- matrix(NA, nrow = length(subset), ncol = ncol(resp.pars.subset))
    result[subset, ] <- resp.pars.subset
    colnames(result) <- dat$var.names
    result
}
