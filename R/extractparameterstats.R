#' Extract Parameter Statistics from a Choice Model Fit Using Hierarchical Bayes
#'
#' Takes a Hierarchical Bayes (Stan) output and produces sample
#' statistics of the mean and standard deviation parameters for the
#' distribution from which individual coefficients are sampled.
#' @param fit An object of class \code{"FitChoice"} produced by a call
#'     to \code{FitChoiceModel} with \code{algorithm = "HB-Stan"}.
#' @return A matrix containing a summary of the parameter samples from
#'     the MCMC results stored in \code{fit}, including mean, standard
#'     error, effective sample size, and rhat for each mean and
#'     standard deviation parameter.
#' @importFrom rstan extract monitor
#' @family HB diagnostics
#' @seealso \code{\link[rstan]{monitor}}, \url{https://www.displayr.com/convergence-hb-maxdiff/},
#'     \code{\link{FitChoiceModel}} for an example, \code{\link{TracePlots}},
#' \code{\link{PlotPosteriorIntervals}}
#' @export
ExtractParameterStats <- function(fit)
{
    checkValidFit(fit)

    n.classes <- fit$n.classes
    is.multi.class <- n.classes > 1L
    if (is.multi.class && fit$class.match.fail)
        stop("Parameter statistics are not available as classes from ",
             "different chains could not be matched.")

     if (is.multi.class && ('class_weights' %in% fit$stan.fit@sim$pars_oi))
         ex <- rstan::extract(fit$stan.fit,
                              pars = c('class_weights', 'theta', 'sigma'),
                              permuted = FALSE, inc_warmup = FALSE)
     else
         ex <- rstan::extract(fit$stan.fit, pars = c('theta', 'sigma'),
                              permuted = FALSE, inc_warmup = FALSE)
     sample.stats <- suppressWarnings(rstan::monitor(ex, probs = c()))
    rownames(sample.stats) <- makeLabels(fit, TRUE)
    sample.stats
}

checkValidFit <- function(f)
{
    if (!inherits(f, "FitChoice") || is.null(f$stan.fit))
        stop("The selected output was not a choice model output computed using ",
             "Hierarchical Bayes (Stan). Please select such an output before running this script.")
}

makeLabels <- function(fit, add.weight.labels = FALSE)
{
    n.classes <- fit$n.classes
    nms <- if (!is.null(fit$reduced.respondent.parameters))
                    colnames(fit$reduced.respondent.parameters)
                 else
                    colnames(fit$respondent.parameters)
     lbls <- c(rep(paste0(nms, ' (Mean)'), each = n.classes),
              rep(paste0(nms, ' (St. Dev.)'), each = n.classes))

    if (n.classes > 1L)
        lbls <- paste0(lbls, rep(paste0(', Class ', 1:n.classes), 2 * length(nms)))
    if (add.weight.labels && n.classes > 1L)
        if ('class_weights' %in% fit$stan.fit@sim$pars_oi)
            lbls <- c(paste0('Class ', 1:n.classes, ' size') , lbls)
    lbls
}
