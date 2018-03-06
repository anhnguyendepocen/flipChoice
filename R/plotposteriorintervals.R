#' Plot Posterior Intervals From a Choice Model Fit With Hierarchical Bayes
#'
#' Takes a Hierarchical Bayes (Stan) output and produces
#' posterior intervals of the mean and standard deviation parameters
#' for the distribution from which individual coefficients are
#' sampled.
#' @param fit An object of class \code{"FitChoice"} produced by a call
#'     to \code{FitChoiceModel} with \code{algorithm = "HB-Stan"}.
#' @return A \code{ggplot} object that can be further customized using the
#' \code{ggplot2} package.
#' @family HB diagnostics
#' @seealso \url{https://www.displayr.com/convergence-hb-maxdiff/},
#' \code{\link{FitChoiceModel}} for an example, \code{\link{ExtractParameterStats}},
#' \code{\link{TracePlots}}
#' @importFrom rstan plot
#' @importFrom ggplot2 scale_y_discrete
#' @export
PlotPosteriorIntervals <- function(fit)
{
    checkValidFit(fit)

    is.multi.class <- fit$n.classes > 1L
    if (is.multi.class && fit$class.match.fail)
        stop("Posterior intervals are not available as classes from ",
             "different chains could not be matched.")

    p <- rstan::plot(fit$stan.fit, pars = c('theta', 'sigma'))
    lbls <- makeLabels(fit, FALSE)
    p$plot_env <- new.env()
    p + ggplot2::scale_y_discrete(labels=lbls, limits=length(lbls):1)
}
