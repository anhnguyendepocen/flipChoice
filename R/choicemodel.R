#' @title FitChoiceModel
#' @description Fit a choice-based conjoint model using methods such as Hierarchical Bayes
#' @param experiment.data A data.frame from an Experiment question
#' @param n.classes The number of latent classes.
#' @param subset An optional vector specifying a subset of observations to be used in the fitting process.
#' @param weights An optional vector of sampling or frequency weights.
#' @param seed Random seed.
#' @param tasks.left.out Number of questions to leave out for cross-validation.
#' @param hb.iterations The number of iterations in Hierarchical Bayes.
#' @param hb.chains The number of chains in Hierarchical Bayes.
#' @param hb.max.tree.depth http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
#' @param hb.adapt.delta http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#' @param hb.keep.samples Whether to keep the samples of all the parameters in the output.
#' @export
FitChoiceModel <- function(experiment.data, n.classes = 1, subset = NULL, weights = NULL, seed = 123,
                           tasks.left.out = 0, hb.iterations = 500, hb.chains = 8, hb.max.tree.depth = 10,
                           hb.adapt.delta = 0.8, hb.keep.samples = FALSE)
{
    dat <- processExperimentData(experiment.data, subset, weights, tasks.left.out, seed)
    result <- hierarchicalBayesChoiceModel(dat, hb.iterations, hb.chains, hb.max.tree.depth,
                                           hb.adapt.delta, seed, hb.keep.samples, n.classes)
    result <- accuracyResults(dat, result)
    result$algorithm <- "HB-Stan"
    result$n.questions.left.out <- tasks.left.out
    result$is.hb <- TRUE
    result$n.classes <- n.classes
    result$subset <- subset
    result$weights <- weights
    result$output <- "Probabilities"
    resp.pars <- result$respondent.parameters[dat$subset, ]
    result$respondent.probabilities <- exp(resp.pars) / rowSums(exp(resp.pars))
    result$is.choice.model <- TRUE
    result
}

accuracyResults <- function(dat, result)
{
    n.respondents <- dat$n.respondents
    resp.pars <- result$respondent.parameters
    in.sample.accuracies <- predictionAccuracies(resp.pars, dat$X.in, dat$Y.in, dat$subset)
    w <- dat$weights
    result$in.sample.accuracy <- sum(in.sample.accuracies * w) / sum(w)
    if (dat$n.questions.left.out > 0)
    {
        result$prediction.accuracies <- predictionAccuracies(resp.pars, dat$X.out, dat$Y.out,
                                                                   dat$subset)
        result$out.sample.accuracy <- sum(result$prediction.accuracies * w) / sum(w)
    }
    else
    {
        result$prediction.accuracies <- in.sample.accuracies
        result$out.sample.accuracy <- NA
    }
    result
}

predictionAccuracies <- function(resp.pars, X, Y, subset)
{
    n.respondents <- dim(X)[1]
    n.questions <- dim(X)[2]
    n.alternatives <- dim(X)[3]
    resp.pars <- resp.pars[subset, ]
    result <- rep(NA, n.respondents)
    for (r in 1:n.respondents)
    {
        score <- rep(NA, n.questions)
        for (j in 1:n.questions)
        {
            u <- rep(NA, n.alternatives)
            for (k in 1:n.alternatives)
                u[k] <- sum(resp.pars[r, ] * X[r, j, k, ])
            score[j] <- if(which.max(u) == Y[r, j]) 1 else 0
        }
        result[r] <- mean(score)
    }
    result
}

#' \code{RespondentParameters}
#' @description The parameters for each respondent.
#' @param object A \code{FitChoice} or \code{FitMaxDiff} object.
#' @export
RespondentParameters <- function(object)
{
    as.data.frame(object$respondent.parameters)
}

#' @title print.FitChoice
#' @description Print a FitChoice object
#' @param x FitMaxDiff object.
#' @param ... further arguments passed to or from other methods.
#' @importFrom flipFormat HistTable MaxDiffTableClasses FormatAsPercent FormatAsReal
#' @export
print.FitChoice <- function(x, ...)
{
    title <- "Choice Model: Hierarchical Bayes"
    footer <- paste0("n = ", x$n.respondents, "; ")
    if (!is.null(x$subset) && !all(x$subset))
        footer <- paste0(footer, "Filters have been applied; ")
    if (!is.null(x$weights))
        footer <- paste0(footer, "Weights have been applied; Effective sample size: ",
                         FormatAsReal(x$effective.sample.size, decimals = 2), "; ")
    footer <- paste0(footer, "Number of questions: ", x$n.questions, "; ")
    if (x$n.questions.left.out > 0)
    {
        footer <- paste0(footer, "Questions used in estimation: ", x$n.questions - x$n.questions.left.out, "; ")
        footer <- paste0(footer, "Questions left out: ", x$n.questions.left.out, "; ")
    }
    footer <- paste0(footer, "Alternatives per question: ", x$n.alternatives.per.task, "; ")

    subtitle <- if (!is.na(x$out.sample.accuracy))
        paste0("Prediction accuracy (leave-", x$n.questions.left.out , "-out cross-validation): ",
               FormatAsPercent(x$out.sample.accuracy, 3))
    else
        paste0("Prediction accuracy (in-sample): ", FormatAsPercent(x$in.sample.accuracy, 3))

    probs <- x$respondent.probabilities
    stats.table <- matrix(NA, nrow = ncol(probs), ncol = 1)
    for (i in 1:ncol(probs))
        stats.table[i, 1] <- FormatAsReal(mean(probs[, i], na.rm = TRUE) * 100, decimals = 1)
    colnames(stats.table) <- "Mean Probability (%)"

    HistTable(100 * probs, title = title, subtitle = subtitle, footer = footer,
              bin.size = 5, bin.min = 0, bin.max = 100, hist.width = 100, hist.height = 20, stats.table)
}
