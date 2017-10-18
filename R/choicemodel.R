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
#' @param hb.stanfit Whether to include the stanfit property.
#' @export
FitChoiceModel <- function(experiment.data, n.classes = 1, subset = NULL, weights = NULL, seed = 123,
                           tasks.left.out = 0, hb.iterations = 500, hb.chains = 8, hb.max.tree.depth = 10,
                           hb.adapt.delta = 0.8, hb.keep.samples = FALSE, hb.stanfit = TRUE)
{
    dat <- processExperimentData(experiment.data, subset, weights, tasks.left.out, seed)
    result <- hierarchicalBayesChoiceModel(dat, hb.iterations, hb.chains, hb.max.tree.depth,
                                           hb.adapt.delta, seed, hb.keep.samples, n.classes,
                                           hb.stanfit)
    result <- accuracyResults(dat, result)
    result$algorithm <- "HB-Stan"
    result$n.questions.left.out <- tasks.left.out
    result$is.hb <- TRUE
    result$n.classes <- n.classes
    result$subset <- subset
    result$weights <- weights
    result$n.respondents <- dat$n.respondents
    result$n.questions <- dat$n.questions
    result$n.choices <- dat$n.choices
    result$n.attributes <- dat$n.attributes
    result$n.variables <- dat$n.variables
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

#' \code{RespondentParametersTable}
#' @description Produces a formattable table with histograms of respondent parameters.
#' @param resp.pars A matrix of respondent parameters
#' @param title Table title.
#' @param subtitle Table subtitle.
#' @param footer Table footer.
#' @importFrom flipFormat FormatAsReal
#' @export
RespondentParametersTable <- function(resp.pars, title, subtitle, footer)
{
    bin.max <- max(ceiling(max(resp.pars)), -floor(min(resp.pars)))
    bin.min <- -bin.max

    n.variables <- ncol(resp.pars)
    stats.table <- matrix(NA, nrow = n.variables, ncol = 2)
    for (i in 1:n.variables)
    {
        stats.table[i, 1] <- FormatAsReal(mean(resp.pars[, i], na.rm = TRUE), decimals = 1)
        stats.table[i, 2] <- FormatAsReal(sd(resp.pars[, i], na.rm = TRUE), decimals = 1)
    }
    colnames(stats.table) <- c("Mean", "Standard Deviation")

    bin.size <- (bin.max - bin.min) / 50

    footer <- paste0(footer, "Bar width: ", FormatAsReal(bin.size, decimals = 2), "; ")

    HistTable(resp.pars, title = title, subtitle = subtitle, footer = footer,
              bin.size = bin.size, bin.min = bin.min, bin.max = bin.max, hist.width = 300,
              hist.height = 20, color.negative = TRUE, show.tooltips = FALSE,
              histogram.column.name = "Respondent Parameters", stats.table)
}

#' @title print.FitChoice
#' @description Print a FitChoice object
#' @param x FitMaxDiff object.
#' @param ... further arguments passed to or from other methods.
#' @importFrom flipFormat HistTable FormatAsPercent FormatAsReal
#' @export
print.FitChoice <- function(x, ...)
{
    title <- "Choice Model: Hierarchical Bayes"
    footer <- paste0("n = ", x$n.respondents, "; ")
    if (!is.null(x$subset) && !all(x$subset))
        footer <- paste0(footer, "Filters have been applied; ")
    footer <- paste0(footer, "Number of questions: ", x$n.questions, "; ")
    if (x$n.questions.left.out > 0)
    {
        footer <- paste0(footer, "Questions used in estimation: ", x$n.questions - x$n.questions.left.out, "; ")
        footer <- paste0(footer, "Questions left out: ", x$n.questions.left.out, "; ")
    }
    footer <- paste0(footer, "Choices per question: ", x$n.choices, "; ")
    footer <- paste0(footer, "Number of attributes: ", x$n.attributes, "; ")
    footer <- paste0(footer, "Number of variables: ", x$n.variables, "; ")
    footer <- paste0(footer, "Blue and red bars indicate positive and negative parameters respectively; ")

    subtitle <- if (!is.na(x$out.sample.accuracy))
        paste0("Prediction accuracy (leave-", x$n.questions.left.out , "-out cross-validation): ",
               FormatAsPercent(x$out.sample.accuracy, 3))
    else
        paste0("Prediction accuracy (in-sample): ", FormatAsPercent(x$in.sample.accuracy, 3))

    RespondentParametersTable(x$respondent.parameters, title, subtitle, footer)
}
