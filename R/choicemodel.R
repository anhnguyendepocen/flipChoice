#' @title FitChoiceModel
#' @description Fit a choice-based conjoint model using methods such as
#' Hierarchical Bayes
#' @param experiment.data A data.frame from an Experiment question
#' @param cho.file The file path to a cho file.
#' @param design.file The file path to a Sawtooth design file.
#' @param attribute.levels.file A dataframe or matrix where each column
#' contains the level names of an attribute.
#' @param choices A data.frame of choices made by respondents for each
#' question.
#' @param questions A data.frame of IDs of questions presented to the
#' respondents.
#' @param n.classes The number of latent classes.
#' @param subset An optional vector specifying a subset of observations to be
#' used in the fitting process.
#' @param weights An optional vector of sampling or frequency weights.
#' @param seed Random seed.
#' @param tasks.left.out Number of questions to leave out for cross-validation.
#' @param normal.covariance The form of the covariance matrix for Hierarchical
#' Bayes. Can be 'Full, 'Spherical', 'Diagonal'.
#' @param hb.iterations The number of iterations in Hierarchical Bayes.
#' @param hb.chains The number of chains in Hierarchical Bayes.
#' @param hb.max.tree.depth http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
#' @param hb.adapt.delta http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#' @param hb.keep.samples Whether to keep the samples of all the parameters in
#' the output.
#' @param hb.stanfit Whether to include the stanfit property.
#' @param hb.prior.mean The mean for the priors of the mean parameters
#' theta_raw. This is passed as a numeric vector with each element
#' corresponding to an attribute, or a scalar. If hb.prior.mean is nonzero for
#' a categorical attribute, the attribute is treated as ordered categorical and
#' hb.prior.mean controls the offsets from the base attribute.
#' @param hb.prior.sd The standard deviations for the priors of the mean
#' parameters theta_raw. This is passed as a numeric vector with each element
#' corresponding to an attribute, or a scalar. If hb.prior.mean is nonzero for
#' a categorical attribute, the attribute is treated as ordered categorical and
#' hb.prior.sd controls the standard deviations of the offsets from the base
#' attribute.
#' @param hb.warnings Whether to show warnings from Stan.
#' @param hb.beta.draws.to.keep Maximum number of beta draws per respondent to
#' return in beta.draws.
#' @param include.choice.parameters Whether to include alternative-specific
#' parameters.
#' @param ... Additional parameters to pass on to \code{rstan::stan} and
#' \code{rstan::sampling}.
#' @return A list with the following components:
#' \itemize{
#' \item \code{respondent.parameters} A matrix containing the parameters of
#' each respondent.
#' \item \code{parameter.statistics} A matrix containing parameter statistics
#' such as effective sample size and Rhat.
#' \item \code{stan.fit} The stanfit object from the analysis.
#' \item \code{beta.draws} A 3D array containing sampling draws of beta for each
#' respondent.
#' \item \code{in.sample.accuracy} The in-sample prediction accuracy.
#' \item \code{out.sample.accuracy} The out-of-sample prediction accuracy.
#' \item \code{prediction.accuracies} A vector of prediction accuracies for
#' each respondent.
#' \item \code{algorithm} The type of algorithm used.
#' \item \code{n.questions.left.out} The number of questions left out for
#' out-of-sample testing.
#' \item \code{n.classes} The number of classes.
#' \item \code{n.respondents} The number of respondents.
#' \item \code{n.questions} The number of questions per respondent.
#' \item \code{n.choices} The number of choices per question.
#' \item \code{n.attributes} The number of attributes.
#' \item \code{n.variables} The number of variables in the analysis.
#' \item \code{time.taken} The time taken to run the analysis.
#' }
#' @examples
#' \dontrun{
#' data(eggs, package = "flipChoice")
#' fit <- FitChoiceModel(experiment.data = eggs.data, hb.iterations = 100,
#'                                               hb.chains = 1, tasks.left.out = 2)
#' ExtractParameterStats(fit)
#' PlotPosteriorIntervals(fit)
#' TracePlots(fit)
#' }
#' @importFrom flipFormat Labels
#' @export
#'
FitChoiceModel <- function(experiment.data = NULL, cho.file = NULL,
                           design.file = NULL,
                           attribute.levels.file = NULL,
                           choices = NULL, questions = NULL, n.classes = 1,
                           subset = NULL, weights = NULL, seed = 123,
                           tasks.left.out = 0, normal.covariance = "Full",
                           hb.iterations = 500, hb.chains = 8,
                           hb.max.tree.depth = 10, hb.adapt.delta = 0.8,
                           hb.keep.samples = FALSE, hb.stanfit = TRUE,
                           hb.prior.mean = 0, hb.prior.sd = 5,
                           hb.warnings = TRUE, hb.beta.draws.to.keep = 0,
                           include.choice.parameters = TRUE, ...)
{
    if (!is.null(weights))
        stop("Weights are not able to be applied for Hierarchical Bayes.")

    start.time <- proc.time()

    dat <- if (!is.null(experiment.data))
        processExperimentData(experiment.data, subset, weights,
                              tasks.left.out, seed, hb.prior.mean, hb.prior.sd)
    else if (!is.null(cho.file) && !is.null(attribute.levels.file))
        processChoFile(cho.file, attribute.levels.file,
                       subset, weights, tasks.left.out, seed,
                       hb.prior.mean, hb.prior.sd, include.choice.parameters)
    else if (!is.null(design.file) && !is.null(choices) &&
             !is.null(questions))
        processDesignFile(design.file, attribute.levels.file, choices,
                              questions, subset, weights, tasks.left.out,
                              seed, hb.prior.mean, hb.prior.sd,
                              include.choice.parameters)
    else
        stop("Insufficient data was supplied.")

    result <- hierarchicalBayesChoiceModel(dat, hb.iterations, hb.chains,
                                           hb.max.tree.depth, hb.adapt.delta,
                                           seed, hb.keep.samples, n.classes,
                                           hb.stanfit, normal.covariance,
                                           hb.warnings, hb.beta.draws.to.keep, ...)

    end.time <- proc.time()

    result <- accuracyResults(dat, result)
    result$algorithm <- "HB-Stan"
    result$n.questions.left.out <- tasks.left.out
    result$n.classes <- n.classes
    result$subset <- subset
    result$subset.description <- if (is.null(subset)) NULL else Labels(subset)
    result$weights <- weights
    result$weights.description <- if (is.null(weights)) NULL else Labels(weights)
    result$n.respondents <- dat$n.respondents
    result$n.questions <- dat$n.questions
    result$n.choices <- dat$n.choices
    result$n.attributes <- dat$n.attributes
    result$n.variables <- dat$n.variables
    result$time.taken <- (end.time - start.time)[3]
    result
}

accuracyResults <- function(dat, result)
{
    n.respondents <- dat$n.respondents
    resp.pars <- result$reduced.respondent.parameters
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

#' @title RespondentParameters
#' @description The parameters for each respondent.
#' @param object A \code{FitChoice} or \code{FitMaxDiff} object.
#' @export
RespondentParameters <- function(object)
{
    as.data.frame(object$respondent.parameters)
}

#' @title RespondentParametersTable
#' @description Produces a formattable table with histograms of respondent parameters.
#' @param resp.pars A matrix of respondent parameters
#' @param title Table title.
#' @param subtitle Table subtitle.
#' @param footer Table footer.
#' @importFrom flipFormat FormatAsReal
#' @importFrom stats sd
#' @export
RespondentParametersTable <- function(resp.pars, title, subtitle, footer)
{
    bin.max <- max(ceiling(max(resp.pars, na.rm = TRUE)), -floor(min(resp.pars, na.rm = TRUE)))
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

    footer <- paste0(footer, "Column width: ", FormatAsReal(bin.size, decimals = 2), "; ")

    HistTable(resp.pars, title = title, subtitle = subtitle, footer = footer,
              bin.size = bin.size, bin.min = bin.min, bin.max = bin.max, hist.width = 300,
              hist.height = 20, color.negative = TRUE, show.tooltips = FALSE,
              histogram.column.name = "Respondent Coefficients", stats.table)
}

#' @title print.FitChoice
#' @description Produces a string mentioning the parameters with the lowest
#' effective sample size and highest Rhat.
#' @param parameter.statistics Matrix containing parameter statistics from
#' a summary of a stan.fit object.
#' @param parameter.names Names of the parameters.
#' @param n.classes The number of classes.
#' @return A string containing information about parameter statistics.
#' @export
ParameterStatisticsInfo <- function(parameter.statistics, parameter.names,
                                    n.classes)
{
    n.rows <- nrow(parameter.statistics)
    theta.statistics <- parameter.statistics[1:(n.rows / 2), ]
    theta.n.eff.ind <- which.min(theta.statistics[, 4])
    theta.n.eff <- FormatAsReal(theta.statistics[theta.n.eff.ind, 4],
                                decimals = 1)
    theta.rhat.ind <- which.max(theta.statistics[, 5])
    theta.rhat <- FormatAsReal(theta.statistics[theta.rhat.ind, 5],
                               decimals = 2)

    sigma.statistics <- parameter.statistics[(n.rows / 2 + 1):n.rows, ]
    sigma.n.eff.ind <- which.min(sigma.statistics[, 4])
    sigma.n.eff <- FormatAsReal(sigma.statistics[sigma.n.eff.ind, 4],
                                decimals = 1)
    sigma.rhat.ind <- which.max(sigma.statistics[, 5])
    sigma.rhat <- FormatAsReal(sigma.statistics[sigma.rhat.ind, 5],
                               decimals = 2)

    if (n.classes > 1)
        nms <- rep(paste0(rep(parameter.names, each = n.classes), ", Class ",
                          1:n.classes), 2)
    else
        nms <- rep(parameter.names, 2)

    result <- ""
    if (length(theta.rhat.ind) == 0)
        result <- paste0(result, "Lowest effective sample size (Mean): ",
                         theta.n.eff, " at ", nms[theta.n.eff.ind],
                         "; Rhat (Mean) not available; ")
    else if (theta.n.eff.ind == theta.rhat.ind)
        result <- paste0(result, "Lowest effective sample size (Mean): ",
                         theta.n.eff, " and Highest Rhat (Mean): ",
                         theta.rhat, " at ", nms[theta.n.eff.ind],
                         "; ")
    else
        result <- paste0(result, "Lowest effective sample size (Mean): ",
                         theta.n.eff, " at ", nms[theta.n.eff.ind],
                         "; ", "Highest Rhat (Mean): ", theta.rhat, " at ",
                         nms[theta.rhat.ind], "; ")

    if (length(sigma.rhat.ind) == 0)
        result <- paste0(result, "Lowest effective sample size (St. Dev.): ",
                         sigma.n.eff, " at ", nms[sigma.n.eff.ind],
                         "; Rhat (St. Dev.) not available; ")
    else if (sigma.n.eff.ind == sigma.rhat.ind)
        result <- paste0(result, "Lowest effective sample size (St. Dev.): ",
                         sigma.n.eff, " and Highest Rhat (St. Dev.): ",
                         sigma.rhat, " at ", nms[sigma.n.eff.ind],
                         "; ")
    else
        result <- paste0(result, "Lowest effective sample size (St. Dev.): ",
                         sigma.n.eff, " at ", nms[sigma.n.eff.ind],
                         "; ", "Highest Rhat (St. Dev.): ", sigma.rhat, " at ",
                         nms[sigma.rhat.ind], "; ")
    result
}

#' @title print.FitChoice
#' @description Print a FitChoice object
#' @param x FitMaxDiff object.
#' @param ... further arguments passed to or from other methods.
#' @importFrom flipFormat HistTable FormatAsPercent SampleDescription
#' @importFrom flipTime FormatPeriod
#' @export
#' @method print FitChoice
print.FitChoice <- function(x, ...)
{
    title <- "Choice Model: Hierarchical Bayes"

    n.subset <- if (is.null(x$subset)) x$n.respondents else length(x$subset)
    footer <- SampleDescription(n.total = x$n.respondents, n.subset = n.subset,
                                n.estimation = n.subset,
                                subset.label = x$subset.description,
                                weighted = !is.null(x$weights),
                                weight.label = x$weights.description,
                                missing = FALSE)
    footer <- paste0(footer, " ")

    footer <- paste0(footer, "Number of questions: ", x$n.questions, "; ")
    if (x$n.questions.left.out > 0)
    {
        footer <- paste0(footer, "Questions used in estimation: ", x$n.questions - x$n.questions.left.out, "; ")
        footer <- paste0(footer, "Questions left out: ", x$n.questions.left.out, "; ")
    }
    footer <- paste0(footer, "Choices per question: ", x$n.choices, "; ")
    footer <- paste0(footer, "Number of attributes: ", x$n.attributes, "; ")
    footer <- paste0(footer, "Number of variables: ", x$n.variables, "; ")
    footer <- paste0(footer, "Number of classes: ", x$n.classes, "; ")
    if (x$class.match.fail)
        footer <- paste0(footer, "Parameter statistics not available; ")
    else
        footer <- paste0(footer,
                         ParameterStatisticsInfo(x$parameter.statistics,
                                     colnames(x$reduced.respondent.parameters),
                                     x$n.classes))
    if (IsTestRServer())
        footer <- paste0(footer, "Time taken to run analysis: [hidden for tests]; ")
    else
        footer <- paste0(footer, "Time taken to run analysis: ",
                         FormatPeriod(x$time.taken), "; ")

    subtitle <- if (!is.na(x$out.sample.accuracy))
        paste0("Prediction accuracy (leave-", x$n.questions.left.out , "-out cross-validation): ",
               FormatAsPercent(x$out.sample.accuracy, decimals = 1))
    else
        paste0("Prediction accuracy (in-sample): ",
               FormatAsPercent(x$in.sample.accuracy, decimals = 1))

    RespondentParametersTable(x$respondent.parameters, title, subtitle, footer)
}
