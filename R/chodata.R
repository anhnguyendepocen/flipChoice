processChoFile <- function(cho.file, attribute.levels.file,
                           subset, weights, n.questions.left.out, seed,
                           input.prior.mean, input.prior.sd,
                           include.choice.parameters)
{
    raw.lines <- readLines(cho.file)
    attribute.levels <- processAttributeLevelsFile(attribute.levels.file)

    raw.num <- lapply(strsplit(raw.lines, " "), as.numeric)
    n.attributes <- raw.num[[1]][3]
    n.questions <- raw.num[[1]][4]
    has.none.of.these <- raw.num[[1]][5] == 1
    n.choices <- raw.num[[3]][1]
    if (has.none.of.these)
        n.choices <- n.choices + 1

    n.raw <- length(raw.num)
    n.respondents <- n.raw / (n.questions * (raw.num[[3]][1] + 2) + 2)

    if (floor(n.respondents) != n.respondents)
        stop("There is a problem with the .CHO file. Ensure each respondent ",
             "has the same number of questions and choices per question.")

    n.attributes <- length(attribute.levels)
    n.attribute.parameters <- unlist(lapply(attribute.levels, length)) - 1
    n.parameters <-  sum(n.attribute.parameters)
    par.names <- parameterNamesFromAttributes(attribute.levels)
    all.names <- allNamesFromAttributes(attribute.levels)

    checkPriorParameters(input.prior.mean, input.prior.sd, n.choices,
                         n.attributes, n.parameters, include.choice.parameters)

    ordered.attributes <- orderedAttributes(input.prior.mean, n.attributes,
                                            n.parameters)

    n.questions.left.in <- n.questions - n.questions.left.out
    left.out <- LeftOutQuestions(n.respondents, n.questions, n.questions.left.out, seed)

    X <- array(data = 0, dim = c(n.respondents, n.questions, n.choices, n.parameters))
    Y <- matrix(NA, nrow = n.respondents, ncol = n.questions)

    ind <- 0
    for (i in 1:n.respondents)
    {
        ind <- ind + 2 # first two rows per respondent
        for (j in 1:n.questions)
        {
            ind <- ind + 1 # question format row
            for (k in 1:n.choices)
            {
                if (has.none.of.these && k == n.choices)
                    X[i, j, k, ] <- fillXNoneOfThese(n.parameters,
                                                     n.attributes,
                                                     n.attribute.parameters)
                else
                {
                    ind <- ind + 1 # attributes row
                    X[i, j, k, ] <- fillXAttributes(n.parameters,
                                                    n.attributes,
                                                    n.attribute.parameters,
                                                    ordered.attributes,
                                                    raw.num[[ind]])
                }
            }
            ind <- ind + 1 # choice row
            Y[i, j] <- raw.num[[ind]][1]
        }
    }

    if (include.choice.parameters)
    {
        output <- addChoiceParameters(X, n.attributes, n.parameters,
                                      n.attribute.parameters, n.choices,
                                      par.names, all.names, has.none.of.these)
        X <- output$X
        n.attributes <- output$n.attributes
        n.parameters <- output$n.parameters
        n.attribute.parameters <- output$n.attribute.parameters
        par.names <- output$par.names
        all.names <- output$all.names
    }

    subset <- CleanSubset(subset, n.respondents)
    weights <- prepareWeights(weights, subset)
    X <- X[subset, , , ]
    Y <- Y[subset, ]
    n.respondents <- sum(subset)

    split.data <- crossValidationSplit(X, Y, n.questions.left.out, seed)

    prior.mean <- processInputPrior(input.prior.mean, n.parameters,
                                    n.attributes, n.attribute.parameters)
    prior.sd <- processInputPrior(input.prior.sd, n.parameters, n.attributes,
                                  n.attribute.parameters)

    result <- list(n.questions = n.questions,
                   n.questions.left.in = n.questions.left.in,
                   n.questions.left.out = n.questions.left.out,
                   n.choices = n.choices,
                   n.attributes = n.attributes,
                   n.respondents = n.respondents,
                   n.parameters = n.parameters,
                   n.attribute.parameters = n.attribute.parameters,
                   par.names = par.names,
                   all.names = all.names,
                   X.in = split.data$X.in,
                   Y.in = split.data$Y.in,
                   X.out = split.data$X.out,
                   Y.out = split.data$Y.out,
                   subset = subset,
                   weights = weights,
                   parameter.scales = rep(1, n.parameters),
                   prior.mean = prior.mean,
                   prior.sd = prior.sd)

    result
}

processAttributeLevelsFile <- function(attribute.levels.file)
{
    raw.attribute.levels <- readExcelFile(attribute.levels.file)
    n.attributes <- length(raw.attribute.levels)
    nms <- names(raw.attribute.levels)
    attribute.levels <- list()
    for (i in 1:n.attributes)
    {
        not.na <- !is.na(raw.attribute.levels[[i]])
        attribute.levels[[nms[i]]] <- raw.attribute.levels[[i]][not.na]
    }
    attribute.levels
}

parameterNamesFromAttributes <- function(attribute.levels)
{
    n.attributes <- length(attribute.levels)
    n.attribute.parameters <- unlist(lapply(attribute.levels, length)) - 1
    n.parameters <- sum(n.attribute.parameters)
    attribute.names <- names(attribute.levels)
    result <- rep("", n.parameters)
    ind <- 1
    for (i in 1:n.attributes)
    {
        for (j in 1:n.attribute.parameters[i])
        {
            result[ind] <- paste0(attribute.names[i], ": ",
                                  attribute.levels[[i]][j + 1])
            ind <- ind + 1
        }
    }
    result
}

allNamesFromAttributes <- function(attribute.levels)
{
    n.attributes <- length(attribute.levels)
    n.attribute.levels <- unlist(lapply(attribute.levels, length))
    attribute.names <- names(attribute.levels)
    result <- rep("", sum(n.attribute.levels))
    ind <- 1
    for (i in 1:n.attributes)
    {
        for (j in 1:n.attribute.levels[i])
        {
            result[ind] <- paste0(attribute.names[i], ": ",
                                  attribute.levels[[i]][j])
            ind <- ind + 1
        }
    }
    result
}

addChoiceParameters <- function(X, n.attributes, n.parameters,
                                n.attribute.parameters, n.choices, par.names,
                                all.names, has.none.of.these)
{
    X <- addChoiceParametersX(X)
    n.attributes <- n.attributes + 1
    n.parameters <- n.parameters + n.choices - 1
    n.attribute.parameters <- c(n.choices, n.attribute.parameters)
    alt.labels <- createAlternativeLabels(n.choices, has.none.of.these)
    par.names <- c(alt.labels[-1], par.names)
    all.names <- c(alt.labels, all.names)
    list(X = X, n.attributes = n.attributes, n.parameters = n.parameters,
         n.attribute.parameters = n.attribute.parameters,
         par.names = par.names, all.names = all.names)
}

addChoiceParametersX <- function(X)
{
    dim.X <- dim(X)
    n.choices <- dim.X[3]
    dim.new.X <- dim.X
    dim.new.X[4] <- dim.X[4] + n.choices - 1
    new.X <- array(data = 0, dim = dim.new.X)
    new.X[, , , n.choices:dim.new.X[4]] <- X
    for (i in 1:(n.choices - 1))
        new.X[, , i + 1, i] <- 1
    new.X
}

createAlternativeLabels <- function(n.choices, has.none.of.these)
{
    result <- paste0("Alternative: ", 1:n.choices)
    if (has.none.of.these)
        result[n.choices] <- "Alternative: none of these"
    result
}

fillXAttributes <- function(n.parameters, n.attributes, n.attribute.parameters,
                            ordered.attributes, question.design)
{
    result <- rep(0, n.parameters)
    parameter.index <- 0
    for (l in 1:n.attributes)
    {
        if (ordered.attributes[l])
        {
            if (question.design[l] > 1)
            {
                start.ind <- parameter.index + 1
                end.ind <- parameter.index + question.design[l] - 1
                result[start.ind:end.ind] <- 1
            }
        }
        else
        {
            if (question.design[l] > 1)
                result[parameter.index + question.design[l] - 1] <- 1
        }

        parameter.index <- parameter.index + n.attribute.parameters[l]
    }
    result
}
