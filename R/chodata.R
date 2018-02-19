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
    n.attribute.variables <- unlist(lapply(attribute.levels, length)) - 1
    n.variables <-  sum(n.attribute.variables)
    var.names <- variableNamesFromAttributes(attribute.levels)
    all.names <- allNamesFromAttributes(attribute.levels)
    variable.scales <- rep(1, n.variables)

    checkPriorParameters(input.prior.mean, input.prior.sd, n.attributes)

    ordered.attributes <- if (length(input.prior.mean) == 1)
        rep(FALSE, n.attributes)
    else
        input.prior.mean != 0

    n.questions.left.in <- n.questions - n.questions.left.out
    left.out <- LeftOutQuestions(n.respondents, n.questions, n.questions.left.out, seed)

    X <- array(data = 0, dim = c(n.respondents, n.questions, n.choices, n.variables))
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
                    X[i, j, k, ] <- fillXNoneOfThese(n.variables,
                                                     n.attributes,
                                                     n.attribute.variables)
                else
                {
                    ind <- ind + 1 # attributes row
                    X[i, j, k, ] <- fillXAttributes(n.variables,
                                                    n.attributes,
                                                    n.attribute.variables,
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
        output <- addChoiceParameters(X, n.attributes, n.variables,
                                      n.attribute.variables, n.choices,
                                      var.names, all.names, input.prior.mean,
                                      input.prior.sd, has.none.of.these)
        X <- output$X
        n.attributes <- output$n.attributes
        n.variables <- output$n.variables
        n.attribute.variables <- output$n.attribute.variables
        var.names <- output$var.names
        all.names <- output$all.names
        input.prior.mean <- output$input.prior.mean
        input.prior.sd <- output$input.prior.sd
    }

    subset <- CleanSubset(subset, n.respondents)
    weights <- prepareWeights(weights, subset)
    X <- X[subset, , , ]
    Y <- Y[subset, ]
    n.respondents <- sum(subset)

    split.data <- crossValidationSplit(X, Y, n.questions.left.out, seed)

    prior.mean <- processInputPrior(input.prior.mean, n.variables,
                                    n.attributes, n.attribute.variables,
                                    variable.scales)
    prior.sd <- processInputPrior(input.prior.sd, n.variables, n.attributes,
                                  n.attribute.variables, variable.scales)

    result <- list(n.questions = n.questions,
                   n.questions.left.in = n.questions.left.in,
                   n.questions.left.out = n.questions.left.out,
                   n.choices = n.choices,
                   n.attributes = n.attributes,
                   n.respondents = n.respondents,
                   n.variables = n.variables,
                   n.attribute.variables = n.attribute.variables,
                   var.names = var.names,
                   all.names = all.names,
                   X.in = split.data$X.in,
                   Y.in = split.data$Y.in,
                   X.out = split.data$X.out,
                   Y.out = split.data$Y.out,
                   subset = subset,
                   weights = weights,
                   variable.scales = rep(1, n.variables),
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

variableNamesFromAttributes <- function(attribute.levels)
{
    n.attributes <- length(attribute.levels)
    n.attribute.variables <- unlist(lapply(attribute.levels, length)) - 1
    n.variables <- sum(n.attribute.variables)
    attribute.names <- names(attribute.levels)
    result <- rep("", n.variables)
    ind <- 1
    for (i in 1:n.attributes)
    {
        for (j in 1:n.attribute.variables[i])
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

addChoiceParameters <- function(X, n.attributes, n.variables,
                                n.attribute.variables, n.choices, var.names,
                                all.names, input.prior.mean, input.prior.sd,
                                has.none.of.these)
{
    X <- addChoiceParametersX(X)
    n.attributes <- n.attributes + 1
    n.variables <- n.variables + n.choices - 1
    n.attribute.variables <- c(n.choices, n.attribute.variables)
    alt.labels <- createAlternativeLabels(n.choices, has.none.of.these)
    var.names <- c(alt.labels[-1], var.names)
    all.names <- c(alt.labels, all.names)
    if (length(input.prior.mean) > 1)
        input.prior.mean <- c(0, input.prior.mean)
    if (length(input.prior.sd) > 1)
        input.prior.sd <- c(5, input.prior.sd)
    list(X = X, n.attributes = n.attributes, n.variables = n.variables,
         n.attribute.variables = n.attribute.variables,
         var.names = var.names, all.names = all.names,
         input.prior.mean = input.prior.mean,
         input.prior.sd = input.prior.sd)
}

createAlternativeLabels <- function(n.choices, has.none.of.these)
{
    result <- paste0("Alternative: ", 1:n.choices)
    if (has.none.of.these)
        result[n.choices] <- "Alternative: none of these"
    result
}

fillXAttributes <- function(n.variables, n.attributes, n.attribute.variables,
                            ordered.attributes, question.design)
{
    result <- rep(0, n.variables)
    variable.index <- 0
    for (l in 1:n.attributes)
    {
        if (ordered.attributes[l])
        {
            if (question.design[l] > 1)
            {
                start.ind <- variable.index + 1
                end.ind <- variable.index + question.design[l] - 1
                result[start.ind:end.ind] <- 1
            }
        }
        else
        {
            if (question.design[l] > 1)
                result[variable.index + question.design[l] - 1] <- 1
        }

        variable.index <- variable.index + n.attribute.variables[l]
    }
    result
}
