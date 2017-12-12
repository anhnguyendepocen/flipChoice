#' @importFrom readxl read_excel
processDesignFile <- function(design.file, attribute.levels.file,
                              choices, questions, subset, weights,
                              n.questions.left.out, seed,
                              input.prior.mean, input.prior.sd,
                              include.choice.parameters)
{
    output <- readDesignFile(design.file, attribute.levels.file)
    design <- output$design
    attribute.levels <- output$attribute.levels

    output <- processRespondentData(choices, questions)
    choices <- output$choices
    questions <- output$questions

    n.attributes <- dim(design)[2] - 3
    n.questions <- dim(questions)[2]
    n.choices <- getNumberOfChoices(choices)
    n.respondents <- dim(questions)[1]

    # A "None of these" option is left out from the design
    add.none.of.these <- n.choices == length(unique(design[[3]])) + 1
    # A "None of these" option is included in the design
    none.of.these.included <- any(rowSums(design[-1:-3]) == 0)
    has.none.of.these <- add.none.of.these || none.of.these.included

    # if (has.none.of.these)
    #     attribute.levels <- lapply(attribute.levels, function(x) c(x, "None"))

    n.attributes <- length(attribute.levels)
    n.attribute.variables <- unlist(lapply(attribute.levels, length))
    n.variables <-  sum(n.attribute.variables)
    n.raw.variables <- n.variables - n.attributes
    var.names <- variableNamesFromAttributes(attribute.levels)
    variable.scales <- rep(1, n.variables)

    checkPriorParameters(input.prior.mean, input.prior.sd, n.attributes)

    ordered.attributes <- if (length(input.prior.mean) == 1)
        rep(FALSE, n.attributes)
    else
        input.prior.mean != 0

    if (any(ordered.attributes) && has.none.of.these)
        stop('Ordered attributes cannot be specified when the "None of these"',
             ' alternative is present in the analysis.')

    n.questions.left.in <- n.questions - n.questions.left.out
    left.out <- LeftOutQuestions(n.respondents, n.questions,
                                 n.questions.left.out, seed)

    X <- array(data = 0, dim = c(n.respondents, n.questions, n.choices,
                                 n.variables))

    ind.columns <- (1:n.attributes) + 3
    for (i in 1:n.respondents)
    {
        for (j in 1:n.questions)
        {
            question.number <- questions[i, j]
            ind <- which(design[[2]] == question.number)[1]
            for (k in 1:n.choices)
            {
                if (has.none.of.these && k == n.choices)
                    X[i, j, k, ] <- fillXNoneOfThese(n.variables, n.attributes,
                                                     n.attribute.variables)
                else
                {
                    question.design <- as.vector(t(design[ind, ind.columns]))
                    X[i, j, k, ] <- fillXAttributes(n.variables,
                                                    n.attributes,
                                                    n.attribute.variables,
                                                    ordered.attributes,
                                                    question.design)
                    ind <- ind + 1
                }
            }
        }
    }

    Y <- unname(sapply(choices, as.numeric))

    if (include.choice.parameters)
    {
        output <- addChoiceParameters(X, n.attributes, n.variables,
                                      n.raw.variables, n.attribute.variables,
                                      n.choices, var.names, input.prior.mean,
                                      input.prior.sd)
        X <- output$X
        n.attributes <- output$n.attributes
        n.variables <- output$n.variables
        n.raw.variables <- output$n.raw.variables
        n.attribute.variables <- output$n.attribute.variables
        var.names <- output$var.names
        input.prior.mean <- output$input.prior.mean
        input.prior.sd <- output$input.prior.sd
    }

    subset <- CleanSubset(subset, n.respondents)
    weights <- prepareWeights(weights, subset)
    X <- X[subset, , , ]
    Y <- Y[subset, ]
    n.respondents <- sum(subset)

    split.data <- crossValidationSplit(X, Y, n.questions.left.out, seed)

    prior.mean <- processInputPrior(input.prior.mean, n.raw.variables,
                                    n.attributes, n.attribute.variables,
                                    variable.scales)
    prior.sd <- processInputPrior(input.prior.sd, n.raw.variables,
                                  n.attributes, n.attribute.variables,
                                  variable.scales)

    result <- list(n.questions = n.questions,
                   n.questions.left.in = n.questions.left.in,
                   n.questions.left.out = n.questions.left.out,
                   n.choices = n.choices,
                   n.attributes = n.attributes,
                   n.respondents = n.respondents,
                   n.variables = n.variables,
                   n.raw.variables = n.raw.variables,
                   n.attribute.variables = n.attribute.variables,
                   var.names = var.names,
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

readDesignFile <- function(design.file, attribute.levels.file)
{
    design <- read_excel(design.file)
    n.attributes <- length(design) - 3
    design.is.numeric <- all(sapply(design[4:length(design)], function(x) {
            is.numeric(x) || !any(is.na(suppressWarnings(as.numeric(x))))
        }))
    if (design.is.numeric && is.null(attribute.levels.file))
        stop("A file containing attribute levels is required.")
    else if (!design.is.numeric && !is.null(attribute.levels.file))
        warning("The supplied attribute levels will be ignored as levels",
            " are present in the design file.")

    if (design.is.numeric)
    {
        # we apply as.numeric because the design may still contain strings of
        # numbers, e.g. "12"
        design <- data.frame(sapply(design, as.numeric))
        attribute.levels <- processAttributeLevelsFile(attribute.levels.file)
        result <- list(design = design, attribute.levels = attribute.levels)
    }
    else
    {
        nms <- names(design[-1:-3])
        attribute.levels <- list()
        for (i in 1:n.attributes)
        {
            attribute.factor <- as.factor(design[[i + 3]])
            attribute.levels[[nms[i]]] <- levels(attribute.factor)
            design[[i + 3]] <- as.numeric(attribute.factor)
        }
        result <- list(design = design, attribute.levels = attribute.levels)
    }
    result
}

processRespondentData <- function(choices, questions)
{
    choices <- data.frame(sapply(choices, as.numeric))
    ind <- !is.na(rowSums(choices) + rowSums(questions))
    if (sum(!ind) > 0)
        warning(sum(!ind), " respondents with missing data were omitted from",
                " the analysis.")
    list(choices = choices[ind, ], questions = questions[ind, ])
}

# Variable values for the "None of these" choice
fillXNoneOfThese <- function(n.variables, n.attributes, n.attribute.variables)
{
    result <- rep(0, n.variables)
    # variable.index <- 0
    # for (l in 1:n.attributes)
    # {
    #     variable.index <- variable.index + n.attribute.variables[l]
    #     result[variable.index] <- 1
    # }
    result
}

getNumberOfChoices <- function(choices)
{
    first.choices.column <- choices[[1]]
    if (is.numeric(first.choices.column))
        length(unique(first.choices.column))
    else
        length(levels(first.choices.column))
}
