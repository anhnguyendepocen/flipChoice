# Simple method as produced by AlgDesign package
#' @importFrom stats model.matrix
dScore <- function(design)
{
    attribute.columns <- data.frame(design[, c(-1, -2, -3)])
    attribute.columns <- data.frame(lapply(attribute.columns, as.factor))
    X <- model.matrix( ~ ., data = data.frame(attribute.columns))
    d.score <- det(crossprod(X)) ^ (1 / ncol(X)) / nrow(X)
    return(d.score)
}

# Compute the D-error of an unlabeled design (according to Huber and Zwerina 1996)
# design.matrix is a matrix for an unlabeled choice design in the long format, meaning that
#   each row describes one of the alternatives in one of the choice tasks. Complete data for
#   each task is spread across several rows. The columns are:
#   - Column 1 indicates the version number for each profile
#   - Column 2 indicates the task number for each profile
#   - Column 3 indicates the alternative number
#   - Columns 4 and up each correspond to an attribute, with the entries in the columns indicating
#     the level of the attribute (beginning at level 1).
# attribute.levels is a vector of numbers indicating how many levels are in each attribute. The order should
#   correspond to the order of columns in the design.
# effects is a boolean parameter indicating whether or not the error should be computed based on
#   effects coding (TRUE) or dummy coding (FALSE).
# prior is a vector of prior parameters for the attribute levels. Keeping prior = NULL uses a flat prior
# See https://faculty.fuqua.duke.edu/~jch8/bio/Papers/Huber%20Zwerina%201996%20Marketing%20Research.pdf
dErrorHZ <- function(design.matrix, attribute.levels, effects = TRUE, prior = NULL)
{
    K <- sum(attribute.levels - 1) # Total number of parameters
    J <- max(design.matrix[, 3]) # Number of alts per task

    # Treat multiple versions as additional questions
    n.versions <- design.matrix[NROW(design.matrix), 1]
    n.questions <- max(design.matrix[, 2])
    N <- n.versions * n.questions  # Number of tasks
    design.matrix[, 2] <- rep(seq(N), each = J)
    design.matrix <- design.matrix[, -1]

    M <- N * J

    # Generate a coded version of the design using dummy coding or effects coding
    des.att <- design.matrix[, 3:ncol(design.matrix)] # Part of the design matrix containing the attributes
    coded.design <- encode.design(des.att, effects = effects)

    # Generate choice probabilities of each alternative
    if (!is.null(prior))
        diagP <- logitChoiceProbs(coded.design, prior, J, N)
    else
        diagP <- rep(1 / J, M)

    # det of inverse == inverse of det
    dPCriterion(coded.design, diagP, N, J) ^ (-1 / K)
}

dPCriterion <- function(coded.design, choice.probs, n.questions,
                        alternatives.per.question)
{
    xbars <- vector("numeric")
    for (s in 1L:n.questions)
    {
        question.indices <- (s - 1) * alternatives.per.question +
                            (1:alternatives.per.question)
        sums <- colSums(coded.design[question.indices, ] *
                            choice.probs[question.indices])
        xbars <- rbind(xbars, rep.row(sums, alternatives.per.question))
    }
    Z <- as.matrix(coded.design - xbars)
    omega <- crossprod(Z, choice.probs * Z)   # t(Z) %*% P %*% Z
    det(omega)
}

d0Criterion <- function(coded.design, n.questions, alternatives.per.question)
{
    n.parameters <- ncol(coded.design)
    result <- matrix(0, nrow = n.parameters, ncol = n.parameters)
    for (s in 1:n.questions)
    {
        question.indices <- (s - 1) * alternatives.per.question +
                            (1:alternatives.per.question)
        xs <- coded.design[question.indices, ]
        result <- result + crossprod(xs) - tcrossprod(colSums(xs)) /
                  alternatives.per.question
    }
    det(result / alternatives.per.question)
}

logitChoiceProbs = function(coded.matrix, prior, number.alternatives, number.tasks) {
    if (ncol(coded.matrix) != length(prior)) {
        stop("Number of columns in coded.matrix does not match the number of prior parameters")
    }

    if (nrow(coded.matrix) != number.alternatives * number.tasks) {
        stop("Number of rows in design does not match the number of alternatives and tasks")
    }

    choice.probabilities = rep(0, number.alternatives * number.tasks)

    for (task in 1L:number.tasks) {
        current.probs = rep(0, number.alternatives)
        for (alt in 1L:number.alternatives) {
            row.index = (task - 1) * number.alternatives + alt
            current.probs[alt] = exp(coded.matrix[row.index, ] %*% prior)
        }
        start.index = (task - 1) * number.alternatives + 1
        end.index = task * number.alternatives
        choice.probabilities[start.index:end.index] = current.probs / sum(current.probs)
    }
    return(choice.probabilities)
}

rep.row = function(x, n) {
    # Returns a matrix with n rows where each row is a copy of x
    matrix(rep(x, each = n), nrow = n)
}

# Produce an encoded matrix without intercept
encode.design <- function(design, effects = TRUE) {

    old.contrasts <- options("contrasts")

    if (!"data.frame" %in% class(design))
    {
        design <- data.frame(design)
        design[colnames(design)] <- lapply(design[colnames(design)], factor)
    }

    if (effects)
        options(contrasts = c("contr.sum", "contr.poly"))
    else
        options(contrasts = c("contr.treatment", "contr.poly"))
    dummy.matrix <- model.matrix( ~ ., data = design)
    options(contrasts = old.contrasts[[1]])
    return(dummy.matrix[, -1])
}
