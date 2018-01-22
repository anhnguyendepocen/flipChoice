#' @importFrom stats model.matrix
dScore <- function(design) {
    attribute.columns <- data.frame(design[, 3:ncol(design)])
    attribute.columns <- data.frame(lapply(attribute.columns, as.factor))
    X <- model.matrix( ~ ., data = data.frame(attribute.columns))
    d.score <- det(t(X) %*% X) ^ (1 / ncol(X)) / nrow(X)
    return(d.score)
}

# Compute the D-error of an unlabeled design
# design.matrix is a matrix for an unlabeled choice design in the long format, meaning that
#   each row describes one of the alternatives in one of the choice tasks. Complete data for
#   each task is spread across several rows. The columns are:
#   - Column 1 indicates the task number for each profile
#   - Column 2 indicates the alternative number
#   - Columns 3 and up each correspond to an attribute, with the entries in the columns indicating
#     the level of the attribute (beginning at level 1).
# attribute.levels is a vector of numbers indicating how many levels are in each attribute. The order should
#   correspond to the order of columns in the design.
# effects is a boolean parameter indicating whether or not the error should be computed based on
#   effects coding (TRUE) or dummy coding (FALSE).
# prior is a vector of prior parameters for the attribute levels. Keeping prior = NULL uses a flat prior
DerrorHZ = function(design.matrix, attribute.levels, effects = TRUE, prior = NULL) {

    K = sum(attribute.levels - 1) # Total number of parameters
    N = max(design.matrix[,1]) # Number of tasks
    J = max(design.matrix[,2]) # Number of alts per task
    M = N*J

    # Generate a coded version of the design using dummy coding or effects coding
    des.att = design.matrix[, 3:ncol(design.matrix)] # Part of the design matrix containing the attributes
    if (effects) {
        coded.design = effectsMatrix(des.att)
    } else {
        coded.design = dummyMatrix(des.att)
    }

    # Generate choice probabilities
    if (!is.null(prior)) {
        P = diag(logitChoiceProbs(coded.design, prior, J, N))
    } else {
        P = 1/J * diag(M)
    }

    # Compute the D-error (according to Huber and Zwerina 1996)
    flatP = diag(P)
    xbars = vector("numeric")
    for (j in 1L:N) {
        xbars = rbind(xbars, rep.row(colSums(coded.design[design.matrix[,1] == j, ] * flatP[design.matrix[,1] == j]), J))
    }
    Z = as.matrix(coded.design - xbars)
    Omega = t(Z) %*% P %*% Z
    Derror = det(Omega)^(-1/K)
    return(Derror)
}




dummyVector = function(n, k) {
    # Create a dummy vector for the nth level of an attribute with k levels
    dummy = rep(0, k-1)
    if (n > 1)
        dummy[n-1] = 1
    return(dummy)
}

effectsVector = function(n,k) {
    # Create an effects-coded vector for the nth level of an attribute with k
    # levels
    if (n == k)
        effects = rep(-1,k-1)
    else {
        effects = rep(0,k-1)
        effects[n] = 1
    }
    return(effects)
}



dummyMatrix = function(design.matrix) {
    coded.design = mapply(dummyVector, as.numeric(design.matrix[,1]), k = max(as.numeric(design.matrix[,1])), SIMPLIFY = TRUE)
    for (j in 2:ncol(design.matrix)) {
        coded.design = rbind(coded.design, mapply(dummyVector, as.numeric(design.matrix[,j]), k = max(as.numeric(design.matrix[,j])), SIMPLIFY = TRUE))
    }
    coded.design = t(coded.design)
    colnames(coded.design) = 1:ncol(coded.design)
    return(coded.design)
}


effectsMatrix = function(design.matrix) {
    coded.design = mapply(effectsVector, as.numeric(design.matrix[,1]), k = max(as.numeric(design.matrix[,1])), SIMPLIFY = TRUE)
    for (j in 2:ncol(design.matrix)) {
        coded.design = rbind(coded.design, mapply(effectsVector, as.numeric(design.matrix[,j]), k = max(as.numeric(design.matrix[,j])), SIMPLIFY = TRUE))
    }
    coded.design = t(coded.design)
    colnames(coded.design) = 1:ncol(coded.design)
    return(coded.design)
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
