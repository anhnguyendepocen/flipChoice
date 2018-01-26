#' Construct an Efficient Choice Model Design
#'
#' Uses the Modified Federov algorithm and incorporates prior beliefs
#' on the true coefficient values to find a design for a discrete
#' choice experiment that minimize D-error.
#' @param levels.per.attribute A \emph{named} vector containing the
#'     number of levels for each attribute with names giving the attribute names.
#' @param prior Character matrix containing prior values for the model
#'     coefficients.  The number of coefficients must correspond to
#'     the number of attributes/attribute levels specified in
#'     \code{levels.per.attribute}.  If \code{NULL}, the prior for the
#'     coefficients is assumed to be identically zero.  If the matrix
#'     contains two columns, the first column is taken as the prior
#'     mean for the coefficients and the second is taken to be the
#'     prior variances.  If only one column is present, the prior for
#'     the coefficients is assumed to be centered at those values.
#' @param alternatives.per.question Numeric value indicating the
#'     number of profiles to show per question.
#' @param n.questions Numeric value specifying the total number of
#'     questions/tasks to be performed by each respondent.
#' @param labelled.alternatives Logical; whether the first attribute
#'     labels the alternatives.
#' @param dummy.coding Logical value indicating whether dummy coding
#'     should be used for the attributes in the design matrix.  If
#'     \code{FALSE}, effects coding is used.
#' @param seed Integer value specifying the random seed to use for the
#'     algorithm.
#' @return A list containing the following components.  \itemize{
#'     \item \code{design} - A numeric matrix wich contains an
#'     efficient design.  \item \code{error} - Numeric value
#'     indicating the D(B)-error of the design.  \item
#'     \code{inf.error} - Numeric value indicating the percentage of
#'     samples for which the D-error was \code{Inf}.
#'     \item\code{prob.diff} - Numeric value indicating the difference
#'     between the alternative with the highest and the one with the
#'     lowest probability for each choice set. If prior means and
#'     variances were supplied in \code{prior}, this is based on the
#'     average over all draws.  }
#' @seealso \code{\link[idefix]{Modfed}}
#' @references Huber, J. and K. Zwerina (1996). The Importance of
#'     Utility Balance in Efficient Choice Designs. Jouranl of
#'     Marketing
#'     Research. \url{https://faculty.fuqua.duke.edu/~jch8/bio/Papers/Huber
#'     Zwerina 1996 Marketing Research.pdf}
#'
#' Zwerina, K., J. Huber and W. F. Kuhfeld. (2000). A General Method
#' for Constructing Efficient Choice
#' Designs. \url{http://support.sas.com/techsup/technote/mr2010e.pdf}
#' @importFrom idefix Modfed Profiles
#' @examples
#' levels.per.attribute <- c(car = 3, house = 3, boat = 3)
#' prior <- matrix(as.character(1), sum(levels.per.attribute - 1), 2)
#'
#' ## 3^3/3/9 design
#' \dontrun{
#' modifiedFederovDesign(levels.per.attribute, prior, 3, 9)
#' }
modifiedFederovDesign <- function(
                               levels.per.attribute,
                               prior = NULL,
                               alternatives.per.question,
                               n.questions,
                               labelled.alternatives = FALSE,
                               dummy.coding = TRUE,
                               seed = 1776)
{
    set.seed(seed)
    code <- ifelse(dummy.coding, "D", "E")
    candidates <- Profiles(levels.per.attribute,
                           coding = rep(code, length(levels.per.attribute)))

    par.draws <- parsePastedPrior(prior, candidates)
    alt.specific.const <- labelled.alternatives + integer(alternatives.per.question)
    out <- Modfed(candidates, n.sets = n.questions, n.alts = alternatives.per.question,
                  alt.cte = alt.specific.const, par.draws = par.draws)
    out
}

#' @noRd
pastedAttributesToVector <- function(attributes)
{
    attributes <- removeEmptyCells(attributes)
    lvls <- colSums(attributes != "") - 1L
    names(lvls) <- attributes[1L, ]
    lvls
}

#' @noRd
#' @param model design output from \code{\link[idefix]{Modfed}}
#' @importFrom idefix Decode
modelMatrixToDataFrame <- function(
                                   model,
                                   pasted.attributes,
                                   alternatives.per.question,
                                   labeled.alternatives)
{
    attr.list <- pastedAttributesToList(pasted.attributes)
    code <- ifelse(any(model == -1), "E", "D")
    alt.specific.const <- labeled.alternatives + integer(alternatives.per.question)
    out <- Decode(model, attr.list, coding = rep(code, ncol(pasted.attributes)),
                  alt.cte = alt.specific.const)
    out <- as.data.frame(out)
    question <- as.numeric(sub("set([0-9]+)[.]alt[0-9]+", "\\1", rownames(model)))
    alternative <- as.numeric(sub("set[0-9]+[.]alt([0-9]+)", "\\1", rownames(model)))
    out <- cbind(question, alternative, out)
    names(out) <- c("question", "alternative", pasted.attributes[1, ])
    ## rownames(out) <- rownames(model)
    out
}

#' @noRd
pastedAttributesToList <- function(attributes)
{
    n.attr <- ncol(attributes)
    out <- vector("list", n.attr)
    for (i in seq_len(n.attr))
    {
        tatt <- attributes[-1, i]
        out[[i]] <- tatt[tatt != ""]
    }
    names(out) <- attributes[1, ]
    out
}

#' @noRd
removeEmptyCells <- function(mat)
{
    start.idx <- which(mat != "", arr.ind = TRUE)[1, ]
    mat[start.idx[1]:nrow(mat), start.idx[2]:ncol(mat), drop = FALSE]
}

#' @importFrom stats rnorm
#' @noRd
parsePastedPrior <- function(prior, candidates, n.sim = 10)
{
    n.par <- ncol(candidates)
    if (is.null(prior) || length(prior) == 0L)
        return(numeric(n.par))

    prior <- removeEmptyCells(prior)
    prior <- matrix(suppressWarnings(as.numeric(prior)),
                    nrow = nrow(prior), ncol = ncol(prior))
    if (any(is.na(prior)))
        stop("The entered data for the prior must contain only numeric values.")

    if (nrow(prior) != n.par)
        stop("The entered data for the prior must contain the same number of rows as there are parameters",
             " in the design specified by the entered attributes. I.e. there must be exactly ", sQuote(n.par), " rows.")
    if (ncol(prior) == 1L)
        return(drop(prior))
    else if (ncol(prior) == 2L)
    {
        if (any(prior[, 2L] <= 0))
            stop("All prior variances must be positive.")
        return(matrix(rnorm(n.par*n.sim, prior[, 1], sqrt(prior[, 2])),
                      n.sim, n.par, byrow = TRUE))
    }else
        stop("The entered data for the prior must have either one or two columns.")
}
