#' Construct an Efficient Choice Model Design
#'
#' Uses the Modified Federov algorithm and incorporates prior beliefs
#' on the true coefficient values to find a design for a discrete
#' choice experiment that minimize D-error.
#' @param levels.per.attribute A \emph{named} vector containing the
#'     number of levels for each attribute with names giving the
#'     attribute names.  See the Details.
#' @param prior Number vector or two-column matrix containing prior
#'     values for the model coefficients.  The vector length or number
#'     of rows in the matrix must correspond to the number of
#'     attributes/attribute levels specified in
#'     \code{levels.per.attribute}.  If \code{NULL}, the prior for the
#'     coefficients is assumed to be identically zero.  In the matrix
#'     case, the first column is taken as the prior mean for the
#'     coefficients and the second is taken to be the prior variances.
#'     If only one column is present, the prior for the coefficients
#'     is assumed to be centered at those values.
#' @param alternatives.per.question Numeric value indicating the
#'     number of profiles to show per question.
#' @param n.questions Numeric value specifying the total number of
#'     questions/tasks to be performed by each respondent.
#' @param labeled.alternatives Logical; whether the first attribute
#'     labels the alternatives.  See the Details.
#' @param dummy.coding Logical value indicating whether dummy coding
#'     should be used for the attributes in the design matrix.  If
#'     \code{FALSE}, effects coding is used.
#' @param seed Integer value specifying the random seed to use for the
#'     algorithm.
#' @param n.sim Number of simulations to draw from the prior to initial
#'     the algorithm.
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
#' @details If \code{labeled.alternatives} is \code{TRUE}, an
#'     alternative-specific constant will be added to the model and
#'     the first component of \code{levels.per.attribute} is assumed
#'     to contain a name for the labels and the number of labels;
#'     i.e. the number of alternatives per question, and hence,
#'     \code{alternatives.per.question} will be ignored.
#' @seealso \code{\link[idefix]{Modfed}}
#' @references Huber, J. and K. Zwerina (1996). The Importance of
#'     Utility Balance in Efficient Choice Designs. Journal of
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
#' efficientDesign(levels.per.attribute, prior, 3, 9)
#' }
efficientDesign <- function(
                               levels.per.attribute = NULL,
                               prior = NULL,
                               alternatives.per.question,
                               n.questions,
                               labeled.alternatives = FALSE,
                               dummy.coding = TRUE,
                               seed = 1776,
                               n.sim = 10)
{
    set.seed(seed)
    code <- ifelse(dummy.coding, "D", "E")

    ## parsed.data <- parsePastedData(attrib, n.sim = n.sim, coding = code)
    ## levels.per.attribute <- parsed.data[["lvls"]]
    ## par.draws <- parsed.data[["prior"]]
    if (labeled.alternatives)
    {
        idx <- 2:length(levels.per.attribute)
        alternatives.per.question <- levels.per.attribute[1]
    }else
        idx <- 1:length(levels.per.attribute)

    candidates <- Profiles(levels.per.attribute[idx],
                           coding = rep(code, length(levels.per.attribute[idx])))

    alt.specific.const <- labeled.alternatives + integer(alternatives.per.question)
    alt.specific.const[1L] <- 0  # needed for identifiable model

    n.coef <- ncol(candidates) + sum(alt.specific.const == 1L)
    if (is.null(prior))
        prior <- numeric(n.coef)
    else if (NROW(prior) != n.coef || NCOL(prior) > 2L)
        stop(gettextf("Prior should be a length-%d vector or a two-column matrix with %d rows",
                      n.coef, n.coef))

    par.draws <- prior
    if (NCOL(prior) == 2L)
        par.draws <- matrix(rnorm(n.coef*n.sim, prior[, 1], prior[, 2]),
                            n.sim, n.coef, byrow = TRUE)

    out <- Modfed(candidates, n.sets = n.questions, n.alts = alternatives.per.question,
                  alt.cte = alt.specific.const, par.draws = par.draws)
    out$model.matrix <- out$design

    out$design <- modelMatrixToUnlabeledDesign(out$design, levels.per.attribute,
                                                alternatives.per.question)
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
modelMatrixToUnlabeledDesign <- function(
                                   model,
                                   lvls,
                                   alternatives.per.question)
{
    attr.list <- lapply(lvls, seq.int)
    code <- ifelse(any(model == -1), "E", "D")
    alt.specific.const <- integer(alternatives.per.question)
    alt.specific.const[1] <- 0
    idx <- 1:length(attr.list)
    out <- decode(model, attr.list[idx], coding = rep(code, length(lvls[idx])),
                  alt.cte = alt.specific.const)
    ## out <- as.data.frame(out)
    question <- as.numeric(sub("set([0-9]+)[.]alt[0-9]+", "\\1", rownames(model)))
    alternative <- as.numeric(sub("set[0-9]+[.]alt([0-9]+)", "\\1", rownames(model)))
    out <- cbind(question, alternative, out)
    colnames(out) <- c("Question", "Alternative", names(lvls))
    ## rownames(out) <- rownames(model)
    out
}

#' Convert model matrix to data frame of factors
#' @note Based of the function Decode from idefix, which is
#' no longer exported by the package as of v0.2.4
#' @noRd
#' @importFrom idefix Profiles
decode <- function (set, lvl.names, coding, alt.cte = NULL, c.lvls = NULL)
{
    if (!is.null(alt.cte))
    {
        contins <- which(alt.cte == 1)
        if (!length(contins) == 0)
            set <- set[, !grepl("alt[0-9]*.cte", colnames(set))]
    }
    n.alts <- nrow(set)
    n.att <- length(lvl.names)
    conts <- which(coding == "C")
    lvls <- numeric(n.att)
    for (i in 1:n.att)
        lvls[i] <- length(lvl.names[[i]])

    dc <- Profiles(lvls = lvls, coding = coding, c.lvls = c.lvls)
    d <- as.data.frame(expand.grid(lvl.names))
    m <- matrix(data = NA, nrow = n.alts, ncol = n.att)
    if (ncol(set) != ncol(dc))
    {
        stop("Number of columns of the set does not match expected number based ",
             "on the other arguments.")
    }
    for (i in 1:n.alts)
    {
        lev.num <- d[as.numeric(which(apply(dc, 1,
                                            function(x) all(x == set[i, ])))), ]
        lev.num <- as.numeric(lev.num)
        if (any(is.na(lev.num))) {
            stop("The set does not match with the type of coding provided")
        }
        for (c in 1:n.att) {
            m[i, c] <- lvl.names[[c]][lev.num[c]]
        }
    }
    return(m)
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

#' @importFrom stats rnorm
#' @noRd
parsePastedData <- function(paste.data, n.sim = 10, coding = "D", labeled.alts = FALSE)
{
    ## if (is.null(prior) || length(prior) == 0L)
    ##     return(numeric(n.par))

    paste.data <- removeEmptyCells(paste.data)
    n.attr <- ncol(paste.data)
    cnames <- paste.data[1, ]
    mean.idx <- grep("^mean$", cnames, ignore.case = TRUE)
    sd.idx <- grep("^sd$", cnames, ignore.case = TRUE)
    n.means <- length(mean.idx)
    n.sd <- length(sd.idx)

    lvls.idx <- seq_len(ncol(paste.data))
    if (n.means + n.sd)
        lvls.idx <- lvls.idx[-c(mean.idx, sd.idx)]

    attr.names <- cnames[lvls.idx]
    n.lvls <- colSums(apply(paste.data[-1, lvls.idx, drop = FALSE], 2,
                    function(x) x != ""))
    names(n.lvls) <- attr.names

    if (n.means)
    {
        means.list <- vector("list", n.means)
        names(means.list) <- cnames[mean.idx - 1]
        for (i in seq_len(n.means))
        {
            idx <- mean.idx[i]
            n.lvl <- n.lvls[cnames[idx - 1]]
            tmean <- suppressWarnings(as.numeric(paste.data[2:(n.lvl + 1), idx]))
            if (any(is.na(tmean)))
                stop(gettextf("Invalid or incorrect number of prior means specified for %s, %d%s.",
                              cnames[idx - 1], n.lvl, " are required."))
            means.list[[i]] <- tmean
        }
    }
    if (n.sd)
    {
        sd.list <- vector("list", n.sd)
        names(sd.list) <- cnames[sd.idx - 2]
        for (i in seq_len(n.sd))
        {
            idx <- sd.idx[i]
            n.lvl <- n.lvls[cnames[idx - 2]]
            tsd <- suppressWarnings(as.numeric(paste.data[2:(n.lvl + 1), idx]))
            if (any(is.na(tsd)) || any(tsd <= 0))
                stop(gettextf("Invalid or incorrect number of prior standard deviations specified for %s, %d%s",
                     cnames[idx - 2], n.lvl, " are required."))
            sd.list[[i]] <- tsd
        }
    }else
        sd.list <- NULL

    if (n.means)
        prior <- constrainedPrior(n.lvls, means.list, sd.list, coding, n.sim)
    else
        prior <- numeric(sum(n.lvls) - length(n.lvls))

    return(list(lvls = n.lvls, prior = prior,
                attribute.list = pastedAttributesToList(paste.data[, lvls.idx, drop = FALSE])))
}

#' Convert unconstrained prior values to constrained numeric ones
#'
#' Converts the prior means and standard deviations for unconstrained
#' choice model coefficients (or "part-worths") to constrained
#' parameters used as inputs to \code{\link[idefix]{Modfed}}.
#' @param lvls A \emph{named} numeric vector giving the number of
#'     levels for all attributes in the model.
#' @param prior.means A \emph{named} list of prior means for the
#'     coefficients. Each element contains the prior means for a
#'     particular attribute with a name corresponding to a name in
#'     \code{lvls}.  Attributes in \code{lvls} that are not found in
#'     \code{prior.means} are given prior means of 0 for their
#'     coefficients.
#' @param prior.sd A \emph{named} list of prior standard deviations
#'     for the coefficients. Each element contains the prior standard
#'     deviations for a particular attribute with a name corresponding
#'     to a name in \code{lvls}. Attributes in \code{lvls} that are
#'     not found in \code{prior.sd} are given prior standard
#'     deviations of 1 for their coefficients.
#' @param coding \code{"D"} to use dummy coding
#'     (\code{\link[stats]{contr.treatment}}) or \code{"E"} to use
#'     effects coding (\code{\link[stats]{contr.sum}}).
#' @details \code{\link[idefix]{Modfed}} requires either a vector of
#'     prior means for every coefficient or a matrix of draws from the
#'     prior for the coefficients.
#' @return If \code{prior.sd} is \code{NULL}, a vector of prior means
#'     for every coefficient in the model.  Otherwise, a \code{n.sim}
#'     by number of coefficients matrix of prior draws.
#' @importFrom stats contr.sum contr.treatment
#' @noRd
constrainedPrior <- function(
                             lvls,
                             prior.means,
                             prior.sd = NULL,
                             coding = "D",
                             n.sim = 10)
{
    contr.fun <- if (coding == "D")
                            contr.treatment
                        else
                            contr.sum

    n.lvls <- length(lvls)
    n.coef <- sum(lvls) - n.lvls
    n.means <- length(prior.means)
    has.sd <- length(prior.sd) > 0L

    beta.c <- NULL
    sd.c <- NULL
    for (n in names(lvls))
    {
        p <- prior.means[[n]]
        s <- prior.sd[[n]]
        n.lvl <- lvls[n]
        n.c <- n.lvl - 1
        default.beta <- numeric(n.c)
        default.sd <- 1 + numeric(n.c)
        if (!is.null(p) || !is.null(s))
        {
            Cmat <- contr.fun(as.factor(seq_len(n.lvl)))
            Cleftinv <- solve(crossprod(Cmat), t(Cmat))
            if (is.null(p))
                tbeta <- default.beta
            else
                tbeta <- Cleftinv%*%p

            if (is.null(s))
                tsd <- default.sd
            else  # V(beta.c) = C*diag(V(beta.uc))*C^T
                tsd <- sqrt(diag(tcrossprod(Cleftinv%*%diag(s^2), Cleftinv)))
        }else
        {
            tbeta <- default.beta
            tsd <- default.sd
        }
        beta.c <- c(beta.c, tbeta)
        sd.c <- c(sd.c, tsd)
    }

    if (!has.sd)
        return(beta.c)

    return(cbind(beta.c, sd.c))
}
