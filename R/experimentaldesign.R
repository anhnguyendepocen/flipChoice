#' Choice modeling experimental design
#'
#' Creates choice model experimental designs according to a given algorithm.
#'
#' @param design.algorithm The algorithm used to create the
#'     design. One of \code{"Random"}, \code{"Shortcut"},
#'     \code{"Balanced overlap"} and \code{"Complete enumeration"},
#'     and \code{"Modified Federov"}.
#' @param attribute.levels \code{\link{list}} of \code{\link{vector}}s
#'     containing the labels of levels for each attribute, with names
#'     corresponding to the attribute labels; \emph{or} a character
#'     matrix with first row containing attribute names and subsequent
#'     rows containing attribute levels.
#' @param prior Character matrix containing prior values for the model
#'     coefficients; only used for \code{design.algorithm ==
#'     "Modified Federov"}; see Details.
#' @param n.questions Integer; the number of questions asked to each
#'     respondent.
#' @param n.versions Integer; the number of versions of the survey to
#'     create.
#' @param alternatives.per.question Integer; the number of alternative
#'     products shown in each question. Ignored if
#'     \code{"labelled.alternatives"} is TRUE.
#' @param prohibitions Character \code{\link{matrix}} where each row
#'     is a prohibited alternative consisting of the levels of each
#'     attribute. If a level is \code{""} or is \code{"All"} then all
#'     levels of that attribute in combination with the other
#'     specified attribute levels are prohibited.
#' @param none.alternatives Integer; the number of 'None' in all
#'     questions.
#' @param labelled.alternatives Logical; whether the first attribute
#'     labels the alternatives.
#' @param output One of \code{"Attributes and levels"},
#'     \code{"Prohibitions"}, \code{"Unlabelled design"},
#'     \code{"Labelled design"}, \code{"Balances and overlaps"}, or
#'     \code{"Standard errors"}.
#' @param seed Integer; random see to be used by the algorithms.
#' @return A list with components
#' \itemize{
#' \item \code{design} - a numeric array of dimensions (number of questions by alternatives per
#' question by number of attributes) where each value is the index of a level.
#' \item \code{design.algorithm} - as per input.
#' \item \code{attribute.levels} - as per input.
#' \item \code{prohibitions} - as per input.
#' \item \code{output} - as per input.
#' \item \code{Derror} - The D-error of \code{design}.
#' }
#'
#' @details If \code{prior} is supplied and \code{design.algorithm ==
#'     "Modified Federov"}, the number of coefficients must correspond
#'     to the number of attributes/attribute levels specified in
#'     \code{attribute.levels}.  If \code{prior} is \code{NULL}, the prior for the
#'     coefficients is assumed to be identically zero.  If the supplied matrix
#'     contains two columns, the first column is taken as the prior
#'     mean for the coefficients and the second is taken to be the
#'     prior variances.  If only one column is present, the prior for
#'     the coefficients is assumed to be centered at those values.
#'
#' @examples
#' x <- CreateExperiment(c(3, 5, 7, 10), 20)
#' ChoiceModelDesign("Random", x$attribute.levels, n.questions = 30,
#'     alternatives.per.question = 4, prohibitions = x$prohibitions,
#'     output = "Unlabelled design")
#' @importFrom utils getFromNamespace
#' @export
ChoiceModelDesign <- function(
                              design.algorithm = c("Random", "Shortcut", "Balanced overlap",
                                                   "Complete enumeration", "Shortcut2", "Modified Federov"),
                              attribute.levels,
                              prior = NULL,
                              n.questions,
                              n.versions = 1,
                              alternatives.per.question,
                              prohibitions = NULL,
                              none.alternatives = 0,
                              labelled.alternatives = FALSE,
                              output = "Unlabelled design",
                              seed = 54123) {


    # Map the design.algorithm to the function
    design.algorithm <- match.arg(design.algorithm)
    function.name <- sub("^([A-Z])", "\\L\\1", design.algorithm, perl = TRUE)
    function.name <- gsub(" ([[:alpha:]])", "\\U\\1", function.name, perl = TRUE)
    design.function <- getFromNamespace(paste0(function.name, "Design"),
                                        ns = "flipChoice")

    ## NEED TO ADD CODE TO CHECK SUPPLIED ARGS ARE VALID FOR REQUESTED ALGORITHM

    # If labelled.alternatives then alternatives.per.question is calculated and not supplied
    if (labelled.alternatives)
        alternatives.per.question <- length(attribute.levels[[1]])

    if (!is.character(attribute.levels) && is.null(names(attribute.levels)))
        names(attribute.levels) <- paste("Attribute", seq(length(attribute.levels)))


    # Convert from labels to numeric and factors
    prohibitions <- encodeProhibitions(prohibitions, attribute.levels)
    integer.prohibitions <- data.frame(lapply(prohibitions, as.integer))
    levels.per.attribute <- sapply(attribute.levels, length)
    names(levels.per.attribute) <- names(attribute.levels)

    # WHILE TESTING THIS FUNCTION I AM RETURNING THE FOLLOWING TWO OUTPUTS WITHOUT
    # CALCULATING THE ChoiceModelDesign OBJECT (WHICH MAY TAKE A LONG TIME)
    if (output == "Attributes and levels")
    {
        max.levels <- max(sapply(attribute.levels, length))
        levels.table <- sapply(attribute.levels, function (x) c(x, rep("", max.levels - length(x))))
        rownames(levels.table) <- paste("Level", seq(max.levels))
        return(levels.table)
    }
    if (output == "Prohibitions")
        return(prohibitions)


    # Call the algorithm to create the design
    # Design algorithms     - use only unlabelled levels (i.e. integer level indices)
    #                       - simply multiply question per respondent by n.versions
    #                       - ignore None alternatives, these are added later
    if (design.algorithm == "Modified Federov")
    {
        prohibitions <- NA
        if (none.alternatives != 0L)
            stop(gettextf("Having none alternatives for %s equal to %s is not currently implemented.",
                          sQuote("design.algorithm"), dQuote(design.algorithm)))
        args <- list(pasted.attributes = attribute.levels, pasted.prior = prior,
                     n.questions = n.questions * n.versions,
                     profiles.per.question = alternatives.per.question,
                     labelled.alternatives = labelled.alternatives,
                     seed = seed)
    }else
    {
        args <- list(levels.per.attribute = levels.per.attribute, n.questions = n.questions * n.versions,
                     alternatives.per.question = alternatives.per.question, prohibitions = integer.prohibitions,
                     none.alternatives = none.alternatives, labelled.alternatives = labelled.alternatives)
    }
    design <- do.call(design.function, args)

    result <- list(design = design,
                   design.with.none = addNoneAlternatives(design, none.alternatives, alternatives.per.question),
                   design.algorithm = design.algorithm,
                   attribute.levels = attribute.levels,
                   prohibitions = prohibitions,
                   n.questions = n.questions,
                   n.versions = n.versions,
                   alternatives.per.question = alternatives.per.question,
                   none.alternatives = none.alternatives,
                   output = output)
    if (design.algorithm == "Modified Federov")
    {
        result$design <- design$design
        result$Derror <- design$error
    }

    class(result) <- "ChoiceModelDesign"
    return(result)
}


#' @export
#' @method print ChoiceModelDesign
#' @noRd
print.ChoiceModelDesign <- function(x, ...) {

    # Output a table with attributes along the columns and levels along the rows
    if (x$output == "Attributes and levels")
    {
        max.levels <- max(sapply(x$attribute.levels, length))
        levels.table <- sapply(x$attribute.levels, function (z) c(z, rep("", max.levels - length(z))))
        rownames(levels.table) <- paste("Level", seq(max.levels))
        print(levels.table)
    }

    else if (x$output == "Prohibitions")
        print(x$prohibitions)

    # Output the design with indices or labels
    else if (x$output == "Unlabelled design")
        print(x$design.with.none)
    else if (x$output == "Labelled design")
        print(labelDesign(x$design.with.none, x$attribute.levels))

    # Single and pairwise level balances and overlaps
    # (where overlaps is the proportion of questions that have >= 1 repeated level, by attribute)
    else if (x$output == "Balances and overlaps")
        print(balancesAndOverlaps(x))

    # TODO OUTPUT STANDARD ERRORS AND D-EFFICIENCY
    else if (x$output == "Standard errors") {

        ml.model <- mlogitModel(x)
        print(list(d.score = dScore(x$design),
<<<<<<< HEAD
                    d.error = DerrorHZ(x$design, sapply(des$attribute.levels, length), effects = FALSE)))
        print(summary(ml.model))
    }
=======
                   d.error = DerrorHZ(flattenDesign(x$design),
                                      sapply(x$attribute.levels, length), effects = FALSE)))
>>>>>>> 2b6e61be83386a413f300a36cf5913ad5ff7d426

    else
        stop("Unrecognized output.")
}


######################### HELPER FUNCTIONS ###########################

# Convert prohibitions from labels to indices (numeric levels)
# and expand "" or "All" to all levels of the attribute.
encodeProhibitions <- function(prohibitions, attribute.levels) {

    if (is.null(prohibitions))
        return(data.frame())

    prohibitions[prohibitions == ""] <- "All"
    prohibitions <- data.frame(prohibitions)
    if (nrow(prohibitions) == 0)
        return(prohibitions)

    if (ncol(prohibitions) != length(attribute.levels))
        stop("Each prohibition must include a level for each attribute (possibly including 'All').")

    for (i in 1:length(attribute.levels))
    {
        # set levels, standardize rownames and find rows with "All"
        prohibitions[, i] <- factor(prohibitions[, i], levels = c(attribute.levels[[i]], "All"))
        if (any(is.na(prohibitions[, i])))
            stop("Prohibition number(s) ", paste(which(is.na(prohibitions[, i])), collapse = ", "),
                 " contains level(s) that are invalid for attribute ", names(attribute.levels)[i])

        rownames(prohibitions) <- seq(nrow(prohibitions))
        rows.with.all <- prohibitions[, i] == "All"

        if (any(rows.with.all))
        {
            # duplicate rows with "All" then fill with levels
            expanded.rows <- rep(rownames(prohibitions), (length(attribute.levels[[i]]) - 1) * rows.with.all + 1)
            prohibitions <- prohibitions[expanded.rows, ]
            prohibitions[prohibitions[, i] == "All", i] <- attribute.levels[[i]]
        }
    }
    rownames(prohibitions) <- seq(nrow(prohibitions))
    colnames(prohibitions) <- names(attribute.levels)
    return(prohibitions)
}

#. Create an experimental design
#'
#' Creates attributes and levels of an experiment, possibly with random prohibitions.
#' This is useful for quickly creating a design without typing in lists of levels.
#' @param levels.per.attribute Numeric \code{\link{vector}} of the number of levels
#' per attribute.
#' @param n.prohibitions Integer; number of prohibitions.
#' @return A list with components
#' \itemize{
#' \item \code{attribute.levels} - a \code{\link{list}} of \code{\link{vector}}s of the
#' labels of levels for each attribute. The names of the vectors are the attribute labels.
#' \item \code{prohibitions} - Character \code{\link{matrix}} where each row is a prohibited
#' alternative consisting of the levels of each attribute.
#' }
#' @export
CreateExperiment <- function(levels.per.attribute, n.prohibitions = 0) {

    set.seed(12345)
    attributes <- LETTERS[1:length(levels.per.attribute)]
    attribute.levels <- sapply(levels.per.attribute, seq, simplify = FALSE)
    attribute.levels <- mapply(paste0, attributes, attribute.levels, SIMPLIFY = FALSE)
    names(attribute.levels) <- attributes

    # may produce duplicate prohibitions
    prohibitions <- t(replicate(n.prohibitions, sapply(attribute.levels, sample, 1)))

    experiment <- list(attribute.levels = attribute.levels, prohibitions = prohibitions)
}


# Convert an unlabelled design into a labelled design
labelDesign <- function(unlabelled.design, attribute.levels) {

    labelled.design <- array(character(0), dim = dim(unlabelled.design))
    colnames(labelled.design) = colnames(unlabelled.design)
    labelled.design[, 1:2] <- unlabelled.design[, 1:2]
    for (i in 1:length(attribute.levels))
        labelled.design[, i + 2] <- attribute.levels[[i]][unlabelled.design[, i + 2]]
    return(labelled.design)
}


# Compute one and two-way level balances and overlaps.
balancesAndOverlaps <- function(cmd) {

    singles <- singleLevelBalances(cmd$design)

    pairs <- pairLevelBalances(cmd$design)

    # label the levels
    singles <- labelSingleBalanceLevels(singles, cmd$attribute.levels)
    pairs <- labelPairBalanceLevels(pairs, cmd$attribute.levels)

    # flatten pairwise list of list and remove unused
    pairs <- unlist(pairs, recursive = FALSE)
    pairs <- pairs[!is.na(pairs)]

    overlaps = countOverlaps(cmd$design)

    return(list(singles = singles, pairs = pairs, overlaps = overlaps))
}


singleLevelBalances <- function(design) {
    singles <- apply(design[, 3:ncol(design)], 2, table)
    if (!is.list(singles))
        singles <- split(singles, rep(1:ncol(singles), each = nrow(singles)))
    return(singles)
}

pairLevelBalances <- function(design) {
    n.attributes <- ncol(design) - 2
    pairs <- replicate(n.attributes, rep(list(NA), n.attributes), simplify = FALSE)
    for (i in 1:(n.attributes - 1))
        for (j in (i + 1):n.attributes) {
            pairs[[i]][[j]] <- table(design[, i + 2], design[, j + 2])
            names(pairs[[i]])[[j]] <- paste0(colnames(design)[i + 2], "/", colnames(design)[j + 2])
        }
    return(pairs)
}

labelSingleBalanceLevels <- function(singles, attribute.levels) {
    return(mapply(function(x, y) {names(x) <- y; x}, singles, attribute.levels, SIMPLIFY = FALSE))
}

labelPairBalanceLevels <- function(pairs, attribute.levels) {
    n.attributes <- length(attribute.levels)
    for (i in 1:(n.attributes - 1))
        for (j in (i + 1):n.attributes) {
            rownames(pairs[[i]][[j]]) <- attribute.levels[[i]]
            colnames(pairs[[i]][[j]]) <- attribute.levels[[j]]
        }
    return(pairs)
}

countOverlaps <- function(design) {
    # table of counts for each level by question, listed for each attribute
    overlaps <- apply(design[, 3:ncol(design)], 2, table, design[, 1])
    # duplicated levels
    overlaps <- lapply(overlaps, ">=", 2)
    # overlaps for questions (rows) by attribute (cols)
    overlaps <- sapply(overlaps, function(x) apply(x, 2, any))

    return(colSums(overlaps) / nrow(overlaps))
}

flattenDesign <- function(design) {
    n.qns <- dim(design)[1]
    n.alts <- dim(design)[2]
    flattened <- matrix(design, nrow = n.qns * n.alts)
    flattened <- cbind(rep(seq(n.qns), n.alts), rep(seq(n.alts), each = n.qns), flattened)
    flattened <- flattened[order(as.numeric(flattened[, 1])), ]
    colnames(flattened) <- c("Question", "Alternative", dimnames(design)[[3]])
    return(flattened)
}

addNoneAlternatives <- function(design, none.alternatives, alternatives.per.question) {
    if (none.alternatives == 0)
        return(design)

    n <- nrow(design)
    new.n <- n * (alternatives.per.question + none.alternatives) / alternatives.per.question
    design.with.none <- matrix(NA, nrow = new.n, ncol = ncol(design))

    # copy existing alternatives
    new.row.indices <- seq(n) + ((seq(n) - 1) %/% alternatives.per.question) * none.alternatives
    design.with.none[new.row.indices, ] <- design

    colnames(design.with.none) <- colnames(design)
    design.with.none[, 1] <- rep(seq(n / alternatives.per.question), each = alternatives.per.question + none.alternatives)
    design.with.none[, 2] <- rep(seq(alternatives.per.question + none.alternatives), n / alternatives.per.question)
    return(design.with.none)
}

# randomly choose responses to a ChoiceModelDesign
randomChoices <- function(cmd, respondents = 300) {

    n.alts <- cmd$alternatives.per.question
    n.qns <- respondents * cmd$n.questions
    chosen.alternatives <- replicate(n.qns, sample(seq(n.alts), 1))
    chosen.indices <- chosen.alternatives + seq(n.qns) * n.alts - n.alts
    chosen <- rep(FALSE, n.alts * n.qns)
    chosen[chosen.indices] <- TRUE
    return(chosen)
}

# format a design and choices for use with mlogit package
#' @importFrom mlogit mlogit.data mlogit
mlogitModel <- function(cmd, choices = NULL) {
    if (is.null(choices))
        choices <- randomChoices(cmd)

    # TODO use design.with.none
    labeled <- as.data.frame(labelDesign(cmd$design, cmd$attribute.levels))

    copies <- length(choices) / nrow(labeled)
    labeled <- labeled[rep(seq_len(nrow(labeled)), copies), ]
    labeled$Choice <- choices
    mlogit.df <- mlogit.data(labeled, choice = "Choice", shape = "long", varying = 3:ncol(labeled),
                     alt.var = "Alternative", id.var = "Question", drop.index = TRUE)

    form <- paste("Choice ~ ", paste(colnames(mlogit.df)[1:ncol(mlogit.df) - 1], collapse = "+"), "| -1")
    ml.model <- mlogit(as.formula(form), data = mlogit.df)
    return(ml.model)
}



