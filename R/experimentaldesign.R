#' Choice modeling experimental design
#'
#' Creates choice model experimental designs according to a given algorithm.
#'
#' @param design.algorithm The algorithm used to create the
#'     design. One of \code{"Random"}, \code{"Shortcut"},
#'     \code{"Balanced overlap"} and \code{"Complete enumeration"},
#'     and \code{"Modified Fedorov"}.
#' @param attribute.levels \code{\link{list}} of \code{\link{vector}}s
#'     containing the labels of levels for each attribute, with names
#'     corresponding to the attribute labels; \emph{or} a character
#'     matrix with first row containing attribute names and subsequent
#'     rows containing attribute levels.
#' @param prior Character matrix containing prior values for the model
#'     coefficients; only used for \code{design.algorithm =
#'     "Modified Fedorov"}; see Details.
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
#'
#' @return A list with components
#' \itemize{
#' \item \code{design} - a numeric array of dimensions (number of questions by alternatives per
#' question by number of attributes) where each value is the index of a level.
#' \item \code{design.algorithm} - as per input.
#' \item \code{attribute.levels} - as per input.
#' \item \code{prohibitions} - as per input.
#' \item \code{output} - as per input.
#' }
#'
#' @details If \code{prior} is supplied and \code{design.algorithm =
#'     "Modified Fedorov"}, the number of coefficients must correspond
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
#' ChoiceModelDesign("Random", x$attribute.levels, 30, 1, 4, x$prohibitions,
#'                   0, FALSE, "Unlabelled design")
#' @export
ChoiceModelDesign <- function(
                              design.algorithm = c("Random", "Shortcut", "Balanced overlap",
                                                   "Complete enumeration", "Shortcut2", "Modified Fedorov"),
                              attribute.levels,
                              n.questions,
                              n.versions = 1,
                              alternatives.per.question,
                              prohibitions = NULL,
                              none.alternatives = 0,
                              labelled.alternatives = FALSE,
                              output = "Unlabelled design") {


    # Map the design.algorithm to the function
    design.algorithm <- match.arg(design.algorithm)
    function.name <- sub("^([A-Z])", "\\L\\1", design.algorithm, perl = TRUE)
    function.name <- gsub(" ([a-z])", "\\U\\1", function.name, perl = TRUE)
    design.function <- match.fun(paste0(function.name, "Design"))
    if (is.na(design.function))
        stop("Unrecognized design.algorithm: ", design.algorithm)

    ## NEED TO ADD CODE TO CHECK SUPPLIED ARGS ARE VALID FOR REQUESTED ALGORITHM

    # If labelled.alternatives then alternatives.per.question is calculated and not supplied
    if (labelled.alternatives)
        alternatives.per.question <- length(attribute.levels[[1]])

    if (is.null(names(attribute.levels)))
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
    args <- list(levels.per.attribute = levels.per.attribute, n.questions = n.questions * n.versions,
                 alternatives.per.question = alternatives.per.question, prohibitions = integer.prohibitions,
                 none.alternatives = none.alternatives, labelled.alternatives = labelled.alternatives)

    design <- do.call(design.function, args)

    result <- list(design = design,
                   design.with.none = addNoneAlternatives(design, none.alternatives),
                   design.algorithm = design.algorithm,
                   attribute.levels = attribute.levels,
                   prohibitions = prohibitions,
                   n.versions = n.versions,
                   none.alternatives = none.alternatives,
                   output = output)

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
        print(flattenDesign(x$design.with.none))
    else if (x$output == "Labelled design")
        print(flattenDesign(labelDesign(x$design.with.none, x$attribute.levels)))

    # Single and pairwise level balances and overlaps
    # (where overlaps is the proportion of questions that have >= 1 repeated level, by attribute)
    else if (x$output == "Balances and overlaps")
        print(balancesAndOverlaps(x))

    # TODO OUTPUT STANDARD ERRORS AND D-EFFICIENCY
    else if (x$output == "Standard errors")
        print(list(d.score = dScore(x$design),
                    d.error = DerrorHZ(flattenDesign(x$design), x$levels.per.attribute, effects = FALSE)))

    else
        stop("Unrecognized output.")
}


######################### HELPER FUNCTIONS ###########################

# Convert prohibitions from labels to indices (numeric levels)
# and expand "" or "All" to all levels of the attribute.
encodeProhibitions <- function(prohibitions, attribute.levels) {

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
    attribute.levels <- sapply(levels.per.attribute, seq)
    attribute.levels <- mapply(paste0, attributes, attribute.levels)
    names(attribute.levels) <- attributes

    # may produce duplicate prohibitions
    prohibitions <- t(replicate(n.prohibitions, sapply(attribute.levels, sample, 1)))

    experiment <- list(attribute.levels = attribute.levels, prohibitions = prohibitions)
}


# Convert an unlabelled design into a labelled design
labelDesign <- function(unlabelled.design, attribute.levels) {

    labelled.design <- array(character(0), dim = dim(unlabelled.design))
    dimnames(labelled.design)[[3]] = dimnames(unlabelled.design)[[3]]
    for (i in 1:length(attribute.levels))
        labelled.design[, , i] <- attribute.levels[[i]][unlabelled.design[, , i]]
    return(labelled.design)
}


# Compute one and two-way level balances or extract from existing object.
balancesAndOverlaps <- function(cmd) {

    singles <- if (!is.null(cmd$singles)) cmd$singles else singleLevelBalances(cmd$design, names(cmd$attribute.levels))

    pairs <- if (!is.null(cmd$pairs)) cmd$pairs else pairLevelBalances(cmd$design, names(cmd$attribute.levels))

    # label the levels
    singles <- labelSingleBalanceLevels(singles, cmd$attribute.levels)
    pairs <- labelPairBalanceLevels(pairs, cmd$attribute.levels)

    # flatten pairwise list of list and remove unused
    pairs <- unlist(pairs, recursive = FALSE)
    pairs <- pairs[!is.na(pairs)]

    overlaps = overlaps(cmd$design, names(cmd$attribute.levels))

    return(list(singles = singles, pairs = pairs, overlaps = overlaps))
}


singleLevelBalances <- function(design, attribute.names) {
    singles <- apply(design, 3, table)
    names(singles) <- attribute.names
    return(singles)
}

pairLevelBalances <- function(design, attribute.names) {
    n.attributes <- dim(design)[3]
    pairs <- replicate(n.attributes, rep(list(NA), n.attributes), simplify = FALSE)
    for (i in 1:(n.attributes - 1))
        for (j in (i + 1):n.attributes) {
            pairs[[i]][[j]] <- table(design[, , i], design[, , j])
            names(pairs[[i]])[[j]] <- paste0(attribute.names[i], "/", attribute.names[j])
        }
    return(pairs)
}

labelSingleBalanceLevels <- function(singles, attribute.levels) {
    return(mapply(function(x, y) {names(x) <- y; x}, singles, attribute.levels))
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

overlaps <- function(design, attribute.names) {
    # matrix of qn by attribute indicting whether any level of that attribute is repeated in that qn
    overlaps <- apply(design, c(1, 3), anyDuplicated) != 0
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

addNoneAlternatives <- function(design, none.alternatives) {
    if (none.alternatives == 0)
        return(design)

    none.dim <- dim(design)
    none.dim[2] <- none.dim[2] + none.alternatives
    design.with.none <- array(NA, dim = none.dim, dimnames = dimnames(design))
    design.with.none[, 1:dim(design)[2], ] <- design
    return(design.with.none)
}


