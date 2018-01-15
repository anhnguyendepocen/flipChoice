#' Choice modelling experimental design
#'
#' Creates choice model experimental designs according to a given algorithm.
#'
#' @param design.algorithm The algorithm used to create the design. One of \code{"Random"},
#' \code{"Shortcut"}, \code{"Balanced overlap"} and \code{"Complete enumeration"}.
#' @param attribute.levels \code{\link{list}} of \code{\link{vector}}s containing the labels of
#' levels for each attribute. Named with the attribute labels.
#' @param n.questions Integer; the number of questions asked to each respondent.
#' @param alternatives.per.question Integer; the number of alternative products
#' shown in each question. Ignored if \code{"labelled.alternatives"} is TRUE.
#' @param prohibitions Character \code{\link{matrix}} where each row is a prohibited
#' alternative consisting of the levels of each attribute. If a level is \code{""} or
#' is \code{"All"} then all levels of that attribute in combination with the other
#' specified attribute levels are prohibited.
#' @param none.alternatives Integer; the number of 'None' in all questions.
#' @param labelled.alternatives Logical; whether the first attribute labels the alternatives.
#' @param output One of \code{"Attributes and levels"}, \code{"Prohibitions"},
#' \code{"Unlabelled design"}, \code{"Labelled design"}, \code{"Level balances"},
#' \code{"Overlaps"} or \code{"Standard errors"}.
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
#' @examples
#' x <- CreateExperiment(c(3, 5, 7, 10), 20)
#' ChoiceModelDesign("Random", x$attribute.levels, 30, 4, x$prohibitions, FALSE, FALSE, "Unlabelled design")
#' @export
ChoiceModelDesign <- function(design.algorithm,
                              attribute.levels,
                              n.questions,
                              alternatives.per.question,
                              prohibitions = NULL,
                              none.alternatives = 0,
                              labelled.alternatives = FALSE,
                              output = "Unlabelled design") {


    # Map the design.algorithm to the function
    algorithms <- c("Random", "Shortcut", "Balanced overlap", "Complete enumeration")
    function.names <- c("random", "shortcut", "balanced", "enumerated")
    design.function <- function.names[match(design.algorithm, algorithms)]
    if (is.na(design.function))
        stop("Unrecognized design.algorithm: ", design.algorithm)


    # If labelled.alternatives then alternatives.per.question is calculated and not supplied
    if (labelled.alternatives)
        alternatives.per.question <- length(attribute.levels[[1]]) + none.alternatives

    if (is.null(names(attribute.levels)))
        names(attribute.levels) <- paste("Attribute", seq(length(attribute.levels)))


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


    # Convert data to numeric
    # TODO separate expansion from conversion to numeric and add former to result
    unlabelled.prohibitions <- encodeProhibitions(prohibitions, attribute.levels)
    levels.per.attribute <- sapply(attribute.levels, length)
    names(levels.per.attribute) <- names(attribute.levels)

    # Call the algorithm to create the design
    args <- list(levels.per.attribute = levels.per.attribute, n.questions = n.questions,
                 alternatives.per.question = alternatives.per.question, prohibitions = unlabelled.prohibitions,
                 none.alternatives = none.alternatives, labelled.alternatives = labelled.alternatives)

    result <- list(design = do.call(paste0(design.function, "Design"), args),
                   design.algorithm = design.algorithm,
                   attribute.levels = attribute.levels,
                   prohibitions = prohibitions,
                   output = output)

    class(result) <- "ChoiceModelDesign"
    return(result)
}


#' @export
print.ChoiceModelDesign <- function(cmd, ...) {

    # Return a table with attributes along the columns and levels along the rows
    if (cmd$output == "Attributes and levels")
    {
        max.levels <- max(sapply(cmd$attribute.levels, length))
        levels.table <- sapply(cmd$attribute.levels, function (x) c(x, rep("", max.levels - length(x))))
        rownames(levels.table) <- paste("Level", seq(max.levels))
        print(levels.table)
    }

    else if (cmd$output == "Prohibitions")
        print(cmd$prohibitions)

    # Return the design
    else if (cmd$output == "Unlabelled design")
        print(flattenDesign(cmd$design))
    else if (cmd$output == "Labelled design")
        print(flattenDesign(labelDesign(cmd$design, cmd$attribute.levels)))

    # Return some diagnostic
    else if (cmd$output == "Level balances")
        print(levelBalances(cmd))

    # TODO OUTPUT A MEASURE OF HOW MANY QUESTIONS CONTAIN TASKS WITH THE SAME LEVEL
    else if (cmd$output == "Overlaps")
        print(NULL)

    # TODO OUTPUT STANDARD ERRORS AND D-EFFICIENCY
    else if (cmd$output == "Standard errors")
    {
        print(list(d.score = dScore(cmd$design),
                    d.error = DerrorHZ(flattenDesign(cmd$design), levels.per.attribute, effects = FALSE)))
    }
    else
        stop("Unrecognized output.")
}


######################### HELPER FUNCTIONS ###########################

# Convert prohibitions from labels to indices (numeric levels)
# and expand "" or "All" to all levels of the attribute.
encodeProhibitions <- function(prohibitions, attribute.levels) {

    prohibitions[prohibitions == ""] <- "All"
    prohibitions <- data.frame(prohibitions)

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
levelBalances <- function(cmd) {

    singles <- if (!is.null(cmd$singles)) cmd$singles else singleLevelBalances(cmd$design, names(cmd$attribute.levels))

    pairs <- if (!is.null(cmd$pairs)) cmd$pairs else pairLevelBalances(cmd$design, names(cmd$attribute.levels))

    # label the levels
    singles <- labelSingleBalanceLevels(singles, cmd$attribute.levels)
    pairs <- labelPairBalanceLevels(pairs, cmd$attribute.levels)

    # flatten pairwise list of list and remove unused
    pairs <- unlist(pairs, recursive = FALSE)
    pairs <- pairs[!is.na(pairs)]

    return(list(singles = singles, pairs = pairs))
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
    # maybe there is a better way
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

flattenDesign <- function(design) {
    n.qns <- dim(design)[1]
    n.alts <- dim(design)[2]
    flattened <- matrix(design, nrow = n.qns * n.alts)
    flattened <- cbind(rep(seq(n.qns), each = n.alts), rep(seq(n.alts), n.qns), flattened)
    colnames(flattened) <- c("Question", "Alternative", dimnames(design)[[3]])
    return(flattened)
}



