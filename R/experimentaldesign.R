#' Choice modelling experimental design
#'
#' Creates choice model experimental designs according to a given algorithm.
#'
#' @param design.algorithm The algorithm used to create the design. One of \code{"Random"},
#' \code{"Shortcut"}, \code{"Balanced overlap"} and \code{"Complete enumeration"}.
#' @param attribute.levels \code{\link{list}} of \code{\link{vector}}s of the labels of
#' levels for each attribute.
#' @param n.questions Integer; the number of questions asked to each respondent.
#' @param alternatives.per.question Integer; the number of alternative products
#' shown in each question. Ignored if \code{"labelled.alternatives"} is TRUE.
#' @param prohibitions Character \code{\link{matrix}} where each row is a prohibited
#' alternative consisting of the levels of each attribute. If a level is missing or
#' is \code{"All"} then all levels of that attribute in combination with the other
#' attribute levels are prohibited.
#' @param none.alternative Logical; whether to show an alternative of 'None' in all questions.
#' @param labelled.alternatives Logical; whether the first attribute labels the alternatives.
#' @param output TODO
#'
#' @return A list with components
#' \itemize{
#' \item \code{design} - an array of dimensions (number of questions by alternatives per
#' question by number of attributes) where each value is the index of a level.
#' \item \code{...} - other diagnostics according to \code{design.algorithm}.
#' }
#' @export
ChoiceModelDesign <- function(design.algorithm,
                              attribute.levels,
                              n.questions,
                              alternatives.per.question,
                              prohibitions = NULL,
                              none.alternative = FALSE,
                              labelled.alternatives = FALSE,
                              output = "Unlabelled design") {

    algorithms <- c("Random", "Shortcut", "Balanced overlap", "Complete enumeration")
    function.names <- c("random", "shortcut", "balanced", "enumerated")
    design.function <- function.names[match(design.algorithm, algorithms)]
    if (is.na(design.function))
        stop("Unrecognized design.algorithm: ", design.algorithm)

    if (labelled.alternatives)
        alternatives.per.question = length(attribute.levels[[1]])
    # TODO ensure levels of first attribute are cycled through in order of each qn

    if (output == "Attributes and levels")
    {
        max.levels <- max(sapply(attribute.levels, length))
        levels.table <- sapply(attribute.levels, function (x) c(x, rep("", max.levels - length(x))))
        rownames(levels.table) <- paste("Level", seq(max.levels))
        return(levels.table)
    }

    prohibitions <- encodeProhibitions(prohibitions, attribute.levels)
    levels.per.attribute <- sapply(attribute.levels, length)
    names(levels.per.attribute) <- names(attribute.levels)

    if (output == "Prohibitions")
        return(prohibitions)

    args <- list(levels.per.attribute = levels.per.attribute, n.questions = n.questions,
                 alternatives.per.question = alternatives.per.question, prohibitions = prohibitions)
    result <- do.call(paste0(design.function, "Design"), args)

    if (output == "Unlabelled design")
        return(flattenDesign(result$design))

    if (output == "Labelled design")
        return(flattenDesign(labelDesign(result$design, attribute.levels)))

    if (output == "Level balances")
        return(levelBalances(result, attribute.levels))

    # TODO OUTPUT OVERLAPS
    if (output == "Overlaps")
        return(NULL)

    # TODO OUTPUT STANDARD ERRORS AND D-EFFICIENCY
    # https://www.sawtoothsoftware.com/help/lighthouse-studio/manual/hid_web_cbc_designs_6.html
    # https://www.sawtoothsoftware.com/help/lighthouse-studio/manual/estimating_utilities_with_logi.html
    if (output == "Standard errors")
    {
        return(list(d.score = dScore(result$design),
               d.error = DerrorHZ(flattenDesign(result$design), levels.per.attribute, effects = FALSE)))
    }

    stop("Unrecognized output.")
}



################## HELPER FUNCTIONS ###################

# Convert prohibitions from labels to indices (numeric levels)
# and expand "" or "All" to all levels.
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
#' Creates attributes and levels of an experiment with random prohibitions.
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

# Compute one and two-way level balances or extract from exiting list.
levelBalances <- function(design.list, attribute.levels) {

    singles <- if (!is.null(design.list$singles)) design.list$singles else singleLevelBalances(design.list$design,
                                                                                               names(attribute.levels))

    pairs <- if (!is.null(design.list$pairs)) design.list$pairs else pairLevelBalances(design.list$design,
                                                                                       names(attribute.levels))

    # label the levels
    singles <- labelSingleBalanceLevels(singles, attribute.levels)
    pairs <- labelPairBalanceLevels(pairs, attribute.levels)

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



################## Shortcut method ################
#
# TODO EXPLAIN HOW THIS WORKS
# TODO ADD PROHIBITIONS
shortcutDesign <- function(levels.per.attribute, n.questions, alternatives.per.question, prohibitions) {

    n.attributes <- length(levels.per.attribute)
    level.sequences <- sapply(levels.per.attribute, seq) # list of vectors of numeric levels per attribute

    design <- array(0, dim = c(n.questions, alternatives.per.question, n.attributes))
    dimnames(design)[[3]] <- names(levels.per.attribute)
    design.counts <- sapply(levels.per.attribute, function(x) rep(0, x)) # number of times each level shown in design

    for (question in seq(n.questions)) {
        qn.counts <- sapply(levels.per.attribute, function(x) rep(0, x)) # number of times each level shown in current question

        for (alternative in seq(alternatives.per.question)) {

            for (attribute in seq(n.attributes)) {
                min.design.count <- min(design.counts[[attribute]]) # the count of the least used level(s) of this attribute in the design
                min.design.levels <- which(design.counts[[attribute]] == min.design.count) # the least used level(s) of this attribute in the design

                level.counts.in.qn <- qn.counts[[attribute]][min.design.levels] # the counts of the min.design.levels for this attribute in this question
                min.level <- min.design.levels[which.min(level.counts.in.qn)] # the first min.design.levels with the least level.counts.in.qn

                design[question, alternative, attribute] <- min.level
                design.counts[[attribute]][min.level] <- design.counts[[attribute]][min.level] + 1
                qn.counts[[attribute]][min.level] <- qn.counts[[attribute]][min.level] + 1
            }
        }
    }
    return(list(design = design))
}


####################### Random method ###########################
#
# Chooses a random level for each attribute.
# Ensure same alternative is not prohibited or duplicated within a question.
randomDesign <- function(levels.per.attribute, n.questions, alternatives.per.question, prohibitions) {

    set.seed(12345)
    n.attributes <- length(levels.per.attribute)
    level.sequences <- sapply(levels.per.attribute, seq) # list of vectors of numeric levels per attribute
    design <- array(0, dim = c(n.questions, alternatives.per.question, n.attributes))
    dimnames(design)[[3]] <- names(levels.per.attribute)

    for (question in seq(n.questions)) {

        i.alternative <- 1
        while (i.alternative <= alternatives.per.question) {

            new.alternative <- sapply(level.sequences, sample, 1)

            # ignore new.alternative if prohibited
            if (any(sapply(prohibitions, identical, new.alternative)))
                next

            # ignore new.alternative if not unique within this question
            if (i.alternative > 1 && anyDuplicated(design[question, 1:i.alternative, ]))
                next

            # use new.alternative
            design[question, i.alternative, ] <- new.alternative
            i.alternative <- i.alternative + 1
        }
    }
    return(list(design = design))
}






