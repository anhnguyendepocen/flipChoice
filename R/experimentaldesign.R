#' Choice modelling experimental design
#'
#' Creates choice model experimental designs according to a given algorithm.
#'
#' @param design.algorithm The algorithm used to create the design. One of \code{"random"},
#' \code{"shortcut"} and \code{"enumerated"}.
#' @param attribute.levels \code{\link{list}} of \code{\link{vector}}s of the labels of
#' levels for each attribute.
#' @param n.questions Integer; the number of questions asked to each respondent.
#' @param alternatives.per.question Integer; the number of alternative products
#' shown in each question.
#' @param prohibitions \code{\link{list}} of \code{\link{vector}}s where each
#' \code{\link{vector}} is a prohibited alternative consisting of the indices of each
#' prohibited level.
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
                              output = "Unlabelled design") {

    if (output == "Attributes and levels")
    {
        max.levels <- max(sapply(attribute.levels, length))
        levels.table <- sapply(attribute.levels, function (x) c(x, rep("", max.levels - length(x))))
        rownames(levels.table) <- paste("Level", seq(max.levels))
        return(levels.table)
    }

    if (output == "Prohibitions")
        return(prohibitions)

    prohibitions <- encodeProhibitions(prohibitions, attribute.levels)
    levels.per.attribute <- sapply(attribute.levels, length)

    args <- list(levels.per.attribute = levels.per.attribute, n.questions = n.questions,
                 alternatives.per.question = alternatives.per.question, prohibitions = prohibitions)
    result <- do.call(paste0(design.algorithm, "Design"), args)

    if (output == "Unlabelled design")
        return(result$design)

    # TODO LABEL THE DESIGN
}



################## HELPER FUNCTIONS ###################

# Convert prohibitions from labels to indices (numeric levels).
#
# TODO If level is missing (empty string or NA) then all levels of that attribute are prohibted.
# TODO vectorise code
encodeProhibitions <- function(char.prohibitions, attribute.levels) {

    # TODO convert char.prohibitions to a data.frame
    i.prohibitions <- replicate(length(char.prohibitions), rep(NULL, length(attribute.levels)))

    for (i in 1:length(char.prohibitions)) {

        if (length(char.prohibitions[[i]]) != length(attribute.levels))
            stop("Prohibition has wrong number of levels.")

        for (j in 1:length(attribute.levels)) {

            level <- match(char.prohibitions[[i]][j], attribute.levels[[j]])
            if (is.na(level))
                stop(paste("Level of prohibition", char.prohibitions[[i]][j], "not recognized."))
            i.prohibitions[[i]][j] <- level
        }
    }

    return(i.prohibitions)
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
#' \item \code{prohibitions} - a \code{\link{list}} of \code{\link{vector}}s where each
#' \code{\link{vector}} is a prohibited alternative consisting of the labels of each
#' prohibited level.
#' }
#' @export
CreateExperiment <- function(levels.per.attribute, n.prohibitions = 0) {

    set.seed(12345)
    attributes <- LETTERS[1:length(levels.per.attribute)]
    attribute.levels <- sapply(levels.per.attribute, seq)
    attribute.levels <- mapply(paste0, attributes, attribute.levels)
    names(attribute.levels) <- attributes

    # may produce duplicate prohibitions
    prohibitions <- replicate(n.prohibitions, sapply(attribute.levels, sample, 1))
    prohibitions <- split(prohibitions, rep(1:ncol(prohibitions), each = nrow(prohibitions)))

    experiment <- list(attribute.levels = attribute.levels, prohibitions = prohibitions)
}


################## Shortcut method ################
#
# TODO EXPLAIN HOW THIS WORKS
# TODO ADD PROHIBITIONS
shortcutDesign <- function(levels.per.attribute, n.questions, alternatives.per.question, prohibitions) {

    n.attributes <- length(levels.per.attribute)
    level.sequences <- sapply(levels.per.attribute, seq) # list of vectors of numeric levels per attribute

    design <- array(0, dim = c(n.questions, alternatives.per.question, n.attributes))
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






