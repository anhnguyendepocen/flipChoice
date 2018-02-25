################## Shortcut method ################
#
# For each attribute, choose the least frequently used level in the design.
# Break ties by the least frequently used level in the question question, or else at random.
# Does not handle (i.e. ignores) prohibitions.
#' @importFrom nnet which.is.max
shortcutDesign <- function(levels.per.attribute, n.questions, alternatives.per.question, prohibitions,
                           labeled.alternatives, seed = 12345) {

    set.seed(seed)
    n.attributes <- length(levels.per.attribute)
    level.sequences <- sapply(levels.per.attribute, seq, simplify = FALSE) # list of vectors of numeric levels per attribute

    design <- array(0, dim = c(n.questions, alternatives.per.question, n.attributes))
    dimnames(design)[[3]] <- if (n.attributes == 1) list(names(levels.per.attribute)) else names(levels.per.attribute)
    design.counts <- sapply(levels.per.attribute, function(x) rep(0, x), simplify = FALSE) # number of times each level shown in design

    for (question in seq(n.questions)) {
        qn.counts <- sapply(levels.per.attribute, function(x) rep(0, x), simplify = FALSE) # number of times each level shown in current question

        for (alternative in seq(alternatives.per.question)) {

            for (attribute in seq(n.attributes)) {

                if (labeled.alternatives && attribute == 1)
                {
                    design[question, alternative, attribute] <- alternative
                }
                else
                {
                    min.design.count <- min(design.counts[[attribute]]) # the count of the least used level(s) of this attribute in the design
                    min.design.levels <- which(design.counts[[attribute]] == min.design.count) # the least used level(s) of this attribute in the design

                    level.counts.in.qn <- qn.counts[[attribute]][min.design.levels] # the counts of the min.design.levels for this attribute in this question
                    min.level <- min.design.levels[which.is.max(-level.counts.in.qn)] # random min.design.levels with the least level.counts.in.qn

                    design[question, alternative, attribute] <- min.level
                    design.counts[[attribute]][min.level] <- design.counts[[attribute]][min.level] + 1
                    qn.counts[[attribute]][min.level] <- qn.counts[[attribute]][min.level] + 1
                }
            }
        }
    }

    return(flattenDesign(design))
}



