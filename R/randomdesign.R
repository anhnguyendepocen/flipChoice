
####################### Random method ###########################
#
# Chooses a random level for each attribute.
# Ensure same alternative is not prohibited or duplicated within a question.
randomDesign <- function(levels.per.attribute, n.questions, alternatives.per.question, prohibitions,
                         none.alternatives = 0, labelled.alternatives = FALSE) {

    # TODO use labelled,alternatives
    # TODO use none.alternatives

    set.seed(12345)
    n.attributes <- length(levels.per.attribute)
    level.sequences <- sapply(levels.per.attribute, seq) # list of vectors of numeric levels per attribute
    design <- array(0, dim = c(n.questions, alternatives.per.question, n.attributes))
    dimnames(design)[[3]] <- names(levels.per.attribute)

    for (question in seq(n.questions)) {

        i.alternative <- 1
        while (i.alternative <= alternatives.per.question) {

            new.alternative <- sapply(level.sequences, sample, 1)

            if (nrow(prohibitions) > 0)
            {
                # ignore new.alternative if prohibited
                prohibition.matches <- t(apply(prohibitions, 1, function(x) x == new.alternative))
                if (any(apply(prohibition.matches, 1, all)))
                    next
            }

            # ignore new.alternative if not unique within this question
            if (i.alternative > 1 && anyDuplicated(design[question, 1:i.alternative, ]))
                next

            # use new.alternative
            design[question, i.alternative, ] <- new.alternative
            i.alternative <- i.alternative + 1
        }
    }
    return(design)
}




################## Shortcut method ################
#
# TODO EXPLAIN HOW THIS WORKS
# TODO ADD PROHIBITIONS
shortcutDesign <- function(levels.per.attribute, n.questions, alternatives.per.question, prohibitions,
                           none.alternatives = 0, labelled.alternatives = FALSE) {

    # TODO use labelled,alternatives
    # TODO use none.alternatives

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
    return(design)
}




