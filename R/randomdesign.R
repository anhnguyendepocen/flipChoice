
####################### Random method ###########################
#
# Chooses a random level for each attribute.
# Ensure same alternative is not prohibited or duplicated within a question.
randomDesign <- function(levels.per.attribute, n.questions, alternatives.per.question, prohibitions,
                         labeled.alternatives = FALSE, seed = 12345) {
    set.seed(seed)
    n.attributes <- length(levels.per.attribute)
    level.sequences <- sapply(levels.per.attribute, seq, simplify = FALSE) # list of vectors of numeric levels per attribute
    design <- array(0, dim = c(n.questions, alternatives.per.question, n.attributes))
    dimnames(design)[[3]] <- if (n.attributes == 1) list(names(levels.per.attribute)) else names(levels.per.attribute)

    for (question in seq(n.questions)) {

        i.alternative <- 1
        while (i.alternative <= alternatives.per.question) {

            new.alternative <- sapply(level.sequences, sample, 1)
            # With labeled.alternatives, first attribute is set by alternative number
            if (labeled.alternatives)
                new.alternative[1] <- i.alternative

            if (!is.null(prohibitions) && nrow(prohibitions) > 0)
            {
                # ignore new.alternative if prohibited
                prohibition.matches <- t(apply(prohibitions, 1, function(x) x == new.alternative))
                if (any(apply(prohibition.matches, 1, all)))
                    next
            }

            # add new.alternative to design
            design[question, i.alternative, ] <- new.alternative

            # ignore new.alternative if not unique within this question
            if (i.alternative == 1 || anyDuplicated(design[question, 1:i.alternative, ]) == 0)
                i.alternative <- i.alternative + 1
        }
    }
    return(flattenDesign(design))
}




