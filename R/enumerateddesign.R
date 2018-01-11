################## Complete enumeration method #######################
#
#https://sawtoothsoftware.com/forum/5416/there-mathematical-framework-balanced-overlap-short-design

# Return the difference between the largest and smallest values in a vector or array
numRange <- function(x) {
    r <- range(x)
    return(r[2] - r[1])
}

# Increment the counts of levels per attribute after adding a new alternative to the design
addSingles <- function(alternative, singles) {
    return(mapply(function(x, y) {x[y] <- x[y] + 1
    x}, singles, alternative))
}

# Return the sum of the ranges of level counts per attribute when new alternative is added to the design
singleCost <- function(alternative, singles) {
    return(sum(sapply(addSingles(alternative, singles), numRange)))
}

# Increment the pairwise counts of levels after adding a new alternative to the design
addPairs <- function(alternative, pairs) {
    for (i in 1:(length(alternative) - 1))
        for (j in (i + 1):length(alternative))
            pairs[[i]][[j]][alternative[i], alternative[j]] <- pairs[[i]][[j]][alternative[i], alternative[j]] + 1
        return(pairs)
}

# Return the sum of the ranges of pairwise level counts when a new alternative is added to the design
pairCost <- function(alternative, pairs) {
    cost <- 0
    for (i in 1:(length(alternative) - 1)) {
        for (j in (i + 1):length(alternative)) {
            pairs[[i]][[j]][alternative[i], alternative[j]] <- pairs[[i]][[j]][alternative[i], alternative[j]] + 1
            cost <- cost + numRange(pairs[[i]][[j]])
        }
    }
    return(cost)
}


# Calculate the total cost as a weighted sum
totalCost <- function(alternative, singles, pairs, qn.counts) {

    single.cost <- singleCost(alternative, singles)
    pair.cost <- pairCost(alternative, pairs)
    question.overlap.cost <- singleCost(alternative, qn.counts)

    # TODO relative weightings of these costs
    cost <- 1 * pair.cost + 1 * single.cost + 1 * question.overlap.cost
    return(cost)
}

######################################################################################


enumeratedDesign <- function(levels.per.attribute, n.questions, alternatives.per.question, prohibitions) {

    set.seed(12345)

    # initialize empty design
    n.attributes <- length(levels.per.attribute)
    level.sequences <- sapply(levels.per.attribute, seq) # list of vectors of numeric levels per attribute
    design <- array(0, dim = c(n.questions, alternatives.per.question, n.attributes))

    # enumerate all alternatives
    enumeration <- expand.grid(level.sequences)

    # remove prohibited alternatives
    colnames(prohibitions) <- colnames(enumeration)
    dups <- duplicated(rbind(enumeration, prohibitions), fromLast = TRUE)[1:length(enumeration)]
    enumeration <- as.matrix(enumeration[!dups, ])

    # create a list of vectors to count single level occurences
    levels.per.attribute <- sapply(level.sequences, length)
    singles <- sapply(levels.per.attribute, function(x) rep(0, x))
    names(singles) <- names(levels.per.attribute)

    # create a list of lists of matrices to count pairwise level occurences
    # pairs[[i]][[j]]] contains the counts of attribute i and attribute j
    # if i <= j then pairs[[i]][[j]] == NA and not used
    pairs <- replicate(n.attributes, rep(list(NA), n.attributes), simplify = FALSE)
    for (i in 1:(n.attributes - 1))
        for (j in (i + 1):n.attributes) {
            pairs[[i]][[j]] <- matrix(0, nrow = levels.per.attribute[i], ncol = levels.per.attribute[j])
            names(pairs[[i]])[[j]] <- paste0(names(levels.per.attribute)[i], "/", names(levels.per.attribute)[j])
        }

    for (question in seq(n.questions)) {

        # count number of times each level is shown in current question
        qn.counts <- sapply(levels.per.attribute, function(x) rep(0, x))

        for (i.alternative in seq(alternatives.per.question)) {

            costs <- apply(enumeration, 1, totalCost, singles, pairs, qn.counts)
            best.alternative <- enumeration[which.min(costs), ]

            # add best.alternative to design
            design[question, i.alternative, ] <- best.alternative

            # increment pairs, singles, qn.overlap
            singles <- addSingles(best.alternative, singles)
            pairs <- addPairs(best.alternative, pairs)
            qn.counts <- addSingles(best.alternative, qn.counts)
        }
    }

    return(list(design = design, singles = singles, pairs = pairs))
}

