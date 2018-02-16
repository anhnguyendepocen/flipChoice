

completeEnumerationDesign <- function(levels.per.attribute, n.questions, alternatives.per.question, prohibitions,
                                      labeled.alternatives) {
    return(enumeratedDesign(levels.per.attribute, n.questions, alternatives.per.question, prohibitions,
                            labeled.alternatives, cost.weightings = c(1, 1, 1)))
}

balancedOverlapDesign <- function(levels.per.attribute, n.questions, alternatives.per.question, prohibitions,
                                      labeled.alternatives) {
    return(enumeratedDesign(levels.per.attribute, n.questions, alternatives.per.question, prohibitions,
                            labeled.alternatives, cost.weightings = c(6, 6, 1)))
}


################## Complete enumeration method #######################
#
# Similar to https://sawtoothsoftware.com/forum/5416/there-mathematical-framework-balanced-overlap-short-design
#
#' @importFrom nnet which.is.max
enumeratedDesign <- function(levels.per.attribute, n.questions, alternatives.per.question, prohibitions,
                             labeled.alternatives, cost.weightings) {

    set.seed(12345)

    # initialize empty design
    n.attributes <- length(levels.per.attribute)
    level.sequences <- sapply(levels.per.attribute, seq, simplify = FALSE) # list of vectors of numeric levels per attribute
    design <- array(0, dim = c(n.questions, alternatives.per.question, n.attributes))
    dimnames(design)[[3]] <- if (n.attributes == 1) list(names(levels.per.attribute)) else names(levels.per.attribute)

    # enumerate all alternatives
    enumeration <- as.matrix(expand.grid(level.sequences))

    # remove prohibited alternatives
    if (!is.null(prohibitions) && length(prohibitions) != 0)
    {
        colnames(prohibitions) <- colnames(enumeration)
        dups <- duplicated(rbind(enumeration, prohibitions), fromLast = TRUE)[1:nrow(enumeration)]
        enumeration <- as.matrix(enumeration[!dups, ])
    }

    # create a list of vectors to count single level occurences
    levels.per.attribute <- sapply(level.sequences, length)
    singles <- sapply(levels.per.attribute, function(x) rep(0, x), simplify = FALSE)
    names(singles) <- names(levels.per.attribute)

    # create a list of lists of matrices to count pairwise level occurences
    # pairs[[i]][[j]]] contains the counts of attribute i and attribute j
    # if i <= j then pairs[[i]][[j]] == NA and not used
    pairs <- replicate(n.attributes, rep(list(NA), n.attributes), simplify = FALSE)
    if (n.attributes > 1)
    {
        for (i in 1:(n.attributes - 1))
            for (j in (i + 1):n.attributes) {
                pairs[[i]][[j]] <- matrix(0, nrow = levels.per.attribute[i], ncol = levels.per.attribute[j])
                names(pairs[[i]])[[j]] <- paste0(names(levels.per.attribute)[i], "/", names(levels.per.attribute)[j])
            }
    }
    pairs.singles.ratio <- if (n.attributes > 1) (n.attributes - 1) / 2 else 1

    for (question in seq(n.questions)) {

        # count number of times each level is shown in current question
        qn.counts <- sapply(levels.per.attribute, function(x) rep(0, x), simplify = FALSE)

        for (i.alternative in seq(alternatives.per.question)) {

            if (labeled.alternatives)
                valid.enumerations <- enumeration[enumeration[, 1] == i.alternative, ]
            else
                valid.enumerations <- enumeration

            # precompute the cost of incrementing every level of every attribute
            # which is faster than computing the cost of each enumeration indivdually
            singles.costs <- sapply(singles, precomputeCosts, simplify = FALSE)
            qn.counts.costs <- sapply(qn.counts, precomputeCosts, simplify = FALSE)
            pairs.costs <- precomputePairsCosts(pairs)
            costs <- apply(valid.enumerations, 1, totalCostPrecomputed, singles.costs, pairs.costs,
                           qn.counts.costs, pairs.singles.ratio, cost.weightings)

            # break ties at random instead of taking first minimum
            best.alternative <- valid.enumerations[which.is.max(-costs), ]

            # add best.alternative to design
            design[question, i.alternative, ] <- best.alternative

            # increment pairs, singles, qn.overlap
            singles <- addSingles(best.alternative, singles)
            pairs <- addPairs(best.alternative, pairs)
            qn.counts <- addSingles(best.alternative, qn.counts)
        }
    }

    return(flattenDesign(design))
}


####### Helper functions for enumeratedDesign ################################################

# Return the difference between the largest and smallest values in a vector or array
numRange <- function(x) {
    r <- range(x)
    return(r[2] - r[1])
}

# Increment the counts of levels per attribute after adding a new alternative to the design
addSingles <- function(alternative, singles) {
    return(mapply(function(x, y) {x[y] <- x[y] + 1
    x}, singles, alternative, SIMPLIFY = FALSE))
}


# Increment the pairwise counts of levels after adding a new alternative to the design
addPairs <- function(alternative, pairs) {
    if (length(alternative) > 1)
    {
        for (i in 1:(length(alternative) - 1))
            for (j in (i + 1):length(alternative))
                pairs[[i]][[j]][alternative[i], alternative[j]] <- pairs[[i]][[j]][alternative[i], alternative[j]] + 1
        return(pairs)
    }
}



# Calculate the total cost as a weighted sum
totalCostPrecomputed <- function(alternative, singles.costs, pairs.costs, qn.counts.costs, pairs.singles.ratio, cost.weightings) {

    single.cost <- singlePrecomputed(alternative, singles.costs)
    # scale the pair cost down by the ratio of the number of pairs to the number of singles
    # otherwise pair cost will dominate as number of attributes increases
    pair.cost <- pairPrecomputed(alternative, pairs.costs) / pairs.singles.ratio
    question.overlap.cost <- singlePrecomputed(alternative, qn.counts.costs)

    cost <- sum(c(pair.cost, single.cost, question.overlap.cost) * cost.weightings)
    return(cost)
}




precomputeCosts <- function(single) {
    n <- length(single)
    range <- range(single)
    min <- range[1]
    max <- range[2]
    if (min == max)
        return (rep(1, n))

    min.count <- sum(single == min)
    costs <- rep((max - min)^2, n)
    costs[single == max] <- (max + 1 - min)^2
    if (min.count == 1)
        costs[single == min] <- (max - min - 1)^2
    return(costs)
}


precomputePairsCosts <- function(pairs) {

    for (i in 1:(length(pairs))) {
        for (j in 1:length(pairs[[i]])) {

            pair <- pairs[[i]][[j]]
            if(length(dim(pairs)) == 0)
                next

            rows <- NROW(pair)
            cols <- NCOL(pair)
            range <- range(pair)
            min <- range[1]
            max <- range[2]
            if (min == max) {
                costs <- matrix(1, nrow = rows, ncol = cols)
                next
            }

            min.count <- sum(pair == min)
            costs <- matrix((max - min)^2, nrow = rows, ncol = cols)
            costs[pair == max] <- (max + 1 - min)^2
            if (min.count == 1)
                costs[pair == min] <- (max - min - 1)^2

            pairs[[i]][[j]] <- costs
        }
    }
    return(pairs)
}



singlePrecomputed <- function(alternative, precomputed) {
    return(sum(mapply(function(x, y) y[x], alternative, precomputed)))
}

pairPrecomputed <- function(alternative, precomputed) {
    cost <- 0
    if (length(alternative) > 1)
    {
        for (i in 1:(length(alternative) - 1)) {
            for (j in (i + 1):length(alternative)) {
                cost <- cost + precomputed[[i]][[j]][alternative[i], alternative[j]]
            }
        }
    }
    return(cost)
}

