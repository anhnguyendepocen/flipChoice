#' Choice modeling experimental design
#'
#' Creates choice model experimental designs according to a given algorithm.
#'
#' @param design.algorithm The algorithm used to create the
#'     design. One of \code{"Random"}, \code{"Shortcut"},
#'     \code{"Balanced overlap"} and \code{"Complete enumeration"},
#'     and \code{"Efficient"}.
#' @param attribute.levels \code{\link{list}} of \code{\link{vector}}s
#'     containing the labels of levels for each attribute, with names
#'     corresponding to the attribute labels; \emph{or} a character
#'     matrix with first row containing attribute names and subsequent
#'     rows containing attribute levels.
#' @param prior Character matrix containing prior values for the model
#'     coefficients; only used for \code{design.algorithm ==
#'     "Efficient"}; see Details.
#' @param n.questions Integer; the number of questions asked to each
#'     respondent.
#' @param n.versions Integer; the number of versions of the survey to
#'     create.
#' @param alternatives.per.question Integer; the number of alternative
#'     products shown in each question. Ignored if
#'     \code{"labeled.alternatives"} is TRUE.
#' @param prohibitions Character \code{\link{matrix}} where each row
#'     is a prohibited alternative consisting of the levels of each
#'     attribute. If a level is \code{""} or is \code{"All"} then all
#'     levels of that attribute in combination with the other
#'     specified attribute levels are prohibited.
#' @param none.alternatives Integer; the number of 'None' in all
#'     questions.
#' @param labeled.alternatives Logical; whether the first attribute
#'     labels the alternatives.
#' @param output One of \code{"Labeled design"} or \code{"Inputs"}.
#' @param seed Integer; random seed to be used by the algorithms.
#' @return A list with components
#' \itemize{
#' \item \code{design} - a numeric array of dimensions (number of questions by alternatives per
#' question by number of attributes) where each value is the index of a level. Ignoring any none
#' alternatives.
#' \item \code{design.with.none} - as per \code{design} except one additional row per none alternative
#' is added to each question with \code{NA} for all attribute levels.
#' \item \code{labeled.design} - as per \code{design.with.none} except indices of levels are
#' replaced by their labels.
#' \item \code{design.algorithm} - as per input.
#' \item \code{attribute.levels} - as per input.
#' \item \code{prohibitions} - as per input.
#' \item \code{n.questions} - as per input.
#' \item \code{n.versions} - as per input.
#' \item \code{alternatives.per.question} - as per input.
#' \item \code{none.alternatives} - as per input.
#' \item \code{output} - as per input.
#' \item \code{db.error} - the Db-error of \code{design}.
#' \item \code{d.error} - the D-error of \code{design}.
#' \item \code{model.matrix} - the model matrix of dummy coded variables for each alternative
#' in every choice set.
#' \item \code{balances.and.overlaps} a list with components
#'     \itemize{
#'     \item\code{singles} a \code{list} of the counts of each level per attribute.
#'     \item\code{pairs} a \code{list} of the counts of pairwise occurences of levels
#'     for each pair of attributes.
#'     \item\code{overlaps} a \code{vector} of the percentage of questions that include
#'     one or more duplicated level per attribute.
#'     }
#' }
#'
#' @details If \code{prior} is supplied and \code{design.algorithm ==
#'     "Efficient"}, the number of coefficients must correspond
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
#'     alternatives.per.question = 4, prohibitions = x$prohibitions)
#' @importFrom utils getFromNamespace modifyList
#' @export
ChoiceModelDesign <- function(design.algorithm = c("Random", "Shortcut", "Balanced overlap", "Complete enumeration",
                                                    "Efficient"),
                              attribute.levels = NULL,
                              prior = NULL,
                              n.questions,
                              n.versions = 1,
                              alternatives.per.question,
                              prohibitions = NULL,
                              none.alternatives = 0,
                              labeled.alternatives = FALSE,
                              output = "Labeled design",
                              seed = 54123) {


    ## Map the design.algorithm to the function
    design.algorithm <- match.arg(design.algorithm)
    function.name <- sub("^([A-Z])", "\\L\\1", design.algorithm, perl = TRUE)
    function.name <- gsub(" ([[:alpha:]])", "\\U\\1", function.name, perl = TRUE)
    design.function <- getFromNamespace(paste0(function.name, "Design"),
                                        ns = "flipChoice")

    if (!is.null(prior) && design.algorithm != "Efficient")
        warning(gettextf("Prior data can only be used with algorithm %s and will be ignored.",
                         sQuote("Efficient")))


    if (is.list(attribute.levels))
    {
        if (is.null(names(attribute.levels)))
            names(attribute.levels) <- paste("Attribute", seq(length(attribute.levels)))
        levels.per.attribute <- sapply(attribute.levels, length)
        names(levels.per.attribute) <- names(attribute.levels)
    }
    else if (is.character(attribute.levels))
    {
        parsed.data <- parsePastedData(attribute.levels, n.sim = 10, coding = "D",
                                       labeled.alternatives)
        levels.per.attribute <- parsed.data[["lvls"]]
        attribute.levels <- parsed.data[["attribute.list"]]
        if (is.null(prior))
            prior <- parsed.data[["prior"]]
    }
    else
        stop("Input must be either a list of vectors containing the labels of levels for each",
             " attribute, with names corresponding to the attribute labels; or a character",
             "matrix with first row containing attribute names and subsequent rows containing attribute levels.")

    if(any(sapply(attribute.levels, length) < 2))
        stop("All attributes must have at least 2 levels.")

    # If labeled.alternatives then alternatives.per.question is calculated and not supplied
    if (labeled.alternatives)
        alternatives.per.question <- length(attribute.levels[[1]])

    # Convert prohibitions from labels to numeric and factors
    if (!is.null(prohibitions) && length(prohibitions) > 0 && design.algorithm %in% c("Efficient", "Shortcut"))
        warning(gettextf("Prohibitions are not yet implemented for algorithm %s and will be ignored.",
                    sQuote(design.algorithm)))

    prohibitions <- encodeProhibitions(prohibitions, attribute.levels)
    integer.prohibitions <- data.frame(lapply(prohibitions, as.integer))

    # Call the algorithm to create the design
    # Design algorithms     - use only unlabeled levels (i.e. integer level indices)
    #                       - simply multiply question per respondent by n.versions
    #                       - ignore None alternatives, these are added later
    args <- list(levels.per.attribute = levels.per.attribute,
                       prior = prior,
                       n.questions = n.questions * n.versions,
                       alternatives.per.question = alternatives.per.question,
                       prohibitions = integer.prohibitions,
                       labeled.alternatives = labeled.alternatives,
                       seed = seed)

    f <- formals(design.function)
    args <- modifyList(as.list(f), args[names(args) %in% names(f)])

    design <- do.call(design.function, args)

    result <- list(design.algorithm = design.algorithm,
                   attribute.levels = attribute.levels,
                   prohibitions = prohibitions,
                   n.questions = n.questions,
                   n.versions = n.versions,
                   alternatives.per.question = alternatives.per.question,
                   none.alternatives = none.alternatives,
                   output = output)

    if (design.algorithm == "Efficient")
    {
        result$model.matrix <- design$model.matrix
        result$db.error <- design$error
        design <- design$design
    }

    # Add designs and diagnostics
    result$design <- addVersions(design, n.versions)
    result$design.with.none <- addNoneAlternatives(result$design, none.alternatives,
                                                   alternatives.per.question)
    result$labeled.design <- labelDesign(result$design.with.none, attribute.levels)
    result$balances.and.overlaps <- balancesAndOverlaps(result)

    # TODO incorporate None with d.error and standard.errors
    result$d.error <- DerrorHZ(result$design, sapply(result$attribute.levels, length), effects = FALSE)
    ml.model <- mlogitModel(result)
    result$standard.errors <- summary(ml.model)$CoefTable[, 1:2]

    class(result) <- c(class(result), "ChoiceModelDesign")
    return(result)
}


#' @export
#' @method print ChoiceModelDesign
#' @noRd
print.ChoiceModelDesign <- function(x, ...) {

    # Output a table with attributes along the columns and levels along the rows
    if (x$output == "Inputs")
    {
        max.levels <- max(sapply(x$attribute.levels, length))
        levels.table <- sapply(x$attribute.levels, function (z) c(z, rep("", max.levels - length(z))))
        rownames(levels.table) <- paste("Level", seq.int(max.levels))
        print(list(levels.table = levels.table, prohibitions = x$prohibitions))
        return()
    }

    print(x$labeled.design)
}

######################### HELPER FUNCTIONS ###########################

# Convert prohibitions from labels to indices (numeric levels)
# and expand "" or "All" to all levels of the attribute.
encodeProhibitions <- function(prohibitions, attribute.levels) {

    if (is.null(prohibitions) || length(prohibitions) == 0)
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
            expanded.rows <- rep(rownames(prohibitions),
                                                  (length(attribute.levels[[i]]) - 1) * rows.with.all + 1)
            prohibitions <- prohibitions[expanded.rows, ]
            prohibitions[prohibitions[, i] == "All", i] <- attribute.levels[[i]]
        }
    }
    rownames(prohibitions) <- seq(nrow(prohibitions))
    colnames(prohibitions) <- names(attribute.levels)
    return(prohibitions)
}

# Convert an unlabeled design into a labeled design
labelDesign <- function(unlabeled.design, attribute.levels) {
    labeled.design <- lapply(seq_along(attribute.levels),
                             function(i) factor(unlabeled.design[, i + 3],
                                                levels = seq(length(attribute.levels[[i]])),
                                                labels = attribute.levels[[i]]))
    labeled.design <- as.data.frame(labeled.design)
    labeled.design <- cbind(unlabeled.design[, 1:3], labeled.design)
    colnames(labeled.design) = c("Version", "Question", "Alternative", names(attribute.levels))
    return(labeled.design)
}


# Compute one and two-way level balances and overlaps
balancesAndOverlaps <- function(cmd) {

    singles <- singleLevelBalances(cmd$design)

    pairs <- pairLevelBalances(cmd$design)

    # label the levels
    singles <- labelSingleBalanceLevels(singles, cmd$attribute.levels)
    pairs <- labelPairBalanceLevels(pairs, cmd$attribute.levels)

    # flatten pairwise list of list and remove unused
    pairs <- unlist(pairs, recursive = FALSE)
    if (!is.null(pairs))
        pairs <- pairs[!is.na(pairs)]

    overlaps <- countOverlaps(cmd$design, cmd$alternatives.per.question,
                             sapply(cmd$attribute.levels, length))

    return(list(singles = singles, pairs = pairs, overlaps = overlaps))
}


singleLevelBalances <- function(design) {
    singles <- apply(design[, 4:ncol(design), drop = FALSE], 2, table)
    if (!is.list(singles))
    {
        singles <- split(singles, rep(1:ncol(singles), each = nrow(singles)))
        names(singles) <- colnames(design)[4:ncol(design)]
    }
    return(singles)
}

pairLevelBalances <- function(design) {
    n.attributes <- ncol(design) - 3
    if (n.attributes == 1)
        return(NULL)
    pairs <- replicate(n.attributes, rep(list(NA), n.attributes), simplify = FALSE)
    for (i in 1:(n.attributes - 1))
        for (j in (i + 1):n.attributes) {
            pairs[[i]][[j]] <- table(design[, i + 3], design[, j + 3])
            names(pairs[[i]])[[j]] <- paste0(colnames(design)[i + 3], "/", colnames(design)[j + 3])
        }
    return(pairs)
}

labelSingleBalanceLevels <- function(singles, attribute.levels) {
    return(mapply(function(x, y) {names(x) <- y; x}, singles, attribute.levels, SIMPLIFY = FALSE))
}

labelPairBalanceLevels <- function(pairs, attribute.levels) {
    if (is.null(pairs))
        return(NULL)
    n.attributes <- length(attribute.levels)
    for (i in 1:(n.attributes - 1))
        for (j in (i + 1):n.attributes) {
            rownames(pairs[[i]][[j]]) <- attribute.levels[[i]]
            colnames(pairs[[i]][[j]]) <- attribute.levels[[j]]
        }
    return(pairs)
}

#' @importFrom flipFormat FormatAsPercent
countOverlaps <- function(design, alternatives.per.question, levels.per.attribute) {
    # table of counts for each level by question, listed for each attribute
    design <- design[, c(-1, -2, -3), drop = FALSE]
    columns <- split(design, col(design))
    question <- rep(seq(NROW(design) / alternatives.per.question), each = alternatives.per.question)
    overlaps <- lapply(columns, table, question)

    # duplicated levels
    overlaps <- lapply(overlaps, ">=", 2)
    # overlaps for questions (rows) by attribute (cols)
    overlaps <- sapply(overlaps, function(x) apply(x, 2, any))
    prop.qn.overlap.by.attr <- FormatAsPercent(colSums(overlaps) / nrow(overlaps), decimals = 2)
    names(prop.qn.overlap.by.attr) <- paste0(colnames(design), " (", levels.per.attribute, ")")

    return(prop.qn.overlap.by.attr)
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

    if (is.data.frame(design))
    {
        stop("what is this")
        n.questions <- design[[1]][nrow(design)]
        new.rows <- matrix(nrow = n.questions*none.alternatives, ncol = ncol(design))
        new.rows[, 1] <- rep(seq_len(n.questions), each = none.alternatives)
        new.rows[, 2] <- rep((alternatives.per.question+1):(alternatives.per.question+none.alternatives),
                             times = n.questions)
        colnames(new.rows) <- colnames(design)
        out <- rbind(design, new.rows)
        out <- out[order(out$question, out$alternative), ]
        return(out)
    }
    n <- nrow(design)
    new.n <- n * (alternatives.per.question + none.alternatives) / alternatives.per.question
    design.with.none <- matrix(NA, nrow = new.n, ncol = ncol(design))

    # copy existing alternatives
    new.row.indices <- seq(n) + ((seq(n) - 1) %/% alternatives.per.question) * none.alternatives
    design.with.none[new.row.indices, ] <- design

    colnames(design.with.none) <- colnames(design)
    n.versions <- design[NROW(design), 1]
    design.with.none[, 1] <- rep(seq(n.versions), each = NROW(design.with.none) / n.versions)
    design.with.none[, 2] <- rep(seq(n / alternatives.per.question), each = alternatives.per.question + none.alternatives)
    design.with.none[, 3] <- rep(seq(alternatives.per.question + none.alternatives), n / alternatives.per.question)
    return(design.with.none)
}

addVersions <- function(design, n.versions) {
    rows.per.version <- NROW(design) / n.versions
    version.design <- cbind(Version = rep(seq(n.versions), each = rows.per.version), design)
    version.design[, 2] <- rep(design[1:rows.per.version, 1], n.versions)
    return(version.design)
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

# fit a design and choices with mlogit package
#' @importFrom mlogit mlogit.data mlogit
#' @importFrom stats as.formula
mlogitModel <- function(cmd, choices = NULL) {
    if (is.null(choices))
        choices <- randomChoices(cmd)

    # TODO use design.with.none
    labeled <- as.data.frame(labelDesign(cmd$design, cmd$attribute.levels))

    copies <- length(choices) / nrow(labeled)
    labeled <- labeled[rep(seq_len(nrow(labeled)), copies), -1]
    labeled$Choice <- choices
    mlogit.df <- mlogit.data(labeled, choice = "Choice", shape = "long", varying = 3:ncol(labeled),
                     alt.var = "Alternative", id.var = "Question", drop.index = TRUE)

    form <- paste("Choice ~ ", paste0("`", colnames(mlogit.df)[1:ncol(mlogit.df) - 1], "`", collapse = "+"), "| -1")
    ml.model <- mlogit(as.formula(form), data = mlogit.df)
    return(ml.model)
}



