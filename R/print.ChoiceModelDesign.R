#' @export
#' @method print ChoiceModelDesign
#' @noRd
print.ChoiceModelDesign <- function(x, ...) {

    # Output a table with attributes along the columns and levels along the rows
    if (x$output == "Attributes and levels")
    {
        max.levels <- max(sapply(x$attribute.levels, length))
        levels.table <- sapply(x$attribute.levels, function (z) c(z, rep("", max.levels - length(z))))
        rownames(levels.table) <- paste("Level", seq.int(max.levels))
        print(levels.table)
    }

    else if (x$output == "Prohibitions")
        print(x$prohibitions)

    # Output the design with indices or labels
    else if (x$output == "Unlabeled design")
        print(x$design.with.none)
    else if (x$output == "Labeled design")
        print(labelDesign(x$design.with.none, x$attribute.levels))

    # Single and pairwise level balances and overlaps
    # (where overlaps is the proportion of questions that have >= 1 repeated level, by attribute)
    else if (x$output == "Balances and overlaps")
        print(balancesAndOverlaps(x))

    else if (x$output == "Standard errors")
    {
        ml.model <- mlogitModel(x)
        print(list(d.score = dScore(x$design),
                    d.error = DerrorHZ(x$design, sapply(x$attribute.levels, length), effects = TRUE)))
        print(summary(ml.model))
    }
    else
        stop("Unrecognized output.")
}


