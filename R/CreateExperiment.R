#' Create an experimental design
#'
#' Creates attributes and levels of an experiment, possibly with random prohibitions.
#' This is useful for quickly creating a design without typing in lists of levels.
#' @param levels.per.attribute Numeric \code{\link{vector}} of the number of levels
#' per attribute.
#' @param n.prohibitions Integer; number of prohibitions.
#' @param seed Integer; random seed to be used by the algorithms.
#' @return A list with components
#' \itemize{
#' \item \code{attribute.levels} - a \code{\link{list}} of \code{\link{vector}}s of the
#' labels of levels for each attribute. The names of the vectors are the attribute labels.
#' \item \code{prohibitions} - Character \code{\link{matrix}} where each row is a prohibited
#' alternative consisting of the levels of each attribute.
#' }
#' @export
CreateExperiment <- function(levels.per.attribute, n.prohibitions = 0, seed = 12345) {

    set.seed(seed)
    attributes <- LETTERS[1:length(levels.per.attribute)]
    attribute.levels <- sapply(levels.per.attribute, seq, simplify = FALSE)
    attribute.levels <- mapply(paste0, attributes, attribute.levels, SIMPLIFY = FALSE)
    names(attribute.levels) <- attributes

    # may produce duplicate prohibitions
    prohibitions <- t(replicate(n.prohibitions, sapply(attribute.levels, sample, 1)))

    experiment <- list(attribute.levels = attribute.levels, prohibitions = prohibitions)
}
