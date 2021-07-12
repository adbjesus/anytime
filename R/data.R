#' Generated performance traces.
#'
#' A dataset containing generated performance traces for three
#'  algorithms. `traces_large` contains 30 monotonic performance traces
#'  for each algorithm, `traces_small` contains only the first 3
#'  performance traces for each algorithm. `nonmonotonic_traces_large`
#'  contains 30 nonmonotonic performance traces for each algorithm,
#'  `nonmonotonic_traces_small` contains only the first 3 performance
#'  traces for each algorithm. The variables are as follows:
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{time}{execution time in seconds (0--10)}
#'   \item{quality}{quality of the solution (0--1)}
#'   \item{run}{id for the run (factor)}
#'   \item{algo}{id for the algorithm (factor)}
#' }
"traces_large"

#' @rdname traces_large
"traces_small"

#' @rdname traces_large
"nonmonotonic_traces_large"

#' @rdname traces_large
"nonmonotonic_traces_small"
