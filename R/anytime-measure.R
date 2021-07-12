#' A measure of anytime performance
#'
#' This functions provides a measure of anytime performance as described
#' in "A. D. Jesus, A. Liefooghe, B. Derbel, L. Paquete. Algorithm
#' Selection of Anytime Algorithms. Proceedings of the 2020 Genetic and
#' Evolutionary Computation Conference (GECCO 2020), 850-858, 2020."
#'
#' @param data input data frame
#' @param tvar variable with the time data
#' @param qvar variable with the quality data
#' @param rvar variable with the run id
#' @param avar variable with the algorithm id
#' @param tlim limits for time, vector of length 2, or NULL to use the
#'   limits from the data frame
#' @param qlim limits for quality, vector of length 2, or NULL to use
#'   the limits from the data frame
#' @param maximizing boolean denoting whether the quality is being
#'   maximized or minimized
#' @import dplyr
#' @export
#' @examples
#' anytime_measure(traces_small, time, quality, run, algo)
anytime_measure <- function(data, tvar, qvar, rvar, avar,
                            tlim = NULL, qlim = NULL,
                            maximizing = TRUE) {
  if (maximizing == FALSE)
    stop("TODO 'maximizing = FALSE' not yet implemented")

  if (is.null(tlim)) {
    aux <- data %>% pull({{tvar}})
    tlim <- c(min(aux), max(aux))
  }

  if (is.null(qlim)) {
    aux <- data %>% pull({{qvar}})
    qlim <- c(min(aux), max(aux))
  }

  data %>%
    confine({{tvar}}, {{qvar}}, {{rvar}}, {{avar}}, tlim = tlim, qlim = qlim,
            qmax = maximizing, extend = TRUE) %>%
    calculate_epp({{tvar}}, {{qvar}}, {{rvar}}, {{avar}}, tlim, qlim,
                  maximizing) %>%
    group_by(.data$algo) %>%
    mutate(area = (.data$xmax - .data$xmin) * (.data$ymax - .data$ymin),
           res = .data$area * .data$dens) %>%
    summarize(measure = sum(.data$res))
}
