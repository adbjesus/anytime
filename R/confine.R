#' Confine the performance traces to a bounding box.
#'
#' `confine()` confines the performance traces to a given bounding box.
#' Also, it optionally extends the performance traces until the limits
#' of bounding box. This method takes into account whether the quality
#' is being maximized or minimized.
#'
#' @param data data frame to preprocess
#' @param tvar variable that gives the time column in the data
#' @param qvar variable that gives the quality column in the data
#' @param ... grouping variables for the performance traces
#' @param tlim numeric vector of length 2 denoting the limits for time,
#'   or `NULL` to use the limits of the data
#' @param qlim numeric vector of length 2 denoting the limits for
#'   quality, or `NULL` to use the limits of the data
#' @param qmax boolean denoting whether the quality measure is being
#'   maximized or minimized
#' @param extend boolean denoting whether or not to add a fake
#'   observation, for each performance trace, at the limits
#' @import dplyr
#' @import rlang
#' @export
#' @examples
#' require(dplyr)
#'
#' confine(traces_small, time, quality, algo, run,
#'         tlim = c(0, 5), qlim = c(0, 1), extend = TRUE)
#'
#' traces_small %>% mutate(quality = 1 - quality) %>%
#' confine(time, quality, algo, run,
#'         tlim = c(0, 5), qlim = c(0, 1), qmax = FALSE, extend = TRUE)
confine <- function(data,
                    tvar,
                    qvar,
                    ...,
                    tlim = NULL,
                    qlim = NULL,
                    qmax = TRUE,
                    extend = TRUE) {
  if (is.null(tlim)) {
    aux <- pull(data, {{tvar}})
    tlim <- c(min(aux), max(aux))
  }
  if (is.null(qlim)) {
    aux <- pull(data, {{qvar}})
    qlim <- c(min(aux), max(aux))
  }

  if (length(tlim) != 2)
    stop("Parameter 'tlim' must be NULL or have length 2")
  if (length(qlim) != 2)
    stop("Parameter 'qlim' must be NULL or have length 2")

  data <- filter(data,
                 between({{tvar}}, tlim[1], tlim[2]) &
                 between({{qvar}}, qlim[1], qlim[2]))

  if (extend) {
    if (qmax) {
      aux <- data %>% group_by(...)
      aux1 <- aux %>% slice_min({{tvar}}) %>% mutate({{qvar}} := qlim[1])
      aux2 <- aux %>% slice_max({{tvar}}) %>% mutate({{tvar}} := tlim[2])
      data <- bind_rows(data, aux1, aux2)
    } else {
      aux <- data %>% group_by(...)
      aux1 <- aux %>% slice_min({{tvar}}) %>% mutate({{qvar}} := qlim[2])
      aux2 <- aux %>% slice_max({{tvar}}) %>% mutate({{tvar}} := tlim[2])
      data <- bind_rows(data, aux1, aux2)
    }
  }

  data
}
