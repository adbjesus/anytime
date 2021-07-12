#' Connects the anytime performance quantiles for an anytime algorithm
#'
#' `geom_anytime_quantile()` calculates the attainment quantiles for the
#' anytime algorithms.
#'
#' @inheritParams ggplot2::layer
#' @param prob a numeric value denoting the probability to plot
#' @param orientation one of "tq" for time on x-axis, and quality on
#'   y-axis, or "qt" for the inverse
#' @param maximizing boolean denoting whether or not the quality measure
#'   is being maximized
#' @param extend boolean denoting whether or not to extend the lines to
#'   the limits of the scale as given either by xlim() and ylim() or by
#'   the dataset
#' @param line one of "step" or "path" to denote whether to use
#'   geom_step or geom_path to connect the points in the quantiles
#' @param ... other params passed to `geom_step` or `geom_path` layer
#' @import ggplot2
#' @importFrom stats quantile
#' @export
#' @examples
#' require(dplyr)
#' require(ggplot2)
#'
#' traces_large %>%
#' ggplot(aes(time, quality, colour = algo, group = algo, run_group = run)) +
#'   geom_anytime_quantile(prob = 0.25, linetype = 3) +
#'   geom_anytime_quantile(prob = 0.50, linetype = 1) +
#'   geom_anytime_quantile(prob = 0.75, linetype = 3)
#'
#' nonmonotonic_traces_large %>%
#' ggplot(aes(time, quality, colour = algo, group = algo, run_group = run)) +
#'   geom_anytime_quantile(prob = 0.25, linetype = 3) +
#'   geom_anytime_quantile(prob = 0.50, linetype = 1) +
#'   geom_anytime_quantile(prob = 0.75, linetype = 3)
geom_anytime_quantile <- function(mapping = NULL,
                                  data = NULL,
                                  stat = "identity",
                                  position = "identity",
                                  show.legend = NA,
                                  inherit.aes = TRUE,
                                  prob = 0.5,
                                  orientation = "tq",
                                  maximizing = TRUE,
                                  extend = TRUE,
                                  line = "step",
                                  ...) {
  layer(
    geom = GeomAnytimeQuantile,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      prob = prob,
      orientation = orientation,
      maximizing = maximizing,
      extend = extend,
      line = line,
      ...
    )
  )
}
##' @import ggplot2
##' @noRd
GeomAnytimeQuantile <- ggproto("GeomAnytimeTrace", GeomPath,
  required_aes = c(GeomPath$required_aes, "run_group"),
  draw_panel = function(data, panel_params, coord,
                        prob = 0.5,
                        orientation = "tq",
                        maximizing = TRUE,
                        extend = TRUE,
                        line = "step") {
    line <- match.arg(line, c("step", "line"))
    orientation <- match.arg(orientation, c("tq", "qt"))

    if (orientation == "tq") {
      tlim <- panel_params$x$scale$dimension()
      qlim <- panel_params$y$scale$dimension()
      data <- confine(data, x, y, run_group, group,
                      tlim = tlim, qlim = qlim,
                      qmax = maximizing,
                      extend = extend)
      data <- data_quantile(data, x, y, run_group, group,
                            tlim, qlim, prob,
                            orientation, maximizing)
    } else {
      stop("Orientation 'qt' still not working")
    }

    if (orientation == "tq") {
      if (maximizing) {
        data <- data %>% arrange(group, x, y)
      } else {
        data <- data %>% arrange(group, x, -y)
      }
    } else {
      if (maximizing) {
        data <- data %>% arrange(group, y, x)
      } else {
        data <- data %>% arrange(group, y, -x)
      }
    }

    if (line == "step") {
      direction <- ifelse(orientation == "tq", "hv", "vh")
      GeomStep$draw_panel(data, panel_params, coord, direction = direction)
    } else {
      GeomPath$draw_panel(data, panel_params, coord)
    }
  }
)

#' @import ggplot2
#' @noRd
data_quantile <- function(data, tvar, qvar, rvar, gvar,
                          tlim, qlim,
                          prob = 0.5,
                          orientation = "tq",
                          maximizing = TRUE) {
  orientation <- match.arg(orientation, c("tq", "qt"))
  if (orientation == "qt")
    stop("Orientation 'qt' still not working")
  if (!between(prob, 0, 1))
    stop("Parameter `quantile` must be between 0 and 1")

  n <- nrow(data)
  quan <- numeric(n)

  if (maximizing)
    data <- data %>% arrange({{gvar}}, {{tvar}}, {{qvar}})
  else
    data <- data %>% arrange({{gvar}}, {{tvar}}, desc({{qvar}}))

  groups <- data %>% pull({{gvar}}) %>% unique()

  m <- 0
  for (g in groups) {
    dat <- data %>% filter({{gvar}} == g)
    ts <- dat %>% pull({{tvar}})
    qs <- dat %>% pull({{qvar}})
    rs <- dat %>% pull({{rvar}})
    runs <- unique(rs)
    if (maximizing) {
      aux <- rep(qlim[1]-1, length(runs))
    } else {
      aux <- rep(qlim[2]+1, length(runs))
    }
    for (i in 1:length(ts)) {
      ind <- match(rs[i], runs)
      aux[ind] <- qs[i]
      m <- m + 1
      if (maximizing) {
        if (sum(between(aux, qlim[1], qlim[2])) / length(runs) > prob) {
          quan[m] <- quantile(aux, 1 - prob, type = 1)
        } else {
          quan[m] <- NA
        }
      } else {
        if (sum(between(aux, qlim[1], qlim[2])) / length(runs) > prob) {
          quan[m] <- quantile(aux, prob, type = 1)
        } else {
          quan[m] <- NA
        }
      }
    }
  }

  data %>%
    ungroup() %>%
    mutate({{qvar}} := quan) %>%
    filter(!is.na({{qvar}}))
}
