#' Connects the anytime performance trace of an anytime algorithm
#'
#' `geom_anytime_trace()` connects the observations of an anytime
#' performance trace for an anytime algorithm in order of the time
#' variable, and taking into account whether quality is being maximized
#' or minimzed. The time and quality variables can be represented in the
#' `x-axis` or the `y-axis` depending on the orientation. Moreover, the
#' performance trace may be extended until the limits of the scale.
#' Finally, the line that gives the performance trace may be a "step"
#' line or a "path" line. The `group` aesthetic determines how
#' observations are connected.
#'
#' @inheritParams ggplot2::layer
#' @param orientation one of "tq" for time on x-axis, and quality on
#'   y-axis, or "qt" for the inverse
#' @param maximizing boolean denoting whether or not the quality measure
#'   is being maximized
#' @param extend boolean denoting whether or not to extend the lines to
#'   the limits of the scale as given either by xlim() and ylim() or by
#'   the dataset
#' @param line one of "step" or "path" to denote whether to use
#'   geom_step or geom_path to connect the performance trace
#' @param ... other params passed to `geom_step` or `geom_path` layer
#' @import ggplot2
#' @export
#' @examples
#' require(dplyr)
#' require(ggplot2)
#'
#' traces_small %>%
#' ggplot(aes(time, quality, colour = algo, shape = run)) +
#'   geom_point() +
#'   geom_anytime_trace()
#'
#' traces_small %>%
#' ggplot(aes(quality, time, colour = algo, shape = run)) +
#'   geom_point() +
#'   geom_anytime_trace(orientation = "qt")
#'
#' nonmonotonic_traces_small %>%
#' ggplot(aes(time, quality, colour = algo, shape = run)) +
#'   geom_point() +
#'   geom_anytime_trace()
#'
#' nonmonotonic_traces_small %>%
#' ggplot(aes(quality, time, colour = algo, shape = run)) +
#'   geom_point() +
#'   geom_anytime_trace(orientation = "qt")
geom_anytime_trace <- function(data = NULL,
                               mapping = NULL,
                               stat = "identity",
                               position = "identity",
                               show.legend = NA,
                               inherit.aes = TRUE,
                               orientation = "tq",
                               maximizing = TRUE,
                               extend = TRUE,
                               line = "step",
                               ...) {
  layer(
    geom = GeomAnytimeTrace,
    data = data,
    mapping = mapping,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      orientation = orientation,
      extend = extend,
      line = line,
      maximizing = maximizing,
      ...
    )
  )
}
##' @import ggplot2
##' @noRd
GeomAnytimeTrace <- ggproto("GeomAnytimeTrace", GeomPath,
  draw_panel = function(data, panel_params, coord,
                        orientation = "tq",
                        maximizing = TRUE,
                        extend = TRUE,
                        line = "step") {
    line <- match.arg(line, c("step", "line"))
    orientation <- match.arg(orientation, c("tq", "qt"))

    if (orientation == "tq") {
      data <- confine(data, x, y, group,
                      tlim = panel_params$x$scale$dimension(),
                      qlim = panel_params$y$scale$dimension(),
                      qmax = maximizing, extend = extend)
    } else {
      data <- confine(data, y, x, group,
                      tlim = panel_params$y$scale$dimension(),
                      qlim = panel_params$x$scale$dimension(),
                      qmax = maximizing, extend = extend)
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
