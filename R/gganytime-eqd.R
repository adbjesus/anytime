#' Plots the empirical time distribution for a specific quality
#'
#' `gganytime_eqd()` calculates the empirical time distribution,
#' which gives the minimum time needed to achieve a certain quality.
#'
#' @param data dataset
#' @param tvar variable with time information
#' @param qvar variable with quality information
#' @param time a numeric value denoting the fixed time
#' @param tgroup trace grouping variables (use c(...))
#' @param agroup aesthetics grouping variables (use c(...))
#' @param aes aesthetics mapping
#' @param qlim limits for time
#' @param maximizing boolean denoting whether or not the quality measure
#'   is being maximized
#' @param extend boolean denoting whether or not to extend the lines to
#'   infinity
#' @param line one of "step" or "line"
#' @import ggplot2
#' @import dplyr
#' @import rlang
#' @export
#' @examples
#' require(ggplot2)
#' gganytime_eqd(traces_large, time, quality, 5, c(run, algo), c(algo),
#'               aes = aes(colour = algo))
gganytime_eqd <- function(data, tvar, qvar, time, tgroup, agroup,
                          aes = aes(), qlim = NULL, maximizing = TRUE,
                          extend = TRUE, line = "step") {
  match.arg(line, c("step", "line"))

  if (is.null(qlim)) {
    aux <- data %>% pull({{qvar}})
    qlim <- c(min(aux), max(aux))
  }

  data <- data %>%
    group_by(across({{agroup}})) %>%
    mutate(n = length(unique({{tgroup}}))) %>%
    filter(between({{qvar}}, qlim[1], qlim[2]) & {{tvar}} <= !!time) %>%
    group_by(across(c({{tgroup}}, .data$n)))

  if (maximizing) {
    data <- data %>%
      slice_max({{tvar}}) %>%
      summarize(x = max({{qvar}})) %>%
      arrange(desc(.data$x))
  } else{
    data <- data %>%
      slice_max({{tvar}}) %>%
      summarize(x = min({{qvar}})) %>%
      arrange(.data$x)
  }

  data <- data %>%
    group_by(across({{agroup}})) %>%
    mutate(y = seq(n()) / .data$n)

  if (extend) {
    aux <- data %>% group_by(across({{agroup}}))
    if (maximizing) {
      aux1 <- aux %>% slice_min(.data$x) %>% mutate(x = qlim[1])
      aux2 <- aux %>% slice_max(.data$x) %>% mutate(x = qlim[2], y = 0)
      data <- bind_rows(data, aux1, aux2)
    } else {
      aux1 <- aux %>% slice_min(.data$x) %>% mutate(x = qlim[1], y = 0)
      aux2 <- aux %>% slice_max(.data$x) %>% mutate(x = qlim[2])
      data <- bind_rows(data, aux1, aux2)
    }
  }

  data <- data %>% arrange(.data$x)

  aes$x <- quo(.data$x)
  aes$y <- quo(.data$y)

  p <- ggplot(data, aes) +
    xlab("Quality") +
    ylab("Proportion of Runs")

  if (line == "step")
    p + geom_step(direction = "vh")
  else
    p + geom_path()
}
