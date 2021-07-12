#' Plots the empirical time distribution for a specific quality
#'
#' `gganytime_etd()` calculates the empirical time distribution,
#' which gives the minimum time needed to achieve a certain quality.
#'
#' @param data dataset
#' @param tvar variable with time information
#' @param qvar variable with quality information
#' @param quality a numeric value denoting the fixed quality
#' @param tgroup trace group (use c(...))
#' @param agroup aesthetics group (use c(...))
#' @param aes aesthetics mapping
#' @param tlim limits for time
#' @param maximizing boolean denoting whether or not the quality measure
#'   is being maximized
#' @param extend boolean denoting whether or not to extend the lines to
#'   the limits
#' @param line one of "step" or "line"
#' @import ggplot2
#' @import dplyr
#' @import rlang
#' @export
#' @examples
#' require(ggplot2)
#' gganytime_etd(traces_large, time, quality, 0.5, c(run, algo), c(algo),
#'               aes = aes(colour = algo))
gganytime_etd <- function(data, tvar, qvar, quality, tgroup, agroup,
                          aes = aes(), tlim = NULL, maximizing = TRUE,
                          extend = TRUE, line = "step") {
  match.arg(line, c("step", "line"))

  if (is.null(tlim)) {
    aux <- data %>% pull({{tvar}})
    tlim <- c(min(aux), max(aux))
  }

  data <- data %>%
    group_by(across({{agroup}})) %>%
    mutate(n = length(unique({{tgroup}}))) %>%
    filter(between({{tvar}}, tlim[1], tlim[2]))

  if (maximizing)
    data <- filter(data, {{qvar}} >= !!quality)
  else
    data <- filter(data, {{qvar}} <= !!quality)

  data <- data %>%
    group_by(across(c({{tgroup}}, .data$n))) %>%
    summarize(x = min({{tvar}})) %>%
    arrange(.data$x) %>%
    group_by(across({{agroup}})) %>%
    mutate(y = seq(n()) / .data$n)

  if (extend) {
    aux <- data %>% group_by(across({{agroup}}))
    aux1 <- aux %>% slice_min(.data$x) %>% mutate(x = tlim[1], y = 0)
    aux2 <- aux %>% slice_max(.data$x) %>% mutate(x = tlim[2])
    data <- bind_rows(aux1, data, aux2)
  }

  aes$x <- quo(.data$x)
  aes$y <- quo(.data$y)

  p <- ggplot(data, aes) +
    xlab("Time") +
    ylab("Proportion of Runs")

  if (line == "step")
    p + geom_step(direction = "hv")
  else
    p + geom_path()
}
