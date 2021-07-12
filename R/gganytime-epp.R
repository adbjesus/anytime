#' Plot the empirical performance profile of an anytime algorithm.
#'
#' @param data data frame with the anytime data
#' @param tvar time variable in the data
#' @param qvar quality variable in the data
#' @param rvar run id variable in the data
#' @param avar algorithm id variable in the data
#' @param algo which algorithm to choose for plotting, if there is only
#'   a single one in the data you may ignore this
#' @param tlim limits for time
#' @param qlim limits for quality
#' @param maximizing whether the quality is being maximized or minimized
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' gganytime_epp(traces_small, time, quality, run, algo, 1)
gganytime_epp <- function(data, tvar, qvar, rvar, avar,
                          algo = NULL,
                          tlim = NULL,
                          qlim = NULL,
                          maximizing = T) {
  if (is.null(tlim)) {
    aux <- data %>% pull({{tvar}})
    tlim <- c(min(aux), max(aux))
  }

  if (is.null(qlim)) {
    aux <- data %>% pull({{qvar}})
    qlim <- c(min(aux), max(aux))
  }

  if (!is.null(algo))
    data <- data %>% filter({{avar}} == !!algo)

  data %>%
    confine({{tvar}}, {{qvar}}, {{rvar}}, {{avar}}, tlim = tlim, qlim = qlim,
            qmax = maximizing, extend = TRUE) %>%
    calculate_epp({{tvar}}, {{qvar}}, {{rvar}}, {{avar}}, tlim, qlim,
                  maximizing) %>%
    ggplot(aes(xmin = .data$xmin, xmax = .data$xmax,
               ymin = .data$ymin, ymax = .data$ymax)) +
    geom_rect(aes(color = .data$dens), fill = NA) +
    geom_rect(aes(fill = .data$dens), color = NA) +
    xlab("Time") +
    ylab("Solution Quality") +
    labs(fill = "Prob.", colour = "Prob.") +
    scale_fill_distiller(palette = "Greys", direction = 1, limits = c(0, 1)) +
    scale_colour_distiller(palette = "Greys", direction = 1, limits = c(0, 1))
}

#' Plots pairwise differences between the empirical performance profiles.
#'
#' @inheritParams gganytime_epp
#' @import ggplot2
#' @import dplyr
#' @importFrom utils combn
#' @export
#' @examples
#' gganytime_epp_pair(traces_small, time, quality, run, algo)
gganytime_epp_pair <- function(data, tvar, qvar, rvar, avar,
                               tlim = NULL,
                               qlim = NULL,
                               maximizing = T) {
  if (is.null(tlim)) {
    aux <- data %>% pull({{tvar}})
    tlim <- c(min(aux), max(aux))
  }

  if (is.null(qlim)) {
    aux <- data %>% pull({{qvar}})
    qlim <- c(min(aux), max(aux))
  }

  data <- data %>%
    confine({{tvar}}, {{qvar}}, {{rvar}}, {{avar}}, tlim = tlim, qlim = qlim,
            qmax = maximizing, extend = TRUE) %>%
    calculate_epp({{tvar}}, {{qvar}}, {{rvar}}, {{avar}}, tlim, qlim,
                  maximizing)

  aux <- data %>% group_split({{avar}})

  foo <- function(a, b) {
    if (any(a$algo == b$algo)) {
      dens <- a$dens
    } else {
      dens <- pmax(a$dens - b$dens, 0)
    }
    data.frame(xmin = a$xmin,
               xmax = a$xmax,
               ymin = a$ymin,
               ymax = a$ymax,
               alg1 = a$algo,
               alg2 = b$algo,
               dens = dens)
  }

  l1 <- lapply(combn(aux, 2, simplify = F), function(x) foo(x[[1]], x[[2]]))
  l2 <- lapply(combn(aux, 2, simplify = F), function(x) foo(x[[2]], x[[1]]))
  l3 <- lapply(aux, function(x) foo(x, x))

  do.call("rbind", append(append(l1, l2), l3)) %>%
    filter(.data$dens > 0) %>%
    ggplot(aes(xmin = .data$xmin, xmax = .data$xmax,
               ymin = .data$ymin, ymax = .data$ymax)) +
    geom_rect(aes(color = .data$dens), fill = NA) +
    geom_rect(aes(fill = .data$dens), color = NA) +
    facet_grid(cols = vars(.data$alg1), rows = vars(.data$alg2)) +
    xlab("Time") +
    ylab("Solution Quality") +
    labs(fill = "Prob.", colour = "Prob.") +
    scale_fill_distiller(palette = "Greys", direction = 1, limits = c(0, 1)) +
    scale_colour_distiller(palette = "Greys", direction = 1, limits = c(0, 1))
}

#' Plots the minimum positive difference between the empirical
#' performance profile of an algorithm and all others
#'
#' @inheritParams gganytime_epp
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' gganytime_epp_min(traces_small, time, quality, run, algo)
gganytime_epp_min <- function(data, tvar, qvar, rvar, avar,
                               tlim = NULL,
                               qlim = NULL,
                               maximizing = T) {
  if (is.null(tlim)) {
    aux <- data %>% pull({{tvar}})
    tlim <- c(min(aux), max(aux))
  }

  if (is.null(qlim)) {
    aux <- data %>% pull({{qvar}})
    qlim <- c(min(aux), max(aux))
  }

  data <- data %>%
    confine({{tvar}}, {{qvar}}, {{rvar}}, {{avar}}, tlim = tlim, qlim = qlim,
            qmax = maximizing, extend = TRUE) %>%
    calculate_epp({{tvar}}, {{qvar}}, {{rvar}}, {{avar}}, tlim, qlim,
                  maximizing)

  algs <- unique(data$algo)
  aux1 <- list()
  aux2 <- list()
  for (i in seq_along(algs)) {
    aux1[[i]] <- data %>% filter(.data$algo == algs[i])
    aux2[[i]] <- aux1[[i]]
  }
  for (i in seq_along(algs)) {
    for (j in seq_along(algs)) {
      if (i != j) {
        aux2[[i]]$dens <- pmin(aux2[[i]]$dens, aux1[[i]]$dens - aux1[[j]]$dens)
      }
    }
  }
  for (i in seq_along(algs)) {
    aux2[[i]]$dens <- pmax(aux2[[i]]$dens, 0)
  }
  do.call("rbind", aux2) %>%
    filter(.data$dens > 0) %>%
    ggplot(aes(xmin = .data$xmin, xmax = .data$xmax,
               ymin = .data$ymin, ymax = .data$ymax)) +
    geom_rect(aes(color = .data$dens), fill = NA) +
    geom_rect(aes(fill = .data$dens), color = NA) +
    facet_wrap(vars(.data$algo)) +
    xlab("Time") +
    ylab("Solution Quality") +
    labs(fill = "Prob.", colour = "Prob.") +
    scale_fill_distiller(palette = "Greys", direction = 1, limits = c(0, 1)) +
    scale_colour_distiller(palette = "Greys", direction = 1, limits = c(0, 1))
}

#' Plots gganytime_epp_pair and gganytime_epp_min together
#'
#' @inheritParams gganytime_epp
#' @import ggplot2
#' @import dplyr
#' @importFrom utils combn
#' @export
#' @examples
#' gganytime_epp_all(traces_small, time, quality, run, algo)
gganytime_epp_all <- function(data, tvar, qvar, rvar, avar,
                               tlim = NULL,
                               qlim = NULL,
                               maximizing = T) {
  if (is.null(tlim)) {
    aux <- data %>% pull({{tvar}})
    tlim <- c(min(aux), max(aux))
  }

  if (is.null(qlim)) {
    aux <- data %>% pull({{qvar}})
    qlim <- c(min(aux), max(aux))
  }

  data <- data %>%
    confine({{tvar}}, {{qvar}}, {{rvar}}, {{avar}}, tlim = tlim, qlim = qlim,
            qmax = maximizing, extend = TRUE) %>%
    calculate_epp({{tvar}}, {{qvar}}, {{rvar}}, {{avar}}, tlim, qlim,
                  maximizing)

  aux <- data %>% group_split({{avar}})

  foo <- function(a, b) {
    if (any(a$algo == b$algo)) {
      dens <- a$dens
    } else {
      dens <- pmax(a$dens - b$dens, 0)
    }
    data.frame(xmin = a$xmin,
               xmax = a$xmax,
               ymin = a$ymin,
               ymax = a$ymax,
               alg1 = a$algo,
               alg2 = b$algo,
               dens = dens)
  }

  l1 <- lapply(combn(aux, 2, simplify = F), function(x) foo(x[[1]], x[[2]]))
  l2 <- lapply(combn(aux, 2, simplify = F), function(x) foo(x[[2]], x[[1]]))
  l3 <- lapply(aux, function(x) foo(x, x))

  diffs <- do.call("rbind", append(append(l1, l2), l3))

  diffs2 <- diffs %>% group_by(.data$xmin, .data$xmax,
                               .data$ymin, .data$ymax, .data$alg1) %>%
    summarize(alg2 = "Min.", dens = min(.data$dens)) %>%
    ungroup()

  rbind(diffs, diffs2) %>%
    filter(.data$dens > 0) %>%
    ggplot(aes(xmin = .data$xmin, xmax = .data$xmax,
               ymin = .data$ymin, ymax = .data$ymax)) +
    geom_rect(aes(color = .data$dens), fill = NA) +
    geom_rect(aes(fill = .data$dens), color = NA) +
    facet_grid(cols = vars(.data$alg1), rows = vars(.data$alg2)) +
    xlab("Time") +
    ylab("Solution Quality") +
    labs(fill = "Prob.", colour = "Prob.") +
    scale_fill_distiller(palette = "Greys", direction = 1, limits = c(0, 1)) +
    scale_colour_distiller(palette = "Greys", direction = 1, limits = c(0, 1))
}

#' @import dplyr
#' @noRd
calculate_epp <- function(data, tvar, qvar, rvar, avar, tlim, qlim,
                          maximizing) {
  if(maximizing == FALSE)
    stop("TODO 'maximizing = FALSE' not yet implemented")

  algs <- data %>% pull({{avar}}) %>% unique()
  runs <- lapply(algs, function(x) {
    data %>% filter({{avar}} == !!x) %>% pull({{rvar}}) %>% unique()
  })

  runs.length <- unlist(lapply(runs, length))
  runs.length.sum <- sum(runs.length)

  q <- matrix(0, runs.length.sum, 2)
  q[, 2] <- unlist(lapply(seq_along(algs), function(x) rep(x, runs.length[x])))

  data <- data %>% arrange({{tvar}}, {{qvar}})
  smax <- nrow(data) * runs.length.sum
  xmin <- numeric(smax)
  xmax <- numeric(smax)
  ymin <- numeric(smax)
  ymax <- numeric(smax)
  dens <- numeric(smax)
  algo <- character(smax)
  s <- 0
  t <- 0
  for (i in seq_len(nrow(data))) {
    nt <- data[i, ] %>% pull({{tvar}})
    active <- runs.length
    aux <- q[order(q[, 1]), ]
    lq <- 0
    for (j in seq_len(nrow(q))) {
      if (aux[j, 1] > 0) {
        for (a in seq_along(algs)) {
          s <- s + 1
          xmin[s] <- t
          xmax[s] <- nt
          ymin[s] <- lq
          ymax[s] <- aux[j, 1]
          dens[s] <- active[a] / runs.length[a]
          algo[s] <- algs[a]
        }
        lq <- aux[j, 1]
      }
      active[aux[j, 2]] <- active[aux[j, 2]] - 1
    }
    a <- match(data[i, ] %>% pull({{avar}}), algs)
    r <- match(data[i, ] %>% pull({{rvar}}), runs[[a]])
    ind <- sum(runs.length[1:a]) - runs.length[a] + r
    q[ind, 1] <- data[i, ] %>% pull({{qvar}})
    t <- nt
  }

  data.frame(
    xmin = xmin[1:s], xmax = xmax[1:s], ymin = ymin[1:s],
    ymax = ymax[1:s], dens = dens[1:s], algo = algo[1:s]
  )
}
