library(dplyr)

test_that("confine() confines correctly", {
  dat <- traces_small

  mint <- min(dat$time) / 4
  maxt <- 3 * max(dat$time) / 4
  minq <- min(dat$quality) / 4
  maxq <- 3 * max(dat$quality) / 4

  res1 <- dat %>% filter(between(time, mint, maxt) &
                         between(quality, minq, maxq))

  res2 <- confine(traces_small, time, quality, run, algo,
                  tlim = c(mint, maxt), qlim = c(minq, maxq),
                  extend = FALSE)

  expect_equal(res1, res2)
})

test_that("confine() works without limits", {
  dat <- traces_small

  res <- confine(traces_small, time, quality, run, algo,
                 tlim = NULL, qlim = NULL,
                 extend = FALSE)

  expect_equal(dat, res)
})

test_that("confine() extends correctly", {
  dat <- traces_small

  nruns <- length(unique(dat$run))
  nalgs <- length(unique(dat$algo))

  mint <- -10
  maxt <- 100
  minq <- -20
  maxq <- 200

  res <- confine(traces_small, time, quality, run, algo,
                 tlim = c(mint, maxt), qlim = c(minq, maxq),
                 qmax = TRUE, extend = TRUE)

  expect_equal(nrow(res), nrow(dat) + nruns * nalgs * 2)

  res2 <- res %>% filter(time == mint | time == maxt |
                         quality == minq | quality == maxq)

  expect_equal(nrow(res2), nruns * nalgs * 2)

  res2 <- arrange(res2, run, algo, time, quality)
  for (i in 1:nruns * nalgs) {
    expect_equal(res2$quality[i * 2 - 1], minq)
    expect_equal(res2$time[i * 2], maxt)
  }

  dat <- mutate(dat, quality = 1 - quality)
  res <- confine(traces_small, time, quality, run, algo,
                 tlim = c(mint, maxt), qlim = c(minq, maxq),
                 qmax = FALSE, extend = TRUE)

  expect_equal(nrow(res), nrow(dat) + nruns * nalgs * 2)

  res2 <- res %>% filter(time == mint | time == maxt |
                         quality == minq | quality == maxq)

  expect_equal(nrow(res2), nruns * nalgs * 2)

  res2 <- arrange(res2, run, algo, time, quality)
  for (i in 1:nruns * nalgs) {
    expect_equal(res2$quality[i * 2 - 1], maxq)
    expect_equal(res2$time[i * 2], maxt)
  }
})
