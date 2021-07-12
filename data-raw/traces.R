library(dplyr)

random_trace <- function(run, alg, n, monotonic, tmax, qmax, trans) {
  t <- sort(trans(runif(n)) * tmax)
  q <- sort(runif(n) * qmax)
  if (monotonic == FALSE && n > 2) {
    m <- as.integer(n / 2)
    aux <- q[m]
    q[m] <- q[m + 1]
    q[m + 1] <- aux
  }
  r <- rep(run, n)
  a <- rep(alg, n)
  data.frame(time = t, quality = q, run = r, algo = a)
}

random_traces <- function(runs, ...) {
  do.call("rbind", lapply(1:runs, random_trace, ...))
}

n <- 10
tmax <- 10
qmax <- 1

set.seed(0)

df1 <- random_traces(30, 1, n, T, tmax, qmax * 0.70, function(x) x**3)
df2 <- random_traces(30, 2, n, T, tmax, qmax * 0.85, identity)
df3 <- random_traces(30, 3, n, T, tmax, qmax * 1.00, function(x) 1 - x**4)
d1 <- rbind(df1, df2, df3)
d2 <- filter(d1, run <= 3)

traces_large <- mutate(d1, run = as.factor(run), algo = as.factor(algo))
traces_small <- mutate(d2, run = as.factor(run), algo = as.factor(algo))

usethis::use_data(traces_large, overwrite = TRUE)
usethis::use_data(traces_small, overwrite = TRUE)

df1 <- random_traces(30, 1, n, F, tmax, qmax * 0.70, function(x) x**3)
df2 <- random_traces(30, 2, n, F, tmax, qmax * 0.85, identity)
df3 <- random_traces(30, 3, n, F, tmax, qmax * 1.00, function(x) 1 - x**4)
d1 <- rbind(df1, df2, df3)
d2 <- filter(d1, run <= 3)

nonmonotonic_traces_large <- mutate(d1, run = as.factor(run),
                                    algo = as.factor(algo))
nonmonotonic_traces_small <- mutate(d2, run = as.factor(run),
                                    algo = as.factor(algo))

usethis::use_data(nonmonotonic_traces_large, overwrite = TRUE)
usethis::use_data(nonmonotonic_traces_small, overwrite = TRUE)
