# # This fake data set is used for testing weighting a filtering operations.
# set.seed(192)
# n <- 10
# y <- rep(0, n)
# k <- 5
# filter <- c(rep(TRUE, k), rep(FALSE, n - k))
# wgt <- as.integer(filter)
#
# one.if.filtered <- runif(n, 0, 4)
# y[filter] <- one.if.filtered[filter]
# y[!filter] <- 4-one.if.filtered[!filter]
#
# binary <- as.integer(y > 2)
# testweightsfilters <- data.frame(filter, wgt, y, one.if.filtered, binary)
#
#
# LinearRegression(y ~ one.if.filtered, testweightsfilters)
#
#
# lm(y ~ one.if.filtered, subset = filter)
# lm(y ~ one.if.filtered, weights = wgt)
# lm(y ~ one.if.filtered, subset = filter, weights = wgt)
#
# lm(y ~ one.if.filtered)
# lm(y ~ one.if.filtered, subset = filter)
# lm(y ~ one.if.filtered, weights = wgt)
# lm(y ~ one.if.filtered, subset = filter, weights = wgt)
#
#
# devtools::use_data(testweightsfilters, internal = FALSE, overwrite = TRUE)
#
