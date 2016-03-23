# This fake data set is used for testing weighting a filtering operations.
set.seed(192)
n <- 10
y <- rep(10, n)
k <- 5
filter <- c(rep(TRUE, k), rep(FALSE, n - k))
wgt <- as.integer(filter)

one.if.filtered <- runif(n, 0, 4)
y[filter] <- one.if.filtered[filter]
y[!filter] <- 4-one.if.filtered[!filter]
#y <- y + rnorm(n, 0, 0.01)

binary <- as.integer(y > 2)
ordered = factor(binary)
testweightsfilters <- data.frame(filter, wgt, y, one.if.filtered, binary, ordered)
rm(filter, wgt, y, one.if.filtered, binary,n, k, ordered)

#sb <-  testweightsfilters$filter

Regression(y ~ one.if.filtered, data = testweightsfilters)
Regression(y ~ one.if.filtered, data = testweightsfilters, subset = testweightsfilters$filter)
Regression(y ~ one.if.filtered, data = testweightsfilters, weights = testweightsfilters$wgt)
Regression(y ~ one.if.filtered, data = testweightsfilters, subset = testweightsfilters$filter, weights = testweightsfilters$wgt)

type = "Binary Logit"
Regression(binary ~ one.if.filtered, data = testweightsfilters, type = type)
Regression(binary ~ one.if.filtered, data = testweightsfilters, type = type, subset = testweightsfilters$filter)
Regression(binary ~ one.if.filtered, data = testweightsfilters, type = type, weights = testweightsfilters$wgt)
Regression(binary ~ one.if.filtered, data = testweightsfilters, type = type, subset = testweightsfilters$filter, weights = testweightsfilters$wgt)

type = "Ordered Logit"
Regression(ordered ~ one.if.filtered, data = testweightsfilters, type = type)
Regression(ordered ~ one.if.filtered, data = testweightsfilters, type = type, subset = testweightsfilters$filter)
Regression(ordered ~ one.if.filtered, data = testweightsfilters, type = type, weights = testweightsfilters$wgt)
Regression(ordered ~ one.if.filtered, data = testweightsfilters, type = type, subset = testweightsfilters$filter, weights = testweightsfilters$wgt)

type = "Poisson"
Regression(binary ~ one.if.filtered, data = testweightsfilters)
Regression(binary ~ one.if.filtered, data = testweightsfilters, subset = testweightsfilters$filter)
Regression(binary ~ one.if.filtered, data = testweightsfilters, weights = testweightsfilters$wgt)
Regression(binary ~ one.if.filtered, data = testweightsfilters, subset = testweightsfilters$filter, weights = testweightsfilters$wgt)

type = "Quasi-Poisson"
Regression(binary ~ one.if.filtered, data = testweightsfilters)
Regression(binary ~ one.if.filtered, data = testweightsfilters, subset = testweightsfilters$filter)
Regression(binary ~ one.if.filtered, data = testweightsfilters, weights = testweightsfilters$wgt)
Regression(binary ~ one.if.filtered, data = testweightsfilters, subset = testweightsfilters$filter, weights = testweightsfilters$wgt)

type = "Quasi-Poisson"
Regression(binary ~ one.if.filtered, data = testweightsfilters)
Regression(binary ~ one.if.filtered, data = testweightsfilters, subset = testweightsfilters$filter)
Regression(binary ~ one.if.filtered, data = testweightsfilters, weights = testweightsfilters$wgt)
Regression(binary ~ one.if.filtered, data = testweightsfilters, subset = testweightsfilters$filter, weights = testweightsfilters$wgt)




type = linear


BinaryLogit(binary ~ one.if.filtered, data = testweightsfilters)
BinaryLogit(binary ~ one.if.filtered, data = testweightsfilters, subset = testweightsfilters$sb)
BinaryLogit(binary ~ one.if.filtered, data = testweightsfilters, weights = testweightsfilters$wgt)
BinaryLogit(binary ~ one.if.filtered, data = testweightsfilters, subset = testweightsfilters$sb, weights = testweightsfilters$wgt)



lm(y ~ one.if.filtered, data = testweightsfilters)

lm(y ~ one.if.filtered, weights = wgt)
lm(y ~ one.if.filtered, subset = filter, weights = wgt)

lm(y ~ one.if.filtered)
lm(y ~ one.if.filtered, subset = filter)
lm(y ~ one.if.filtered, weights = wgt)
lm(y ~ one.if.filtered, subset = filter, weights = wgt)


devtools::use_data(testweightsfilters, internal = FALSE, overwrite = TRUE)

