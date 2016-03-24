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
count <- round(y)
ordered <- factor(count)
testweightsfilters <- data.frame(filter, wgt, y, one.if.filtered, binary, ordered, count)
rm(filter, wgt, y, one.if.filtered, binary,n, k, ordered)
devtools::use_data(testweightsfilters, internal = FALSE, overwrite = TRUE)

library(foreign)
write.foreign(testweightsfilters, "c:/delete/testweightsfilters.txt", "c:/delete/testweightsfilters.sps",   package="SPSS")
write.csv(testweightsfilters, "c:/delete/testweightsfilters.csv")



data(testweightsfilters)

Regression(count ~ one.if.filtered, data = testweightsfilters)
Regression(count ~ one.if.filtered, data = testweightsfilters, subset = testweightsfilters$filter)
Regression(count ~ one.if.filtered, data = testweightsfilters, robust.se = TRUE, subset = testweightsfilters$filter)
Regression(count ~ one.if.filtered, data = testweightsfilters, weights = testweightsfilters$wgt)
Regression(count ~ one.if.filtered, data = testweightsfilters, subset = testweightsfilters$filter, weights = testweightsfilters$wgt)

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
Regression(count ~ one.if.filtered, data = testweightsfilters, type = type)
Regression(count ~ one.if.filtered, data = testweightsfilters, type = type, subset = testweightsfilters$filter)
Regression(count ~ one.if.filtered, data = testweightsfilters, type = type, weights = testweightsfilters$wgt)
Regression(count ~ one.if.filtered, data = testweightsfilters, type = type, subset = testweightsfilters$filter, weights = testweightsfilters$wgt)

# Comparisons of alternative models. NB: svy.glm corresponds to Stata results.
n <- 1
tempData <- data.frame(x = rep(c(3.3702579, 0.7609655, 2.2110524, 0.9685861, 2.7369464), n),
    y = rep(c(3, 1, 2, 1, 3), n))

sDesign <- survey::svydesign(id = ~ 1, weights = rep(1, 5 * n), data = tempData)
summary(survey::svyglm(y~x, sDesign,  family = poisson()), df.resid = Inf)

summary(glm(y~x, data = tempData, family = poisson))
Regression(count ~ one.if.filtered, data = testweightsfilters, type = type, robust.se = TRUE, subset = testweightsfilters$filter)



type = "Quasi-Poisson"
Regression(count ~ one.if.filtered, data = testweightsfilters, type = type)
Regression(count ~ one.if.filtered, data = testweightsfilters, type = type, subset = testweightsfilters$filter)
Regression(count ~ one.if.filtered, data = testweightsfilters, type = type, weights = testweightsfilters$wgt)
Regression(count ~ one.if.filtered, data = testweightsfilters, type = type, subset = testweightsfilters$filter, weights = testweightsfilters$wgt)

type = "NBD"
Regression(count ~ one.if.filtered, data = testweightsfilters, type = type)
Regression(count ~ one.if.filtered, data = testweightsfilters, type = type, subset = testweightsfilters$filter)
Regression(count ~ one.if.filtered, data = testweightsfilters, type = type, weights = testweightsfilters$wgt)
Regression(count ~ one.if.filtered, data = testweightsfilters, type = type, subset = testweightsfilters$filter, weights = testweightsfilters$wgt)
