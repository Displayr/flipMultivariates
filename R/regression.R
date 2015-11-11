#' Object Goodness-of-Fit Plot
#' \code{Linear Regression} A generic function used to produce plots showing outliers in the data from
#' a model object.  The function invokes particular \code{\link{methods}}
#' which depend on the \code{\link{class}} of the first argument.
#'
#' reports the goodness-of-fit of an object.
#' @param object An object for which a summary is desired..
#' @param ... Additional arguments affecting the goodness-of-fit displayed.

LinearRegression <- function(formula, data, ...) {
    if(is.factor(data[,1])) {
        WarningFactorToNumeric()
        data[,1] <- unclass(data[,1])
   }
    if (is.null(data$QCalibratedWeight))
        result <- lm(formula, data = data, subset = data$QFilter, model = "ls", ...)
    else {
        result <- svyglm(formula,  weightedSurveyDesign(data), subset = data$QFilter, model = "normal.survey", ...)
    }
result}

BinaryLogit = function(formula, data) {
    data[,1] <- DichotomizeFactor(data[,1], warning = TRUE, variable.name = names(data)[1])
	if (is.null(data$QCalibratedWeight))
        result <- glm(formula, data = data, subset = data$QFilter, family = binomial)
    else {
	    result <- svyglm(formula, weightedSurveyDesign(data), subset = data$QFilter, family = binomial)
    }
result}

PoissonRegression = function(formula, data) {
    stopIfNotCount(data[,1], variable.name = names(data)[1])
	if (is.null(data$QCalibratedWeight))
        result <- glm(formula, data = data, subset = data$QFilter, family = poisson)
    else {
	    result <- svyglm(formula, weightedSurveyDesign(data), subset = data$QFilter, family = poisson)
    }
result}


QuasiPoissonRegression <- function(formula, data) {
    stopIfNotCount(data[,1], variable.name = names(data)[1])
	if (is.null(data$QCalibratedWeight))
        result <- glm(formula, data = data, subset = data$QFilter, family = quasipoisson)
    else {
	    result <- svyglm(formula, weightedSurveyDesign(data), subset = data$QFilter, family = quasipoisson)
    }
result}


#context("Testing weightedcomputations")

# test_that("WeightedSVD",
# {
#     set.seed(123)
#     z <- matrix(runif(21), ncol = 3)
#     zWeight <- c(4,rep(1,6))
#     z1 <- rbind(z[rep(1:7, zWeight),])
#     z1
#     q = function(x) {round(abs(x),5)}
#
#     expect_equal(q(svd(z1), q(WeightedSVD(z, zWeight))))
#     expect_equal(q(svd(z1, nu = 0L)), q(WeightedSVD(z, zWeight, nu = 0L)))
# })
#

