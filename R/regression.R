#' \code{Linear Regression} Linear Regression.
#'
#' reports the goodness-of-fit of an object.
#' @param object An object for which a summary is desired..
#' @param ... Additional arguments affecting the goodness-of-fit displayed.
#' @details
#' Uses \code{\link{Zelig:zelimethods}}
#' which depend on the \code{\link{class}} of the first argument.
#'
#' @export
LinearRegression <- function(formula, data, weights = NULL, subset = NULL, ...) {
    if(is.factor(data[,1])) {
        WarningFactorToNumeric()
        data[,1] <- unclass(data[,1])
    }
    if (is.null(weights))
    {
        if(is.null(subset) | length(subset) == 1)
        {
            result <- Zelig::zelig(formula,  data = data , model = "ls", ...)
        }
        else
            result <- Zelig::zelig(formula,  data = data , model = "ls", subset = subset, ...)
    }
    else
    {
        if(is.null(subset) | length(subset) == 1)-
            result <- Zelig::zelig(formula,  data = data , model = "normal.survey", weights = ~weights, ...)
        else
            result <- Zelig::zelig(formula,  data = data , model = "normal.survey", weights = ~weights, subset = subset, ...)
    }
    result$predicted <- predict(result, newdata = data, na.action = na.exclude)
    class(result) = append("Regression", class(result))
result}

#' @export
print.Regression <- function(flipRegression.object)
{
    print(summary(flipRegression.object))
}


LinearRegression <- function(formula, data, weights = NULL, subset = NULL, ...) {
    if(is.factor(data[,1])) {
        WarningFactorToNumeric()
        data[,1] <- unclass(data[,1])
    }
    if (is.null(weights))
    {
        if(is.null(subset) | length(subset) == 1)
        {
            result <- Zelig::zelig(formula,  data = data , model = "ls", ...)
        }
        else
            result <- Zelig::zelig(formula,  data = data , model = "ls", subset = subset, ...)
            class(result) = append(class(result), "lm")
    }
    else
    {
        if(is.null(subset) | length(subset) == 1)-
            result <- Zelig::zelig(formula,  data = data , model = "normal.survey", weights = ~weights, ...)
        else
            result <- Zelig::zelig(formula,  data = data , model = "normal.survey", weights = ~weights, subset = subset, ...)
        class(result) = append(class(result), "survey.glm")
    }
    result$predicted <- predict(result, newdata = data, na.action = na.exclude)
    class(result) = append("Regression", class(result))
result}

cpuss <- cpus
cpus$QPopulationWeight = runif(nrow(cpus))
z <- LinearRegression(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, cpuss)


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

