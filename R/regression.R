#' Linear Regression.
#'
#' Reports the goodness-of-fit of an object.
#' @param object An object for which a summary is desired..
#' @param ... Additional arguments affecting the goodness-of-fit displayed.
#' @details
#' Uses \code{\link{Zelig::zelimethods}}
#' which depend on the \code{\link{class}} of the first argument.
#'
#' @export
LinearRegression <- function(formula, data, weights = NULL, subset = NULL, ...) {
    dependent.name <- dependentName(formula)
    dependent.variable <- data[[dependent.name]]
    if(is.factor(dependent.variable)) {
        WarningFactorToNumeric()
        data[[dependent.name]] <- dependent.variable <- unclass(dependent.variable)
    }
    if (is.null(weights))
    {
        if(is.null(subset) | length(subset) == 1)
        {
            result <- lm(formula,  data, ...)
        }
        else
        {
            data$sb <- subset
            result <- lm(formula,  data, subset = sb, ...)
        }
        #result <- zelig.result$zelig.out$z.out[[1]]
        #zelig.result$zelig.out$z.out <- NULL
        #result$zelig <- zelig.result
    }
    else
    {
        if(is.null(subset) | length(subset) == 1)
            result <- survey::svyglm(formula, weightedSurveyDesign(data, weights), ...)
        else
        {
            data$sb <- subset
            result <- survey::svyglm(formula, weightedSurveyDesign(data, weights),
                                     subset = data$sb, ...)
        }
#        data$weights = weights
#         if(is.null(subset) | length(subset) == 1)-
#             zelig.result <- Zelig::zelig(formula,  data = data , model = "normal.survey", weights = ~weights, ...)
#         elseLinea
#             zelig.result <- Zelig::zelig(formula,  data = data , model = "normal.survey", weights = ~weights, subset = subset, ...)
    }
    result$predicted <- predict(result, newdata = data, na.action = na.exclude)
    result$resid <- dependent.variable - result$predicted
    class(result) <- append("Regression", class(result))

    result
}

#' @export
print.Regression <- function(x, ...)
{
    print(summary(x), ...)
}

#' @export
predict.Regression <- function(object, ...)
{
    object$predicted
}

#' @export
fitted.Regression <- function(object, ...)
{
    object$predicted
}
#
# #' @export
# resid.Regression <- function(object, ...)
# {
#     object$resid
# }




#' Binary Logistic Regression
#'
#' @inheritParams LinearRegression
#' @export
BinaryLogit <- function(formula, data, weights = NULL, subset = NULL, ...)
{
    data <- CreatingBinaryDependentVariableIfNecessary(formula, data)
    if (is.null(weights))
    {
        if (is.null(subset) || length(subset) == 1)
        {
            result <- glm(formula,  data , family = binomial, ...)
        }
        else
        {
            data$sb <- subset
            result <- glm(formula,  data , family = binomial, subset = data$sb, ...)
        }
    }
    else
    {
        if (is.null(subset) || length(subset) == 1)
            result <- survey::svyglm(formula, weightedSurveyDesign(data, weights), family = binomial, ...)
        else
        {
            data$sb <- subset
            result <- survey::svyglm(formula, weightedSurveyDesign(data, weights),
                subset = data$sb, family = binomial, ...)
        }
    }
    # result$predicted <- predict(result, newdata = data, na.action = na.exclude)
    # result$resid <- dependent.variable - result$predicted
    class(result) <- append("Regression", class(result))
    result
}

# #' Quasi-Binomial Regression
# #'
# #' @inheritParams LinearRegression
# #' @export
# QuasiBinomialRegression = function(formula, data, weights = NULL, subset = NULL, ...)
# {
#     stopIfNotCount(formula, data)
#     if (is.null(weights))
#     {
#         if (is.null(subset) || length(subset) == 1)
#             result <- glm(formula, data = data, family = quasibinomial)
#         else
#         {
#             data$sb <- subset
#             result <- glm(formula, data = data, subset = data$sb, family = quasipoisson, ...)
#         }
#     }
#     else
#     {
#         if (is.null(subset) || length(subset) == 1)
#             result <- survey::svyglm(formula, weightedSurveyDesign(data, weights), family = quasipoisson(), ...)
#         else
#         {
#             data$sb <- subset
#             result <- survey::svyglm(formula, weightedSurveyDesign(data, weights),
#                 subset = data$sb, family = quasipoisson(), ...)
#         }
#     }
#     result$predicted <- predict(result, newdata = data, na.action = na.exclude)
#     result$resid <- dependentVariable(formula, data) - result$predicted
#     class(result) <- append("Regression", class(result))
#     result
# }

#' Poisson Regression
#'
#' @inheritParams LinearRegression
#' @export
PoissonRegression = function(formula, data, weights = NULL, subset = NULL, ...)
{
    stopIfNotCount(formula, data)
    if (is.null(weights))
    {
        if (is.null(subset) || length(subset) == 1)
            result <- glm(formula, data, family = poisson)
        else
        {
            data$sb = subset
            result <- glm(formula, data, subset = data$sb, family = poisson, ...)
        }
    }
    else
    {
        if (is.null(subset) || length(subset) == 1)
            result <- survey::svyglm(formula, weightedSurveyDesign(data, weights), family = poisson(), ...)
        else
        {
            data$sb <- subset
            result <- survey::svyglm(formula, weightedSurveyDesign(data, weights),
                subset = data$sb, family = poisson(), ...)
        }
    }
#     result$predicted <- predict(result, newdata = data, na.action = na.exclude)
#     result$resid <- dependentVariable(formula, data) - result$predicted
    class(result) <- append("Regression", class(result))
    result
}

# generalizedLinearModel = function(formula, data, weights = NULL, subset = NULL,
#     family = c(normal, binomial, poisson, quasipoisson)[1], ...)
# {
#     if (is.null(weights))
#     {
#         if (is.null(subset) || length(subset) == 1)
#             result <- glm(formula, data = data, family = family)
#         else
#         {
#             data$sb <- subset
#             result <- glm(formula, data = data, subset = data$sb, family = family, ...)
#         }
#     }
#     else
#     {
#         if(is.null(subset) || length(subset) == 1)
#             result <- survey::svyglm(formula, weightedSurveyDesign(data, weights), family = family, ...)
#         else
#         {
#             data$sb <- subset
#             result <- survey::svyglm(formula, weightedSurveyDesign(data, weights),
#                 subset = data$sb, family = family, ...)
#         }
#     }
#     result$predicted <- predict(result, newdata = data, na.action = na.exclude)
#     result$resid <- dependentVariable(formula, data) - result$predicted
#     class(result) <- append("Regression", class(result))
#     result
# }


#' Quasi-Poisson Regression
#'
#' @inheritParams LinearRegression
#' @export
QuasiPoissonRegression = function(formula, data, weights = NULL, subset = NULL, ...)
{
    stopIfNotCount(formula, data)
    if (is.null(weights))
    {
        if (is.null(subset) || length(subset) == 1)
            result <- glm(formula, data, family = quasipoisson)
        else
        {
            data$sb <- subset
            result <- glm(formula, data, subset = data$sb, family = quasipoisson, ...)
        }
    }
    else
    {
        if (is.null(subset) || length(subset) == 1)
            result <- survey::svyglm(formula, weightedSurveyDesign(data, weights),
                family = quasipoisson(), ...)
        else
        {
            data$sb <- subset
            result <- survey::svyglm(formula, weightedSurveyDesign(data, weights),
                subset = data$sb, family = quasipoisson(), ...)
        }
    }
#     result$predicted <- predict(result, newdata = data, na.action = na.exclude)
#     result$resid <- dependentVariable(formula, data) - result$predicted
    class(result) <- append("Regression", class(result))
    result
}

#' Ordered Logistic Regression
#'
#' @inheritParams LinearRegression
#' @export
OrderedLogit = function(formula, data, weights = NULL, subset = NULL, ...)
{
    dependent.variable = dependentVariable(formula, data)
    if (!is.ordered(dependent.variable))
    {
        warningNotOrdered()
        data[[dependentName(formula)]] = ordered(dependent.variable)
    }
    if (is.null(weights))
    {
        if(is.null(subset) || length(subset) == 1)
            result <- MASS::polr(formula, data, Hess = TRUE, ...)
        else
        {
            data$sb <- subset
            result <- MASS::polr(formula, data, subset = data$sb, Hess = TRUE, ...)
        }
    }
    else
    {
        if(is.null(subset) || length(subset) == 1)
            result <- MASS::polr(formula, data, weights = weights, Hess = TRUE,  ...)
        else
        {
            data$sb <- subset
            result <- MASS::polr(formula, data, subset = data$sb, weights = weights, Hess = TRUE,  ...)
        }
    }
    # result$predicted <- predict(result, newdata = data, na.action = na.exclude)
    # result$resid <- dependentVariable(formula, data) - result$predicted
    class(result) <- append("Regression", class(result))
    result
}


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

# a1 <- rnorm(1000)
# b1 <- c1 <- d1 <- rep(NA,1000)
# y1 <- a1
# a2 <- rnorm(1000)
# b2 <- rnorm(1000)
# c2 <- rnorm(1000)
# d2 <- rnorm(1000)
# y2 <- (a2 + b2 + c2 + d2)/4
# dat <- data.frame(y = c(y1, y2), a = c(a1, a2), b = c(b1, b2), c = c(c1, c2), d = c(d1, d2))
# psych::setCor(1, 2:5, data = dat)
