#'  \code{LinearRegression}Linear Regression.
#'
#' Linear regression: ordinary least squares or least squares with survey weights.
#' @param formula An object of class \code{\link{formula}} (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#' @param data A \code{\link{data.frame}}.
#' @param subset An optional vector specifying a subset of observations to be used in the fitting process.
#' @param weights An optional vector of sampling weights.
#' @param robust.se Computes standard errors that are robust to violations of the assumption of constant variance, using the HC3 modification of White's estimator.
#' @param ... Additional argments to be past to  \code{\link{lm}} or, if the data
#' is weighted,  \code{\link{survey::svyglm}}.
#'
#' @export
LinearRegression <- function(formula, data, subset = NULL, weights = NULL, robust.se = FALSE, ...) {
    cl <- match.call()

    dependent.name <- dependentName(formula)
    dependent.variable <- data[[dependent.name]]
    if (is.factor(dependent.variable)) {
        WarningFactorToNumeric()
        data[[dependent.name]] <- dependent.variable <- unclass(dependent.variable)
    }
    if (is.null(weights))
    {
        if (is.null(subset) || length(subset) == 1)
        {
            result <- lm(formula, data, ...)
        }
        else
        {
            data$sb <- subset
            result <- lm(formula, data, subset = data$sb, ...)
        }
        #result <- zelig.result$zelig.out$z.out[[1]]
        #zelig.result$zelig.out$z.out <- NULL
        #result$zelig <- zelig.result
    }
    else
    {
        if (robust.se)
            warningRobustInappropriate()
        if (is.null(subset) || length(subset) == 1)
            result <- survey::svyglm(formula, weightedSurveyDesign(data, weights), ...)
        else
        {
            data$sb <- subset
            result <- survey::svyglm(formula, weightedSurveyDesign(data, weights),
                                     subset = data$sb, ...)
        }
#        data$weights <- weights
#         if (is.null(subset) || length(subset) == 1)
#             zelig.result <- Zelig::zelig(formula,  data = data , model = "normal.survey", weights = ~weights, ...)
#         else
#             zelig.result <- Zelig::zelig(formula,  data = data , model = "normal.survey", weights = ~weights, subset = subset, ...)
    }
    result$predicted <- predict(result, newdata = data, na.action = na.exclude)
    result$resid <- dependent.variable - result$predicted
    result$call <- cl
    result$robust <- robust.se
    class(result) <- append("Regression", class(result))

    result
}

#' @export
print.Regression <- function(Regression.object, ...)
{
    Regression.summary <- summary(Regression.object)
    if (Regression.object$robust.se)
        Regression.summary$coefficients <- lmtest::coeftest(Regression.object,
          vcov = car::hccm(Regression.object))
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

#'  \code{BinaryLogit} Binary Logit Regression.
#'
#' @inheritParams LinearRegression
#' @export
BinaryLogit <- function(formula, data, subset = NULL, weights = NULL, ...)
{
    cl <- match.call()

    data <- CreatingBinaryDependentVariableIfNecessary(formula, data)
    if (is.null(weights))
    {
        if (is.null(subset) || length(subset) == 1)
        {
            result <- glm(formula, data, family = binomial, ...)
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
    result$call <- cl
    class(result) <- append("Regression", class(result))
    result
}


#' \code{PoissonRegression}
#'
#' Poisson Regression
#' @inheritParams LinearRegression
#' @export
PoissonRegression <- function(formula, data, subset = NULL, weights = NULL, ...)
{
    stopIfNotCount(formula, data)

    cl <- match.call()

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
    result$call <- cl
    class(result) <- append("Regression", class(result))

    result
}


#' \code{QuasiPoissonRegression}
#'
#' Quasi-Poisson Regression
#' @inheritParams LinearRegression
#' @export
QuasiPoissonRegression = function(formula, data, subset = NULL, weights = NULL, ...)
{
    stopIfNotCount(formula, data)

    cl <- match.call()

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
    result$call <- cl
    class(result) <- append("Regression", class(result))

    result
}

#' \code{OrderedLogit}
#' Ordered Logit Regression
#'
#' @inheritParams LinearRegression
#' @export
OrderedLogit = function(formula, data, subset = NULL, weights = NULL, ...)
{
    cl <- match.call()

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
    result$call <- cl
    class(result) <- append("Regression", class(result))

    result
}


# #' Quasi-Binomial Regression
# #'
# #' @inheritParams LinearRegression
# #' @export
# QuasiBinomialRegression = function(formula, data, subset = NULL, weights = NULL, ...)
# {
#     stopIfNotCount(formula, data)
#
#     cl <- match.call()
#
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
#     result$call <- cl
#     class(result) <- append("Regression", class(result))
#
#     result
# }

#
# #' @export
# resid.Regression <- function(object, ...)
# {
#     object$resid
# }

# generalizedLinearModel = function(formula, data, subset = NULL, weights = NULL,
#     family = c(normal, binomial, poisson, quasipoisson)[1], ...)
# {
#     cl <- match.call()
#
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
