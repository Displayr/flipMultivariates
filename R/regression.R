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
            result <- lm(formula,  data = data, ...)
        }
        else
        {
            data$sb = subset
            result <- lm(formula,  data = data, subset = sb, ...)
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
            data$sb = subset
	        result <- survey::svyglm(formula, weightedSurveyDesign(data, weights),
	                                 subset = sb, ...)
         }
#        data$weights = weights
#         if(is.null(subset) | length(subset) == 1)-
#             zelig.result <- Zelig::zelig(formula,  data = data , model = "normal.survey", weights = ~weights, ...)
#         elseLinea
#             zelig.result <- Zelig::zelig(formula,  data = data , model = "normal.survey", weights = ~weights, subset = subset, ...)
    }
    #result$predicted <- predict(result, newdata = data, na.action = na.exclude)
    #result$resid <- dependent.variable - result$predicted
    class(result) = append("Regression", class(result))
result}

#' @export
print.Regression <- function(Regression.object)
{
    print(summary(Regression.object))
}

#' @export
predict.Regression <- function(Regression.object)
{
    Regression.object$predicted
}

#' @export
fitted.Regression <- function(Regression.object)
{
    Regression.object$predicted
 }
#
# #' @export
# resid.Regression <- function(Regression.object)
# {
#     Regression.object$resid
# }




#' @export
BinaryLogit <- function(formula, data, weights = NULL, subset = NULL, ...)
{
    data <- CreatingBinaryDependentVariableIfNecessary(formula, data)
    if (is.null(weights))
    {
        if(is.null(subset) | length(subset) == 1)
        {
            result <- glm(formula,  data = data , family = binomial, ...)
        }
        else
        {
            data$sb = subset
            result <-glm(formula,  data = data , family = binomial, subset = sb, ...)
        }
    }
    else
    {
       if(is.null(subset) | length(subset) == 1)
    	    result <- survey::svyglm(formula, weightedSurveyDesign(data, weights), family = binomial, ...)
         else
         {
            data$sb = subset
	        result <- survey::svyglm(formula, weightedSurveyDesign(data, weights),subset = sb, family = binomial, ...)
         }
    }
    # result$predicted <- predict(result, newdata = data, na.action = na.exclude)
    #result$resid <- dependent.variable - result$predicted
    class(result) = append("Regression", class(result))
result}

# #' @export
# QuasiBinomialRegression = function(formula, data, weights = NULL, subset = NULL, ...)
# {
#     stopIfNotCount(formula, data)
# 	if (is.null(weights))
# 	{
#         if(is.null(subset) | length(subset) == 1)
#             result <- glm(formula, data = data, family = quasibinomial)
#         else
#         {
#             data$sb = subset
#             result <- glm(formula, data = data, subset = sb, family = quasipoisson, ...)
#         }
# 	}
#     else
#     {
#        if(is.null(subset) | length(subset) == 1)
#     	    result <- survey::svyglm(formula, weightedSurveyDesign(data, weights), family = quasipoisson(), ...)
#          else
#          {
#             data$sb = subset
# 	        result <- survey::svyglm(formula, weightedSurveyDesign(data, weights),subset = sb, family = quasipoisson(), ...)
#          }
#     }
#     result$predicted <- predict(result, newdata = data, na.action = na.exclude)
#     result$resid <- dependentVariable(formula, data) - result$predicted
#     class(result) = append("Regression", class(result))
# result}

#' @export
PoissonRegression = function(formula, data, weights = NULL, subset = NULL, ...)
{
    stopIfNotCount(formula, data)
	if (is.null(weights))
	{
        if(is.null(subset) | length(subset) == 1)
            result <- glm(formula, data = data, family = poisson)
        else
        {
            data$sb = subset
            result <- glm(formula, data = data, subset = sb, family = poisson, ...)
        }
	}
    else
    {
       if(is.null(subset) | length(subset) == 1)
    	    result <- survey::svyglm(formula, weightedSurveyDesign(data, weights), family = poisson(), ...)
         else
         {
            data$sb = subset
	        result <- survey::svyglm(formula, weightedSurveyDesign(data, weights),subset = sb, family = poisson(), ...)
         }
    }
#     result$predicted <- predict(result, newdata = data, na.action = na.exclude)
#     result$resid <- dependentVariable(formula, data) - result$predicted
    class(result) = append("Regression", class(result))
result}

# generalizedLinearModel = function(formula, data, weights = NULL, subset = NULL,
#     family = c(normal, binomial, poisson, quasipoisson)[1], ...)
# {
# 	if (is.null(weights))
# 	{
#         if(is.null(subset) | length(subset) == 1)
#             result <- glm(formula, data = data, family = family)
#         else
#         {
#             data$sb = subset
#             result <- glm(formula, data = data, subset = sb, family = family, ...)
#         }
# 	}
#     else
#     {
#        if(is.null(subset) | length(subset) == 1)
#     	    result <- survey::svyglm(formula, weightedSurveyDesign(data, weights), family = family, ...)
#          else
#          {
#             data$sb = subset
# 	        result <- survey::svyglm(formula, weightedSurveyDesign(data, weights),subset = sb, family = family, ...)
#          }
#     }
#     result$predicted <- predict(result, newdata = data, na.action = na.exclude)
#     result$resid <- dependentVariable(formula, data) - result$predicted
#     class(result) = append("Regression", class(result))
# result}


#' @export
QuasiPoissonRegression = function(formula, data, weights = NULL, subset = NULL, ...)
{
    stopIfNotCount(formula, data)
	if (is.null(weights))
	{
        if(is.null(subset) | length(subset) == 1)
            result <- glm(formula, data = data, family = quasipoisson)
        else
        {
            data$sb = subset
            result <- glm(formula, data = data, subset = sb, family = quasipoisson, ...)
        }
	}
    else
    {
       if(is.null(subset) | length(subset) == 1)
    	    result <- survey::svyglm(formula, weightedSurveyDesign(data, weights), family = quasipoisson(), ...)
         else
         {
            data$sb = subset
	        result <- survey::svyglm(formula, weightedSurveyDesign(data, weights),subset = sb, family = quasipoisson(), ...)
         }
    }
#     result$predicted <- predict(result, newdata = data, na.action = na.exclude)
#     result$resid <- dependentVariable(formula, data) - result$predicted
    class(result) = append("Regression", class(result))
result}

#' @export
OrderedLogit = function(formula, data, weights = NULL, subset = NULL, ...)
{
    dependent.variable = dependentVariable(formula, data)
    if(!is.ordered(dependent.variable))
    {
        warningNotOrdered()
        data[[dependentName(formula)]] = ordered(dependent.variable)
    }
	if (is.null(weights))
	{
        if(is.null(subset) | length(subset) == 1)
            result <- MASS::polr(formula, data, Hess = TRUE, ...)
        else
        {
            data$sb = subset
            result <- MASS::polr(formula, data, subset = sb, Hess = TRUE, ...)
        }
	}
    else
    {
       if(is.null(subset) | length(subset) == 1)
            result <- MASS::polr(formula, data = data, weights = weights, Hess = TRUE,  ...)
         else
         {
            data$sb = subset
            result <- MASS::polr(formula, data = data, subset = sb, weights = weights, Hess = TRUE,  ...)
         }
    }
    # result$predicted <- predict(result, newdata = data, na.action = na.exclude)
    #result$resid <- dependentVariable(formula, data) - result$predicted
    class(result) = append("Regression", class(result))
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

