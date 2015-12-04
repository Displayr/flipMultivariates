#'  \code{LinearRegression}Linear Regression.
#'
#' Linear regression: ordinary least squares or least squares with survey weights.
#' @param formula An object of class \code{\link{formula}} (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.
#' @param data A \code{\link{data.frame}}.
#' @param subset An optional vector specifying a subset of observations to be used in the fitting process.
#' @param weights An optional vector of sampling weights.
#' @param missing How missing data is to be treated in the regression. Options are:
#' \code{"Error if missing data"}, \code{"Exclude cases with missing data"},
#' \code{"Use partial data (pairwise)"},and \code{"Imputation"}.

#' @param robust.se Computes standard errors that are robust to violations of the assumption of constant variance, using the HC3 modification of White's estimator.
#' @param ... Additional argments to be past to  \code{\link{lm}} or, if the data
#' is weighted,  \code{\link{survey::svyglm}}.
#'
#' @details "Imputation" is performed using \code{\link{mice}}. All selected
#' outcome and predictor variables are included in the imputation. Then,
#' cases with missing values in the outcome variable are excluded from the
#' analysis (von Hippel 2007). Where "Use partial data (pairwise)" is used, if the data is weighted, a
#' synthetic data file is created by sampling with replacement in proportion to the weights,where the
#' sample size is the sum of the weights.
#' @references von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#' Improved Strategy for Analyzing Multiply Imputed Data." Sociological Methodology 37:83-117.
#' @export
LinearRegression <- function(formula, data, subset = NULL, weights = NULL, missing = "Exclude cases with missing data", robust.se = FALSE, ...) {
    cl <- match.call()

    outcome.name <- outcomeName(formula)
    outcome.variable <- data[[outcome.name]]
    row.names <- rownames(data)
    if (is.factor(outcome.variable)) {
        WarningFactorToNumeric()
        data[[outcome.name]] <- outcome.variable <- unclass(outcome.variable)
    }
    if (missing == "Use partial data (pairwise)")
    {
        result <- lm(formula, data, ...)
        variable.names <- names(data)
        outcome.index <- match(outcome.name, variable.names)
        predictors.index <- (1:ncol(data))[-outcome.index]
        indices <- c(outcome.index, predictor.index)
        factors <- unlist(lapply(data[,indices]))
        if (any(factors))
            stop(paste0("Factors are not permitted when missing is set to 'Use partial data (pairwise)'.
                 Factors: ", paste(variable.names[indices][factors], collapse = ", ")))
        subset.data <- ifelse(is.null(subset), data, subset.data.frame(data, subset))
        estimation.data <- ifelse(is.null(weights), filtered.data,
                                AdjustDataToReflectWeights(filtered.data, weights))
        lm.cov <- psych::setCor(outcome.index, predictors.index, data = estimation.data)
        coefficents <- cbind(lm.cov$beta, lm.cov$se, lm.cov$t, lm.cov$Probability)
        dimnames(ans$coefficients) <- list(variable.name[predictor.index],
            c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
        intercept <- mean(weighted.data[[outcome.name]]) -
            mean(weighted.data[, predictors.index] %*% lm.cov$beta)
        result$predicted <- data[, predictors.index] %*% lm.cov$beta + intercept
        coefficents <- coefficients[c(1,1:nrow(coefficients)),]
        #coefficents[1,] <- c(intercept, NA, NA, NA)
        rownames(coefficents)[1] <- "(Intercept)"
        result$coef.matrix <- result
        rng <- range(rcorr.adjust(estimation.data, use = "pairwise.complete.obs")[[1]][[2]])
        if (rng[1] == rng[2])
            result$sample.size <- paste0("n = ", rng[1],
                " cases used in estimation, a total sample size of ", sum(subset), "\n")
        else
            result$sample.size <- paste0("Pairwise correlations have been used to estimate this regression.\n",
                                         "sample sizes for the correlations range from ", rng[1], "to", rng[2])
        if (!is.null(weights))
            result$sample.size <- paste0(result$sample.size, "Data has been resampled with probabilities proportional to the weights.\n")
    }
    else
    {
        print(dim(data))
        subset.data <- ifThen(hasSubset(subset),
            ifThen(is.null(weights), subset(data, subset),subset(data, subset & !is.na(weights))),
            data)
        print(dim(subset.data))
        estimation.data <- switch(missing, "Error if missing data" = ErrorIfMissingDataFound(subset.data),
                       "Exclude cases with missing data" = ExcludeCasesWithAnyMissingData(subset.data),
                       "Use partial data (pairwise)" = stop("Error: partial data should have already been processed."),
                       "Imputation" = SingleImputaton(subset.data, outcome.name))
        print(dim(subset.data))
        if (is.null(weights))
        {
            print("dog")
#             if (is.null(subset) || length(subset) == 1)
#             {
                result <- lm(formula, estimation.data, ...)
#             }
print(summary(lm(ltotexp ~ suppins + phylim + actlim + totchr + age + female + income, data = subset.data))      )
#             else
#             {
#                 data$sb <- subset
#                 result <- lm(formula, estimation.data, subset = data$sb, ...)
#             }
            #result <- zelig.result$zelig.out$z.out[[1]]
            #zelig.result$zelig.out$z.out <- NULL
            #result$zelig <- zelig.result
            estimation.subset <- row.names %in% rownames(estimation.data)
            if (robust.se)
                result$robust.coefficients <- lmtest::coeftest(result,
                    vcov = car::hccm(result))
        }
        else
        {
            estimation.subset <- row.names %in% rownames(estimation.data)
            estimation.weights <- weights[row.names %in% rownames(estimation.data)]
            if (robust.se)
                warningRobustInappropriate()
            # if (is.null(subset) || length(subset) == 1)
            result <- survey::svyglm(formula, weightedSurveyDesign(estimation.data, estimation.weights), ...)
            # else
#             {
#                 data$sb <- subset
#                 result <- survey::svyglm(formula, weightedSurveyDesign(estimation.data, weights),
#                                          subset = data$sb, ...)
#             }
    #        data$weights <- weights
    #         if (is.null(subset) || length(subset) == 1)
    #             zelig.result <- Zelig::zelig(formula,  data = data , model = "normal.survey", weights = ~weights, ...)
    #         else
    #             zelig.result <- Zelig::zelig(formula,  data = data , model = "normal.survey", weights = ~weights, subset = subset, ...)
        }
        result$predicted <- predict.lm(result, newdata = data, na.action = na.pass)
        result$sample.size <- paste0("n = ", sum(estimation.subset),
            " cases used in estimation, of a total sample size of ",ifelse(hasSubset(subset), sum(subset), nrow(data)), ".\n")
        if (!is.null(weights))
            result$sample.size <- paste0(result$sample.size, "Data has been weighted.\n")
        result$sample.size <- paste0(result$sample.size,
            switch(missing, "Error if missing data" = "",
                   "Exclude cases with missing data" = "Cases containing missing values have been excluded.\n",
                   "Imputation" = "Missing values of predictor variables have been imputed.\n"))
    }
    if (hasSubset(subset))
        result$na.action <- c(result$na.action, row.names(!subset))
    result$model <- data #over-riding the data that is automatically saved (which has had missing values removed).
    result$resid <- outcome.variable - result$predicted
    result$call <- cl
    result$robust.se <- robust.se
    result$weighted <- !is.null(weights)
    class(result) <- append("Regression", class(result))
    return(result)
}

#' @export
print.Regression <- function(Regression.object, ...)
{
    Regression.summary <- summary(Regression.object)
    if (Regression.object$robust.se)
        Regression.summary$coefficients <- Regression.object$robust.coefficients
    else
    {   #Testing to see if the variance is non-constant.
        if (!Regression.object$weighted)
        {

            bp.test <- breusch.pagan(Regression.object, Regression.summary)
            if (bp.test$p <= 0.05)
            {
                warning(paste0("A Breusch Pagan Test for non-constant variance has been failed (p = ",
                    FormatAsPValue(bp.test$p), "). Consider using Robust Standard Errors."))
                outcome.variable <- outcomeVariableFromModel(Regression.object)
            }
        }
    }
    # Inserting the coefficients from the
    if (!is.null(Regression.object$coef.matrix)) # Partial data.
        Regression.summary$coefficients <- Regression.object$coef.matrix
    outcome.variable <- outcomeVariableFromModel(Regression.object)
    if (length(unique(outcome.variable)) == 2)
        warning(paste0("The outcome variable contains only two unique values. A BinaryLogit may be
                       more appropriate."))
    else
    {
        if (isCount(outcome.variable))
            warning(paste0("The outcome variable appears to contain count data (i.e., the values are non-negative integers). A count data model may be more appropriate (e.g., Poisson Regression)."))
    }
    print(Regression.summary, ...)
    cat(Regression.object$sample.size)
    if (Regression.object$robust.se)
        cat("Heteroscedastic-robust standard errors.")

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

#' @export
fitted.values.Regression <- function(object, ...)
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

    data <- CreatingBinaryoutcomeVariableIfNecessary(formula, data)
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
    # result$resid <- outcome.variable - result$predicted
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
#     result$resid <- outcomeVariable(formula, data) - result$predicted
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
#     result$resid <- outcomeVariable(formula, data) - result$predicted
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

    outcome.variable = outcomeVariable(formula, data)
    if (!is.ordered(outcome.variable))
    {
        warningNotOrdered()
        data[[outcomeName(formula)]] = ordered(outcome.variable)
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
    # result$resid <- outcomeVariable(formula, data) - result$predicted
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
#     result$resid <- outcomeVariable(formula, data) - result$predicted
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
#     result$resid <- outcomeVariable(formula, data) - result$predicted
#     class(result) <- append("Regression", class(result))
#     result
# }




