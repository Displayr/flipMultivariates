#'  \code{Regression}Generalized Regression.
#'
#' @param formula An object of class \code{\link{formula}} (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of type specification are given under ‘Details’.
#' @param data A \code{\link{data.frame}}.
#' @param subset An optional vector specifying a subset of observations to be used in the fitting process, or,
#' the name of a variable in \code{data}. It may not be an expression.
#' \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or,
#' the name of a variable in \code{data}. It may not be an expression.
#' @param missing How missing data is to be treated in the regression. Options are:
#' \code{"Error if missing data"}, \code{"Exclude cases with missing data"},
#' \code{"Use partial data (pairwise correlations)"},and \code{"Imputation (replace missing values with estimates)"}.
#' @param robust.se Computes standard errors that are robust to violations
#' of the assumption of constant variance, using the HC1 (degrees of freedom)
#' modification of White's (1980) estimator (Long and Ervin, 2000).
#' @param type Defaults to \code{"linear"}. Other types are: \code{"Poisson"},
#' \code{"Quasi-Poisson"}, \code{"Binary Logit"}, \code{"NBD"}, and \code{"Ordered Logit"}
#' @param ... Additional argments to be past to  \code{\link{lm}} or, if the data
#' is weighted,  \code{\link{survey::svyglm}}.
#'
#' @details "Imputation (replace missing values with estimates)" is performed using multivariate imputation by chained equations
#' (predictive mean matching) with the \code{\link{mice}} package. All selected
#' outcome and predictor variables are included in the imputation, including any data excluded
#' via \code{subset} and due to having invalid weights. Then,
#' cases with missing values in the outcome variable are excluded from the
#' analysis (von Hippel 2007). Where "Use partial data (pairwise correlations)" is used, if the data is weighted, a
#' synthetic data file is created by sampling with replacement in proportion to the weights,where the
#' sample size is the sum of the weights.
#' @references von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#' Improved Strategy for Analyzing Multiply Imputed Data." Sociological Methodology 37:83-117.
#' White, H. (1980), A heteroskedastic-consistent covariance matrix estimator and a direct
#' test of heteroskedasticity. Econometrica, 48, 817-838.
#' Long, J. S. and Ervin, L. H. (2000). Using heteroscedasticity consistent standard errors
#' in the linear regression model. The American Statistician, 54( 3): 217– 224.
#' @export
Regression <- function(formula, data, subset = NULL,
                             weights = NULL,
                             missing = "Exclude cases with missing data",
                             type = "Linear",
                             robust.se = FALSE, ...)
{
    cl <- match.call()
    if (!is.null(subset))
        subset[is.na(subset)] <- FALSE
    if (!is.null(weights))
        weights[is.na(weights)] <- 0
    unfiltered.weights <- weights
    outcome.name <- outcomeName(formula)
    outcome.variable <- data[[outcome.name]]
    if (type == "Binary Logit")
        data <- CreatingBinaryDependentVariableIfNecessary(formula, data)
    else if (type == "Ordered" & !is.factor(outcome.variable))
        data[, outcome.name] <- ordered(outcome.variable)
    else if (is.factor(outcome.variable))
    {
            WarningFactorToNumeric()
            data[, outcome.name] <- outcome.variable <- unclass(outcome.variable)
    }
    row.names <- rownames(data)
    if (missing == "Use partial data (pairwise correlations)")
    {
        if (type != "Linear")
            stop(paste0("'Use partial data (pairwise)' can only be used with 'type' of 'Linear'."))
        if (robust.se)
            stop(paste0("Robust standard errors cannot be computed with 'missing' set to ", missing, "."))
        result <- linearRegressionFromCorrelations(formula, data, subset,
                             weights, outcome.variable, outcome.name, ...)
    }
    else
    {
        regression.variable.names <- all.vars(formula)
        if (missing != "Imputation (replace missing values with estimates)")
            regression.data <- data[ ,regression.variable.names]
        data.post.missing.value.treatment <- switch(missing, "Error if missing data" = ErrorIfMissingDataFound(regression.data),
                       "Exclude cases with missing data" = ExcludeCasesWithAnyMissingData(regression.data),
                       "Use partial data (pairwise correlations)" = stop("Error: partial data should have already been processed."),
                       "Imputation (replace missing values with estimates)" = SingleImputation(formula, data, outcome.name))
        post.missing.data.estimation.subset <- row.names %in% rownames(data.post.missing.value.treatment)
        estimation.subset <- flipU::IfThen(hasSubset(subset),
            subset[post.missing.data.estimation.subset],
            rep(TRUE, nrow(data.post.missing.value.treatment)))
        if (!is.null(weights))
        {
            weights <- weights[post.missing.data.estimation.subset]
            estimation.subset <- estimation.subset & weights > 0
            weights <- weights[estimation.subset]
        }
        estimation.data <- data.post.missing.value.treatment[estimation.subset, regression.variable.names] #Removing variables not used in the formula.
        missing.data.proportion <- 1 - nrow(estimation.data)/ ifelse(hasSubset(subset), sum(subset), nrow(data))
        if (missing.data.proportion > 0.20)
            warning(paste(FormatAsPercent(missing.data.proportion), "of the data is missing and has been excluded from the analysis.",
                          "Consider either filters to ensure that the data that is missing is in-line with your expectations,",
                          "or, set 'Missing Data' to another option."))
        if (missing != "Imputation (replace missing values with estimates)")
            estimation.data <- estimation.data[ ,regression.variable.names]
        if (is.null(weights))
        {
            if (type == "Linear")
                result <- lm(formula, estimation.data)
            else if (type == "Poisson" | type == "Quasi-Poisson" | type == "Binary Logit")
                result <- glm(formula, estimation.data, family = switch(type,
                        "Poisson" = poisson,
                        "Quasi-Poisson" = quasipoisson,
                        "Binary Logit" = "binomial"))
            else if (type == "Ordered")
                result <- MASS::polr(formula, estimation.data, Hess = TRUE, ...)
            else if (type == "NBD")
                result <- MASS::glm.nb(formula, estimation.data, ...)
            else
                stop("Unknown regression 'type'.")

            if (robust.se)
            {
                result$robust.coefficients <- lmtest::coeftest(result,
                    vcov = car::hccm(result, type = "hc1"))
                colnames(result$robust.coefficients)[2] <- "Robust SE"
            }
        }
        else
        {
            if (robust.se)
                warningRobustInappropriate()
            if (type == "Linear")
                result <- survey::svyglm(formula, weightedSurveyDesign(estimation.data, weights))
            else if (type == "Ordered")
            {
                estimation.data$weights <- weights
                result <- MASS::polr(formula, estimation.data, weights = weights, Hess = TRUE, ...)
            }
            else if (type == "NBD")
            {
                estimation.data$weights <- weights
                result <- MASS::glm.nb(formula, estimation.data, weights = weights, ...)
            }
            else
            {
                result <- switch(type,
                                 "Binary Logit" = survey::svyglm(formula, weightedSurveyDesign(estimation.data, weights), family = binomial()),
                                 "Poisson" = survey::svyglm(formula, weightedSurveyDesign(estimation.data, weights), family = poisson()),
                                 "Quasi-Poisson" = survey::svyglm(formula, weightedSurveyDesign(estimation.data, weights), family = quasipoisson()))
            }
        }
        result$flip.subset <- row.names %in% rownames(estimation.data)
        missing.data <- any(!post.missing.data.estimation.subset)
        if (missing == "Imputation (replace missing values with estimates)")
            data[post.missing.data.estimation.subset, ] = data.post.missing.value.treatment
        result$flip.predicted.values <- predict(result, newdata = data, na.action = na.pass)
        fitted.values <- fitted(result)
        result$flip.fitted.values <- rep(NA, total.n <- nrow(data))
        result$flip.fitted.values[result$flip.subset] <- fitted.values
        result$sample.size <- paste0("n = ", sum(estimation.subset)," cases used in estimation")
        result$sample.size <- paste0(result$sample.size, ifelse(!missing.data, ".\n",paste0(", of a total sample size of ",
            ifelse(hasSubset(subset), sum(subset), total.n), ".\n")))
        if (!is.null(weights))
            result$sample.size <- paste0(result$sample.size, "Data has been weighted.\n")
        if(missing.data | missing == "Imputation (replace missing values with estimates)")
            result$sample.size <- paste0(result$sample.size,
                switch(missing, "Error if missing data" = "",
                   "Exclude cases with missing data" = "Cases containing missing values have been excluded.\n",
                   "Imputation (replace missing values with estimates)" = "Missing values of predictor variables have been imputed.\n"))

    }
    result$summary  <- summary(result)
    # Inserting the coefficients from the partial data.
    if (!is.null(result$partial.coefs)) # Partial data.
        result$summary$coefficients <- result$partial.coefs
    result$model <- data #over-riding the data that is automatically saved (which has had missing values removed).
    result$summary$call <- result$call <- cl
    result$robust.se <- robust.se
    class(result) <- append("Regression", class(result))
    result$type = type
    result$flip.weights <- unfiltered.weights
    result$flip.residuals <- unclassIfNecessary(outcome.variable) - unclassIfNecessary(result$flip.predicted.values)#Note this occurs after summary, to avoid stuffing up summary, but before Breusch Pagan, for the same reason.
    return(result)
}




linearRegressionFromCorrelations <- function(formula, data, subset = NULL,
                             weights = NULL, outcome.variable, outcome.name, ...)


{
    result <- lm(formula, data, ...)
    variable.names <- names(data)
    formula.names <- all.vars(formula)
    indices <- match(formula.names, variable.names)
    outcome.index <- match(outcome.name, variable.names)
    predictors.index <- indices[-match(outcome.name, formula.names)]
    indices <- c(outcome.index, predictors.index)
    factors <- unlist(lapply(data[,indices], is.factor))
    if (any(factors))
        stop(paste0("Factors are not permitted when missing is set to 'Use partial data (pairwise)'.
             Factors: ", paste(variable.names[indices][factors], collapse = ", ")))
    subset.data <- flipU::IfThen(is.null(subset), data, subset(data, subset))
    weights <- flipU::IfThen(is.null(subset), weights, subset(weights, subset))
    # Taking the print chart statement out of setCor.
    .pairwise.regression <- psych::setCor
    n <- length(body(.pairwise.regression))
    while(as.character(body(.pairwise.regression)[n]) != "setCor.diagram(set.cor, main = main)")
        n <- n - 1
    body(.pairwise.regression)[n] <- NULL
    estimation.data <- flipU::IfThen(is.null(weights), subset.data,
                            AdjustDataToReflectWeights(subset.data, weights))
#     result$lm.cov <- lm.cov <- .pairwise.regression(outcome.index, predictors.index,
#         data = estimation.data, std = FALSE)
     result$lm.cov <- lm.cov <- .pairwise.regression(outcome.index, predictors.index,
         data = estimation.data, std = TRUE)
    scaled.beta <- as.matrix(lm.cov$beta)
    sds.independent = apply(estimation.data[, predictors.index], 2, sd, na.rm = TRUE)
    sd.dependent <- sd(estimation.data[, outcome.index], na.rm = TRUE)
    beta <- scaled.beta / (sds.independent / sd.dependent)
    se <- lm.cov$se / (sds.independent / sd.dependent)
    partial.coefs <- cbind(beta, se, lm.cov$t, lm.cov$Probability)
    dimnames(partial.coefs) <- list(variable.names[predictors.index],
        c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    beta <- as.matrix(lm.cov$beta)
    fitted <- as.matrix(estimation.data[, predictors.index]) %*% beta
    intercept <- mean(estimation.data[, outcome.name], na.rm = TRUE) - mean(fitted, na.rm = TRUE)
    fitted <- as.matrix(data[, predictors.index]) %*% beta
    result$flip.predicted.values <- result$flip.fitted.values <- fitted + intercept
    partial.coefs <- partial.coefs[c(1,1:nrow(partial.coefs)),]
    partial.coefs[1,] <- c(intercept, NA, NA, NA)
    rownames(partial.coefs)[1] <- "(Intercept)"
    result$partial.coefs <- partial.coefs
    result$coef <- partial.coefs[, 1]
    # print(RcmdrMisc::rcorr.adjust(estimation.data, use = "pairwise.complete.obs"))
    pairwise.n <- crossprod(!is.na(estimation.data))
    rng <- range(pairwise.n[lower.tri(pairwise.n)])
    if (rng[1] == rng[2])
        result$sample.size <- paste0("n = ", rng[1],
            " cases used in estimation.\n")
    else
        result$sample.size <- paste0("Pairwise correlations have been used to estimate this regression.\n",
                                     "Sample sizes for the correlations range from ", rng[1], " to ", rng[2], ".")
    if (!is.null(weights))
        result$sample.size <- paste0(result$sample.size, "Data has been resampled with probabilities proportional to the weights.\n")
    result$flip.subset <- !is.na(data[outcome.index]) & !is.na(fitted) & (rownames(data) %in% rownames(estimation.data))
#     print(sum(result$flip.subset))
#     print(summary(result$flip.fitted.values[result$flip.subset]))
    result
}


#' @export
print.Regression <- function(Regression.object, ...)
{
    Regression.summary <- Regression.object$summary

    if (Regression.object$robust.se)
        Regression.summary$coefficients <- Regression.object$robust.coefficients
    else
    {   #Testing to see if the variance is non-constant.
#         if (!Regression.object$weighted)# & is.null(Regression.object$partial.coefs))
#         {
        if (Regression.object$type == "Linear")
        {
            bp.test <- BreuschPagan(Regression.object)
            if (bp.test$p <= 0.05)
            {
                suggest <- ifelse(Regression.object$weighted,
                " This test can be misleading with weighted data (i.e., the failure of the test may not be problematic).",
                ifelse(is.null(Regression.object$partial.coefs), " Or, consider using Robust Standard Errors.", ""))
                warning(paste0("A Breusch Pagan Test for non-constant variance has been failed (p = ",
                    FormatAsPValue(bp.test$p), "). A plot of the residuals versus the fitted values of the outcome variable may be useful (Insert > Advanced > Regression > Plots > Residuals vs Fitted). A transformation of the outcome or predictor variables may solve this problem.",
                    suggest, "\n"))
                outcome.variable <- outcomeVariableFromModel(Regression.object)
            }
         }
    }
    outcome.variable <- outcomeVariableFromModel(Regression.object)
    if (length(unique(outcome.variable)) == 2)
        warning(paste0("The outcome variable contains only two unique values. A BinaryLogit may be
                       more appropriate."))
    else
    {
        if (Regression.object$type == "Linear" & isCount(outcome.variable))
            warning(paste0("The outcome variable appears to contain count data (i.e., the values are non-negative integers). A limited dependent variable regression may be more appropriate (e.g., Quasi-Poisson Regression, Ordered Logit)."))
    }
    print(Regression.summary, ...)
    if (!is.null(Regression.object$lm.cov))
        cat(paste0("Partial-data Multiple R-squared ", FormatAsReal(Regression.object$lm.cov$R2, 4), " (the R-squared and F above are based only on complete cases).\n"))
    cat(Regression.object$sample.size)
    if (Regression.object$robust.se)
        cat("Heteroscedastic-robust standard errors.")

}


#' @export
predict.Regression <- function(object, ...)
{
    object$flip.predicted.values
}

#' @export
fitted.Regression <- function(object, ...)
{
    object$flip.fitted.values
}

#' @export
fitted.values.Regression <- function(object, ...)
{
    object$flip.fitted.values
}


# #' @export
# resid.Regression <- function(object, ...)
# {
#     object$flip.residuals
# }

#' @export
residuals.Regression <- function(object, ...)
{
    object$flip.residuals
}

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


