a <- model.frame(ncases/(ncases+ncontrols) ~ agegp + tobgp + alcgp,
                 data = esoph, subset = (ncases+ncontrols) > 50)

esoph$ncontrols[1:10] <- NA
a <- model.frame(ncases/(ncases+ncontrols) ~ agegp + tobgp + alcgp,
                 data = esoph, weights = (ncases+ncontrols), na.action = NULL)

model.response(a)
model.extract(a, "weights")





timlm <- function (formula, data, subset, weights, na.action, method = "qr",
    model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,
    contrasts = NULL, offset, ...)
{
print(nrow(data))
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action",
        "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
mf$na.action <- NULL
print(mf)

    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
print(dim(mf))
    if (method == "model.frame")
        return(mf)
    else if (method != "qr")
        warning(gettextf("method = '%s' is not supported. Using 'qr'",
            method), domain = NA)
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    w <- as.vector(model.weights(mf))
print(length(w))
    if (!is.null(w) && !is.numeric(w))
        stop("'weights' must be a numeric vector")
    offset <- as.vector(model.offset(mf))
    if (!is.null(offset)) {
        if (length(offset) != NROW(y))
            stop(gettextf("number of offsets is %d, should equal %d (number of observations)",
                length(offset), NROW(y)), domain = NA)
    }
print(length(w))
    if (is.empty.model(mt)) {
        x <- NULL
        z <- list(coefficients = if (is.matrix(y)) matrix(, 0,
            3) else numeric(), residuals = y, fitted.values = 0 *
            y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w !=
            0) else if (is.matrix(y)) nrow(y) else length(y))
        if (!is.null(offset)) {
            z$fitted.values <- offset
            z$residuals <- y - offset
        }
    }
    else {
        x <- model.matrix(mt, mf, contrasts)
        z <- if (is.null(w))
            lm.fit(x, y, offset = offset, singular.ok = singular.ok,
                ...)
        else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok,
            ...)
    }
    class(z) <- c(if (is.matrix(y)) "mlm", "lm")
    z$na.action <- attr(mf, "na.action")
    z$offset <- offset
    z$contrasts <- attr(x, "contrasts")
    z$xlevels <- .getXlevels(mt, mf)
    z$call <- cl
    z$terms <- mt
    if (model)
        z$model <- mf
    if (ret.x)
        z$x <- x
    if (ret.y)
        z$y <- y
    if (!qr)
        z$qr <- NULL
    z
}

timlm(log(Overall) ~ Fees + exp(Interest) + Phone*Branch + Online + ATM, data = bank, weights = ID, subset = ID > 100)





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
#' @param robust.se Computes standard errors that are robust to violations
#' of the assumption of constant variance, using the HC1 (degrees of freedom)
#' modification of White's (1980) estimator (Long and Ervin, 2000).
#' @param ... Additional argments to be past to  \code{\link{lm}} or, if the data
#' is weighted,  \code{\link{survey::svyglm}}.
#'
#' @details "Imputation" is performed using multivariate imputation by chained equations
#' (predictive mean matching) with the \code{\link{mice}} package. All selected
#' outcome and predictor variables are included in the imputation, including any data excluded
#' via \code{subset}. Then,


#' cases with missing values in the outcome variable are excluded from the
#' analysis (von Hippel 2007). Where "Use partial data (pairwise)" is used, if the data is weighted, a
#' synthetic data file is created by sampling with replacement in proportion to the weights,where the
#' sample size is the sum of the weights.
#' @references von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#' Improved Strategy for Analyzing Multiply Imputed Data." Sociological Methodology 37:83-117.
#' White, H. (1980), A heteroskedastic-consistent covariance matrix estimator and a direct
#' test of heteroskedasticity. Econometrica, 48, 817-838.
#' Long, J. S. and Ervin, L. H. (2000). Using heteroscedasticity consistent standard errors
#' in the linear regression model. The American Statistician, 54( 3): 217– 224.

#' @export
LinearRegression <- function(formula, data, subset = NULL,
                             weights = NULL,
                             missing = "Exclude cases with missing data",
                             robust.se = FALSE, ...) {
    cl <- match.call()
    if (!is.null(weights) & is.character(weights))
        weights <- data[, weights]
    if (!is.null(subset) & is.character(subset))
        weights <- data[, subset]
    outcome.name <- outcomeName(formula)
    outcome.variable <- data[[outcome.name]]
    row.names <- rownames(data)
    if (is.factor(outcome.variable)) {
        WarningFactorToNumeric()
        data[[outcome.name]] <- outcome.variable <- unclass(outcome.variable)
    }
    if (missing == "Use partial data (pairwise)")
    {
        if (robust.se)
            stop(paste0("Robust standard errors cannot be computed with 'missing' set to ", missing, "."))
        result <- linearRegressionFromCorrelations(formula, data, subset,
                             weights, outcome.variable, outcome.name, ...)
    }
    else
    {
        subset.data <- ifThen(hasSubset(subset),
            ifThen(is.null(weights), subset(data, subset),subset(data, subset & !is.na(weights))),
            data)
        subset.data <- subset.data[, all.vars(formula)] #Removing variables not used in the formula.
        estimation.data <- switch(missing, "Error if missing data" = ErrorIfMissingDataFound(subset.data),
                       "Exclude cases with missing data" = ExcludeCasesWithAnyMissingData(subset.data),
                       "Use partial data (pairwise)" = stop("Error: partial data should have already been processed."),
                       "Imputation" = SingleImputation(formula, subset.data, outcome.name))
        if (is.null(weights))
        {
#             if (is.null(subset) || length(subset) == 1)
#             {
                result <- lm(formula, estimation.data, ...)
#             }
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
            {
                result$robust.coefficients <- lmtest::coeftest(result,
                    vcov = car::hccm(result, type = "hc1"))
                colnames(result$robust.coefficients)[2] <- "Robust SE"
            }
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
        missing.data <- ifelse(hasSubset(subset), sum(subset), length(outcome.variable)) > sum(estimation.subset)
        result$predicted <- predict.lm(result, newdata = data, na.action = na.pass)
        result$sample.size <- paste0("n = ", sum(estimation.subset)," cases used in estimation")
        result$sample.size <- paste0(result$sample.size, ifelse(!missing.data, ".\n",paste0(", of a total sample size of ",
            ifelse(hasSubset(subset), sum(subset), nrow(data)), ".\n")))
        if (!is.null(weights))
            result$sample.size <- paste0(result$sample.size, "Data has been weighted.\n")
        if(missing.data || missing == "Imputation")
            result$sample.size <- paste0(result$sample.size,
                switch(missing, "Error if missing data" = "",
                   "Exclude cases with missing data" = "Cases containing missing values have been excluded.\n",
                   "Imputation" = "Missing values of predictor variables have been imputed.\n"))
        result$subset <- estimation.subset
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
    subset.data <- ifThen(is.null(subset), data, subset(data, subset))
    # Taking the print chart statement out of setCor.
    .pairwise.regression <- psych::setCor
    n <- length(body(.pairwise.regression))
    while(as.character(body(.pairwise.regression)[n]) != "setCor.diagram(set.cor, main = main)")
        n <- n - 1
    body(.pairwise.regression)[n] <- NULL
    estimation.data <- ifThen(is.null(weights), subset.data,
                            AdjustDataToReflectWeights(subset.data, weights))
    result$lm.cov <- lm.cov <- .pairwise.regression(outcome.index, predictors.index,
        data = estimation.data, std = FALSE)
    partial.coefs <- cbind(lm.cov$beta, lm.cov$se, lm.cov$t, lm.cov$Probability)
    dimnames(partial.coefs) <- list(variable.names[predictors.index],
        c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    beta <- as.matrix(lm.cov$beta)
    fitted <- as.matrix(estimation.data[, predictors.index]) %*% beta
    intercept <- mean(estimation.data[, outcome.name], na.rm = TRUE) - mean(fitted, na.rm = TRUE)
    fitted <- as.matrix(data[, predictors.index]) %*% beta
    result$predicted <- fitted + intercept
    partial.coefs <- partial.coefs[c(1,1:nrow(partial.coefs)),]
    partial.coefs[1,] <- c(intercept, NA, NA, NA)
    rownames(partial.coefs)[1] <- "(Intercept)"
    result$partial.coefs <- partial.coefs
    rng <- range(RcmdrMisc::rcorr.adjust(estimation.data, use = "pairwise.complete.obs")[[1]][[2]])
    if (rng[1] == rng[2])
        result$sample.size <- paste0("n = ", rng[1],
            " cases used in estimation.\n")
    else
        result$sample.size <- paste0("Pairwise correlations have been used to estimate this regression.\n",
                                     "Sample sizes for the correlations range from ", rng[1], " to ", rng[2], ".")
    if (!is.null(weights))
        result$sample.size <- paste0(result$sample.size, "Data has been resampled with probabilities proportional to the weights.\n")
    estimation.subset <- rownames(data) %in% rownames(estimation.data)
    result$subset <- estimation.subset
    result
}

#' @export
print.Regression <- function(Regression.object, ...)
{
    Regression.summary <- summary(Regression.object)
    if (Regression.object$robust.se)
        Regression.summary$coefficients <- Regression.object$robust.coefficients
    else
    {   #Testing to see if the variance is non-constant.
#         if (!Regression.object$weighted)# & is.null(Regression.object$partial.coefs))
#         {
        bp.test <- breusch.pagan(Regression.object, Regression.summary)
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
        # }
    }
    # Inserting the coefficients from the partial data.
    if (!is.null(Regression.object$partial.coefs)) # Partial data.
        Regression.summary$coefficients <- Regression.object$partial.coefs
    outcome.variable <- outcomeVariableFromModel(Regression.object)
    if (length(unique(outcome.variable)) == 2)
        warning(paste0("The outcome variable contains only two unique values. A BinaryLogit may be
                       more appropriate."))
    else
    {
        if (isCount(outcome.variable))
            warning(paste0("The outcome variable appears to contain count data (i.e., the values are non-negative integers). A limited dependent variable model may be more appropriate (e.g., Poisson Regression, Ordered Logit)."))
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



# LinearRegression <- function(formula, data, subset = NULL,
#                              weights = NULL,
#                              missing = "Exclude cases with missing data",
#                              robust.se = FALSE, ...) {
#     cl <- match.call()
#     if (!is.null(weights) & is.character(weights))
#         weights <- data[, weights]
#     if (!is.null(subset) & is.character(subset))
#         weights <- data[, subset]
#     outcome.name <- outcomeName(formula)
#     outcome.variable <- data[[outcome.name]]
#     row.names <- rownames(data)
#     if (is.factor(outcome.variable)) {
#         WarningFactorToNumeric()
#         data[[outcome.name]] <- outcome.variable <- unclass(outcome.variable)
#     }
#     if (missing == "Use partial data (pairwise)")
#     {
#         if (robust.se)
#             stop(paste0("Robust standard errors cannot be computed with 'missing' set to ", missing, "."))
#         result <- linearRegressionFromCorrelations(formula, data, subset,
#                              weights, outcome.variable, outcome.name, ...)
#     }
#     else
#     {
#         subset.data <- ifThen(hasSubset(subset),
#             ifThen(is.null(weights), subset(data, subset),subset(data, subset & !is.na(weights))),
#             data)
#         subset.data <- subset.data[, all.vars(formula)] #Removing variables not used in the formula.
#         estimation.data <- switch(missing, "Error if missing data" = ErrorIfMissingDataFound(subset.data),
#                        "Exclude cases with missing data" = ExcludeCasesWithAnyMissingData(subset.data),
#                        "Use partial data (pairwise)" = stop("Error: partial data should have already been processed."),
#                        "Imputation" = SingleImputation(formula, subset.data, outcome.name))
#         if (is.null(weights))
#         {
# #             if (is.null(subset) || length(subset) == 1)
# #             {
#                 result <- lm(formula, estimation.data, ...)
# #             }
# #             else
# #             {
# #                 data$sb <- subset
# #                 result <- lm(formula, estimation.data, subset = data$sb, ...)
# #             }
#             #result <- zelig.result$zelig.out$z.out[[1]]
#             #zelig.result$zelig.out$z.out <- NULL
#             #result$zelig <- zelig.result
#             estimation.subset <- row.names %in% rownames(estimation.data)
#             if (robust.se)
#             {
#                 result$robust.coefficients <- lmtest::coeftest(result,
#                     vcov = car::hccm(result, type = "hc1"))
#                 colnames(result$robust.coefficients)[2] <- "Robust SE"
#             }
#         }
#         else
#         {
#             estimation.subset <- row.names %in% rownames(estimation.data)
#             estimation.weights <- weights[row.names %in% rownames(estimation.data)]
#             if (robust.se)
#                 warningRobustInappropriate()
#             # if (is.null(subset) || length(subset) == 1)
#             result <- survey::svyglm(formula, weightedSurveyDesign(estimation.data, estimation.weights), ...)
#             # else
# #             {
# #                 data$sb <- subset
# #                 result <- survey::svyglm(formula, weightedSurveyDesign(estimation.data, weights),
# #                                          subset = data$sb, ...)
# #             }
#     #        data$weights <- weights
#     #         if (is.null(subset) || length(subset) == 1)
#     #             zelig.result <- Zelig::zelig(formula,  data = data , model = "normal.survey", weights = ~weights, ...)
#     #         else
#     #             zelig.result <- Zelig::zelig(formula,  data = data , model = "normal.survey", weights = ~weights, subset = subset, ...)
#         }
#         missing.data <- ifelse(hasSubset(subset), sum(subset), length(outcome.variable)) > sum(estimation.subset)
#         result$predicted <- predict.lm(result, newdata = data, na.action = na.pass)
#         result$sample.size <- paste0("n = ", sum(estimation.subset)," cases used in estimation")
#         result$sample.size <- paste0(result$sample.size, ifelse(!missing.data, ".\n",paste0(", of a total sample size of ",
#             ifelse(hasSubset(subset), sum(subset), nrow(data)), ".\n")))
#         if (!is.null(weights))
#             result$sample.size <- paste0(result$sample.size, "Data has been weighted.\n")
#         if(missing.data || missing == "Imputation")
#             result$sample.size <- paste0(result$sample.size,
#                 switch(missing, "Error if missing data" = "",
#                    "Exclude cases with missing data" = "Cases containing missing values have been excluded.\n",
#                    "Imputation" = "Missing values of predictor variables have been imputed.\n"))
#         result$subset <- estimation.subset
#     }
#     if (hasSubset(subset))
#         result$na.action <- c(result$na.action, row.names(!subset))
#     result$model <- data #over-riding the data that is automatically saved (which has had missing values removed).
#     result$resid <- outcome.variable - result$predicted
#     result$call <- cl
#     result$robust.se <- robust.se
#     result$weighted <- !is.null(weights)
#     class(result) <- append("Regression", class(result))
#     return(result)
# }
