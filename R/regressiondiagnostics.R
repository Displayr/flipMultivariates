
#' \code{ConfusionMatrixFromVariables}
#'
#' @param observed A \code{factor}.
#' @param predicted A \code{factor}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#'
#' @details A contingency table showing the observed versus predicted values from a model.
#' @export
ConfusionMatrixFromVariables <- function(observed, predicted, subset = NULL, weights = NULL)
{
    # if(!is.factor(observed))
    #     observed <- factor(observed)
    # if(!is.factor(observed))
    #     predicted <- factor(predicted)
    # levels.observed <- levels(observed)
    # n <- length(levels)
    # # Ensuring that the observed and predicted values line up)
    # predicted <- factor(match(predicted, levels.observed))
    # levels(predicted) <- levels.observed
    if (is.null(weights))
    {
        if (is.null(subset))
            return(xtabs(~ observed + predicted))
        else
            return(xtabs(~ observed + predicted, subset = subset))
    }
    else
    {
        if (is.null(subset))
            return(xtabs(weights ~ observed + predicted))
        else
            return(xtabs(weights ~ observed + predicted, subset = subset))
    }
}


#' \code{ConfusionMatrixFromVariablesLinear}
#'
#' @param observed A \code{factor}.
#' @param predicted A \code{factor}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#'
#' @details A contingency table showing the observed versus predicted values from a model, for linear models.
#' Prediced values are assigned the value of the closest observed value.
#' @export
ConfusionMatrixFromVariablesLinear <- function(observed, predicted, subset = NULL, weights = NULL)
{
    if (is.factor(observed))
        observed <- as.numeric(observed)
    if (is.factor(predicted))
        predicted <- as.numeric(predicted)
    unique.observed <- unique(observed)
    unique.observed <- unique.observed[!is.na(unique.observed)]
    predicted.na <- is.na(predicted)
    if(any(predicted.na))
        predicted[predicted.na] <- -Inf
    predicted <- sapply(predicted, function(x) unique.observed[which.min(abs(unique.observed - x))])
    predicted[predicted.na] <- NA
    ConfusionMatrixFromVariables(observed, predicted)
}



#' \code{ConfusionMatrix}
#'
#' @param obj A model with an outcome variable.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @details The proportion of observed values that take the same values as the predicted values.
#' Where the outcome
#' variable in the model is not a factor and not a count, predicted values are assigned to the closest observed
#' value.
#' @export
ConfusionMatrix <- function(obj, subset = NULL, weights = NULL)
{
    observed <- Observed(obj)
    predicted <- predict(obj)

    if (is(obj,"Regression") & obj$type == "Linear")
        return(ConfusionMatrixFromVariablesLinear(observed, predicted, subset, weights))
    return(ConfusionMatrixFromVariables(observed, predicted, subset, weights))
}


#' \code{Accuracy}
#'
#' @param obj A model with an outcome variable.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @details The proportion of observed values that take the same values as the predicted values.
#' Where the outcome
#' variable in the model is not a factor and not a count, predicted values are assigned to the closest observed
#' value.
#' @export
Accuracy <- function(obj, subset = NULL, weights = NULL)
{
    cm <- ConfusionMatrix(obj, subset, weights)
    n <- sum(cm)
    rnames <- rownames(cm)
    cnames <- colnames(cm)
    cm <- cm[rnames %in% cnames, cnames %in% rnames]
    correct <- sum(diag(cm))
    correct / n
}


#' @export
vif.Regression <- function (mod, ...)
{

    if(!any(c("lm", "glm") %in%  class(mod)))
    {
        mod <- Regression(as.formula(mod$call$formula),
            weights = mod$flip.weights, subset = mod$flip.subset, robust.se = mod$robust.se, data = mod$model)
    }
    car:::vif.default(mod)
}

#' \code{BreuschPagan} Breusch-Pagan test for non-constant variance.
#'
#' @param mod An object of class \code{\link{Regression}}.
#' @param show.warnings Returns a warning if the sample size is less than 4.
#' @details Weights are ignored when conducting the test.
#' @export
BreuschPagan <- function(Regression.object, show.warnings = TRUE)#, var.formula)
{
    if (class(Regression.object)[1] != "Regression")
        return(car::ncvTest(Regression.object))
    #Modified from car::ncvTest, to addess different size of input data.
    subset <- Regression.object$flip.subset
    if(sum(subset) < 4)
    {
        if (show.warnings)
            warning("Sample size too small for Breusch-Pagan test.")
        return(list(ChiSquare = NA, Df = NA, p = 1,
            test = "Breusch-Pagan test of Non-constant Variance"))
    }
    residuals <- residuals(Regression.object)[subset]
    fitted.values <- fitted.values(Regression.object)[subset]
    squared.residuals <- residuals^2
    r.squared <- 1 - var(residuals) / var(fitted.values)
    if (r.squared> 0.9999) {
        Chisq <- 0.0
    }
    else
    {
        U <- squared.residuals / mean(squared.residuals)#mean.squared.error#sum(squared.residuals)
        mod <- lm(U ~ fitted.values)#, subset = Regression.object$subset)
        SS <- anova(mod)$"Sum Sq"
        RegSS <- sum(SS) - SS[length(SS)]
        Chisq <- RegSS/2
    }
    result <- list(#formula = var.formula, formula.name = "Variance",
        ChiSquare = Chisq, Df = 1, p = pchisq(Chisq, 1, lower.tail = FALSE),
        test = "Breusch-Pagan test of Non-constant Variance")
    class(result) <- "chisqTest"
    result
}
