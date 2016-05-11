
#' \code{DurbinWatson}
#'
#' @param model A 'Regression'  model.
#' @param n.permutations Number of permutations used in computing the p-value.
#' @details Computes the Durbin-Watson statistic. A permutation test is used for
#' computing the p-value. Tests to a lag of 1. Two-sided.
#' Durbin, J.; Watson, G. S. (1950). "Testing for Serial Correlation in Least Squares Regression, I". Biometrika 37 (3–4): 409–428.
#' @export
DurbinWatson <- function(model, n.permutations = 1000)
{
    set.seed(123)
    residuals <- resid(model)
    if("Regression" %in% class(model))
        residuals <- residuals[model$subset]
    r <- residuals[!is.na(residuals)]
    n <- length(residuals)
    if (n <= 2)
    {
        d = NA
        p = NA

    }
    else
    {
        .dW <- function(x)
        {
            #        flipMultivariates:::printDetails(x[-n])
            #       flipMultivariates:::printDetails(x[-1])
            sum((x[-n] - x[-1]) ^ 2) / sum(x^2)
        }
        .permute <- function(x) { .dW(sample(x)) }
        d <- .dW(r)
        replicates <- replicate(n.permutations, .permute(r))
        p = sum(if (d < 2) d > replicates else d < replicates) / n.permutations * 2
        result <- list(data.name = paste(as.character(model$call)), statistic = c("d" = d), p.value = p, method = "Durbin-Watson statistic")
        class(result) <- "htest"
    }
    result
}

#' @export
print.DurbinWatson <- function(x)
{
    cat(paste0("Durbin-Watson statistic: ", d, "\n"))
    cat(paste0("p-value: ", p, "\n"))
}


#' @export
cooks.distance.Regression <- function(model, ...)
{
    checkAcceptableModel(model, c("lm", "glm"),"cooks.distance")
    stats::cooks.distance(model$original)

    # mod <- model
    # if(!any(c("lm", "glm") %in%  class(mod)))
    # {
    #     cat("Cooks distance has been computed using a linear regression model.\n")
    #     mod <- Regression(as.formula(mod$call$formula),
    #         weights = mod$weights, subset = mod$subset,
    #         robust.se = mod$robust.se, data = mod$model)
    # }
    # model$cooks.distance
    #
    #diagnosticTest(model, "cooks.distance", ...)
}

checkAcceptableModel <- function(x, classes, diagnostic, exclude.partial.data = TRUE)
{
    if (exclude.partial.data & x$missing == "Use partial data (pairwise correlations)")
        stop(paste0("'", diagnostic, "' is not computed for model that are computed using 'Use partial data (pairwise correlations)'"))
    if (!any(classes %in% class(x$original)))
        stop(paste0("'", diagnostic, "' is not computed for models of this type or class."))
}

#' @export
vif.Regression <- function (mod, ...)
{
    checkAcceptableModel(mod, c("lm", "glm"), "vif")
    diagnosticTestFromCar(mod, "vif", ...)
    # if (mod$missing == "Use partial data (pairwise correlations)")
    #     stop(paste0("Variance Inflation Factor (VIF) is not computed for model that are computed using 'Use partial data (pairwise correlations)'"))
    # if(any(c("lm", "glm") %in%  class(mod$original)))
    #     model <- mod$original
    # {
    #     cat("The Variance Inflation Factor (VIF) has been computed using a linear regression model.\n")
    #     model <- Regression(as.formula(mod$original$call$formula),
    #         weights = mod$weights, subset = mod$subset,
    #             robust.se = mod$robust.se, data = mod$model)
    # }
    # assign(".estimation.data", model$estimation.data, envir=.GlobalEnv)
    # assign(".formula", model$formula, envir=.GlobalEnv)
    # v <- car::vif(model$original, ...)#BreuschPagan(x$original)
    # remove(".formula", envir=.GlobalEnv)
    # remove(".estimation.data", envir=.GlobalEnv)
    #v
}

#' @export
ncvTest.Regression <- function(model, ...)
{
    if (any("svyglm" %in% class(model$original)))
        stop(paste0("'ncvTest is not applicable for models with sampling weights."))

    checkAcceptableModel(model, "lm", "ncvTest")
    diagnosticTestFromCar(model, "ncvTest", ...)
    # diagnosticTest(mod, "cooks.distance", ...)
    # assign(".estimation.data", model$estimation.data, envir=.GlobalEnv)
    # assign(".formula", model$formula, envir=.GlobalEnv)
    # bp.test <- car::ncvTest(model$original, ...)#BreuschPagan(x$original)
    # remove(".formula", envir=.GlobalEnv)
    # remove(".estimation.data", envir=.GlobalEnv)
    # bp.test
}

#' @export
residualPlots.Regression <- function(model, ...)
{
    checkAcceptableModel(model, c("glm","lm"), "residualPlots")
    diagnosticTestFromCar(model, "residualPlots", ...)
}


#' @export
influenceIndexPlot.Regression <- function(model, ...)
{
    checkAcceptableModel(model, c("glm","lm"), "influenceIndexPlot")
    diagnosticTestFromCar(model, "influenceIndexPlot", ...)
}

#' @export
influencePlot.Regression <- function(model, ...)
{
    checkAcceptableModel(model, c("glm","lm"), "influencePlot")
    diagnosticTestFromCar(model, "influencePlot", ...)
}


#' @export
infIndexPlot.Regression <- function(model, ...)
{
    checkAcceptableModel(model, c("glm","lm"), "influenceIndexPlot")
    diagnosticTestFromCar(model, "influenceIndexPlot", ...)
}


#' #' @export
#' marginalModelPlots.Regression <- function(model, ...)
#' {
#'     checkAcceptableModel(model, c("glm","lm"), "residualPlots")
#'     diagnosticTestFromCar(model, "marginalModelPlots", ...)
#' }
#'

diagnosticTestFromCar<- function(x, diagnostic, ...)
{
    model <- x$original
    assign(".estimation.data", x$estimation.data, envir=.GlobalEnv)
    assign(".formula", model$formula, envir=.GlobalEnv)
    txt <- paste0("car::", diagnostic, "(model)")
    t <- eval(parse(text = txt))
    # require(car)
    # t <- switch("vif" = vif(model, ...),
    #             "ncvTest"car::ncvTest(model, ...)#BreuschPagan(x$original)
    #     # remove(".formula", envir=.GlobalEnv)
    #     # remove(".estimation.data", envir=.GlobalEnv)
    # #     bp.test
    # #
    # #
    # # }
    #     # t <- car::ncvTest(model, ...)
    # }
    # else
    #     stop(paste0("'", diagnostic, "' is not a known diagnostic for 'Regression' objects."))
    remove(".formula", envir=.GlobalEnv)
    remove(".estimation.data", envir=.GlobalEnv)
    t
}


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
    if(isCount(observed))
        predicted <- floor(predicted)
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
allEffects.Regression <- function(model, ...)
{
    assign(".estimation.data", model$estimation.data, envir=.GlobalEnv)
    effects <- effects::allEffects(model$original, ...)#BreuschPagan(x$original)
    remove(".estimation.data", envir=.GlobalEnv)
    effects
}




#' #' \code{BreuschPagan} Breusch-Pagan test for non-constant variance.
#' #'
#' #' @param mod An object of class \code{\link{Regression}}.
#' #' @param show.warnings Returns a warning if the sample size is less than 4.
#' #' @details Weights are ignored when conducting the test.
#' #' @export
#' BreuschPagan <- function(Regression.object, show.warnings = TRUE)#, var.formula)
#' {
#'     if (class(Regression.object)[1] != "Regression")
#'         return(car::ncvTest(Regression.object))
#'     #Modified from car::ncvTest, to addess different size of input data.
#'     subset <- Regression.object$subset
#'     if(sum(subset) < 4)
#'     {
#'         if (show.warnings)
#'             warning("Sample size too small for Breusch-Pagan test.")
#'         return(list(ChiSquare = NA, Df = NA, p = 1,
#'             test = "Breusch-Pagan test of Non-constant Variance"))
#'     }
#'     residuals <- residuals(Regression.object)[subset]
#'     fitted.values <- fitted.values(Regression.object)[subset]
#'     squared.residuals <- residuals^2
#'     r.squared <- 1 - var(residuals) / var(fitted.values)
#'     if (r.squared> 0.9999) {
#'         Chisq <- 0.0
#'     }
#'     else
#'     {
#'         U <- squared.residuals / mean(squared.residuals)#mean.squared.error#sum(squared.residuals)
#'         mod <- lm(U ~ fitted.values)#, subset = Regression.object$subset)
#'         SS <- anova(mod)$"Sum Sq"
#'         RegSS <- sum(SS) - SS[length(SS)]
#'         Chisq <- RegSS/2
#'     }
#'     result <- list(#formula = var.formula, formula.name = "Variance",
#'         ChiSquare = Chisq, Df = 1, p = pchisq(Chisq, 1, lower.tail = FALSE),
#'         test = "Breusch-Pagan test of Non-constant Variance")
#'     class(result) <- "chisqTest"
#'     result
#' }
