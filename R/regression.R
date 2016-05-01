#' \code{Regression} Generalized Regression.
#'
#' @param formula An object of class \code{\link{formula}} (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#'   The details of type specification are given under \sQuote{Details}.
#' @param data A \code{\link{data.frame}}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param missing How missing data is to be treated in the regression. Options
#'   are: \code{"Error if missing data"}, \code{"Exclude cases with missing
#'   data"}, \code{"Use partial data (pairwise correlations)"},and
#'   \code{"Imputation (replace missing values with estimates)"}.
#' @param type Defaults to \code{"linear"}. Other types are: \code{"Poisson"},
#'   \code{"Quasi-Poisson"}, \code{"Binary Logit"}, \code{"NBD"},
#'   \code{"Ordered Logit"}, and \code{"Multinomial Logit"}
#' @param robust.se Computes standard errors that are robust to violations of
#'   the assumption of constant variance, using the HC1 (degrees of freedom)
#'   modification of White's (1980) estimator (Long and Ervin, 2000).
#' @param detail More detailed outputs.
#' @param method The method to be used; for fitting. This will only do something if
#' method = "model.frame", which returns the model frame.
#' @param ... Additional argments to be past to  \code{\link{lm}} or, if the
#'   data is weighted,  \code{\link[survey]{svyglm}}.
#'
#' @details "Imputation (replace missing values with estimates)". All selected
#'   outcome and predictor variables are included in the imputation, excluding
#'   cases that are excluded via subset or have invalid weights, but including
#'   cases with missing values of the outcome variable.
#'   Then, cases with missing values in the outcome variable are excluded from
#'   the analysis (von Hippel 2007). Where "Use partial data (pairwise
#'   correlations)" is used, if the data is weighted, a synthetic data file is
#'   created by sampling with replacement in proportion to the weights,where the
#'   sample size is the sum of the weights. See \code{\link{SingleImputation}}.
#' @references von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#'   Improved Strategy for Analyzing Multiply Imputed Data." Sociological
#'   Methodology 37:83-117. White, H. (1980), A heteroskedastic-consistent
#'   covariance matrix estimator and a direct test of heteroskedasticity.
#'   Econometrica, 48, 817-838. Long, J. S. and Ervin, L. H. (2000). Using
#'   heteroscedasticity consistent standard errors in the linear regression
#'   model. The American Statistician, 54( 3): 217-224.
#' @export
Regression <- function(formula,
                       data = NULL,
                       subset = NULL,
                       weights = NULL,
                       missing = "Exclude cases with missing data",
                       type = "Linear",
                       robust.se = FALSE,
                       method = "default",
                       detail = TRUE, ...)
{
    cl <- match.call()
    .formula <- formula # Hack to work past scoping issues in car package: https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf.


    # expr <- substitute(subset)
    # pf <- parent.frame()
    # subset <- eval(expr, data, pf)
    #subset <- getVector(formula, data, subset)

    subset <- eval(substitute(subset), data, parent.frame())
    weights <- eval(substitute(weights), data, parent.frame())
    data <- getData(.formula, data)


    # # Extracting and evaluating the weights and subsets.
    # mf <- match.call(expand.dots = FALSE)
    # m <- match(c("formula", "data","subset", "weights"), names(mf), 0)
    # has.subset <- m[3] > 0
    # if (has.subset)
    #     subset.name <- mf[[m[1]]]
    # na.action <- NULL
    # mf <- mf[m[m > 0]]
    # mf$drop.unused.levels <- TRUE
    # mf$na.action <- na.pass#substitute(na.action)
    # mf[[1]] <- quote(stats::model.frame)
    # data <- eval.parent(mf)
    # print(names(data))
    #
    #
    #
    #
    #
    # stop("dog")
    #
    #

    # data <- getData(.formula, data)
    # if (!is.null(subset))
    # {
    #     subset <- try(eval(parse(text=subset)))
    #     if (class(z) == "try-error" & !is.null(data))
    #     {
    #         subset <- with(data, eval(parse(text=subset)))
    #     }
    # }
    # if (!is.null(weights))
    #     eval(parse(text=weights))
    #
    #

    # weights <- getVector(.formula, data, weights)
    # subset <- getVector(.formula, data, subset)
    #printDetails(subset)
    #printDetails(weights)

    # Extracting and evaluating the weights and subsets.
    # mf <- match.call(expand.dots = FALSE)
    # m <- match(c("formula", "data","subset", "weights"), names(mf), 0)
    # has.subet <- m[3] > 0
    # if (has.subet)
    #     subset.name <- mf[[m[1]]]
    # na.action <- NULL
    # mf <- mf[m[m > 0]]
    # mf$drop.unused.levels <- TRUE
    # mf$na.action <- na.pass#substitute(na.action)
    # mf[[1]] <- quote(stats::model.frame)
    # data <- eval.parent(mf)
    # print(names(data))
    # stop("dog")
    #
    # #
    #     #w <- model.weights(mf, na.action = na.pass)
    #     #data <- model.frame(mf, drop.unused.levels = TRUE, na.action = NULL, )
    #     #print(mf)
    #     #mf$drop.unused.levels <- TRUE
    #     #mf$na.action <- list(NULL)
    #     #mf[[1]] <- quote(stats::model.frame)
    #     #print(str(mf))
    #    data <- eval.parent(mf)



    #     mf <- match.call(expand.dots = FALSE)
    #     m <- match(c("formula", "data", "subset", "weights", "na.action", "offset"), names(mf), 0)
    #     # Removing our own special parameters from the call, so that they do not stuff up other package's functions.
    #     terms.to.remove <- match(c("type", "detail", "robust.se", "missing"), names(mf), 0)
    #     terms.to.remove <- terms.to.remove[terms.to.remove > 0]
    #     if (length(terms.to.remove))
    #         mf <- mf[-terms.to.remove]
    #     has.subet <- m[3] > 0
    #     if (has.subet)
    #         subset.name <- mf[[m[3]]]
    #     na.action <- NULL
    #     mf$drop.unused.levels <- TRUE
    #     mf$na.action <- na.pass#substitute(na.action)
    #
    #     mf[[1]] <- quote(stats::get_all_vars) # was as.name("model.frame"), but
    # #    mf[[1]] <- quote(stats::model.frame) # was as.name("model.frame"), but
    #                           ##    need "stats:: ..." for non-standard evaluation
    #     data <- eval.parent(mf)
    #
    #     #w <- model.weights(mf, na.action = na.pass)
    #     #data <- model.frame(mf, drop.unused.levels = TRUE, na.action = NULL, )
    #     #print(mf)
    #     #mf$drop.unused.levels <- TRUE
    #     #mf$na.action <- list(NULL)
    #     #mf[[1]] <- quote(stats::model.frame)
    #     #print(str(mf))
    #    data <- eval.parent(mf)
    if (method == "model.frame")
         return(data)
    mt <- attr(data, "terms")
    #weights <- model.weights(data)
    # if (has.subset)
    #     subset <- data$"(subset)"

    #     print(m)
    # print(mf[[4]])
    # print(names(mf)[[4]])
    # mf <- mf[c(1, m)]
    # stop("dog")
    # subset <- model.subsets(data)
    outcome.name <- outcomeName(.formula)
    outcome.variable <- data[[outcome.name]]
    if (!is.null(weights) & length(weights) != nrow(data))
        stop("'weights' and 'data' are required to have the same number of observations. They do not.")
    if (!is.null(subset) & length(subset) > 1 & length(subset) != nrow(data))
        stop("'subset' and 'data' are required to have the same number of observations. They do not.")

    if (type == "Binary Logit")
    {
        data <- CreatingBinaryDependentVariableIfNecessary(.formula, data)
        outcome.variable <- data[[outcome.name]]
    }
    else if (type == "Ordered Logit")
        data[, outcome.name] <- ordered(outcome.variable)
    else if (type == "Multinomial Logit")
    {
        data[, outcome.name] <- factor(outcome.variable)
        if (!detail)
        {
            warning("The 'Detailed output' option has not been selected. Only detailed output is available
                    with Multinomial Logit.")
            detail = TRUE
        }
    }
    else if (is.factor(outcome.variable))
    {
        WarningFactorToNumeric()
        data[, outcome.name] <- outcome.variable <- unclass(outcome.variable)
    }
    row.names <- rownames(data)
    if (missing == "Use partial data (pairwise correlations)")
    {
        subset <- CleanSubset(subset, nrow(data))
        unfiltered.weights <- weights <- CleanWeights(weights)
        if (type != "Linear")
            stop(paste0("'Use partial data (pairwise)' can only be used with 'type' of 'Linear'."))
        if (robust.se)
            stop(paste0("Robust standard errors cannot be computed with 'missing' set to ", missing, "."))
        result <- list(original = linearRegressionFromCorrelations(.formula, data, subset,
            weights, outcome.variable, outcome.name, ...),
            call = cl)
        result$summary <- result$original$summary
        result$original$summary <- NULL
    }
    else
    {
        processed.data <- EstimationData(.formula, data, subset, weights, missing)
        unfiltered.weights <- processed.data$unfiltered.weights
        .estimation.data <- processed.data$estimation.data
        n <- nrow(.estimation.data)
        if (n < ncol(.estimation.data) + 1)
            stop(warningSampleSizeTooSmall())
        post.missing.data.estimation.sample <- processed.data$post.missing.data.estimation.sample
        weights <- processed.data$weights
        subset <-  processed.data$subset
        if (is.null(weights))
        {
            if (type == "Linear")
                original <- lm(.formula, .estimation.data, model = TRUE)
            else if (type == "Poisson" | type == "Quasi-Poisson" | type == "Binary Logit")
                original <- glm(.formula, .estimation.data, family = switch(type,
                                                                            "Poisson" = poisson,
                                                                            "Quasi-Poisson" = quasipoisson,
                                                                            "Binary Logit" = binomial(link = "logit")))
            else if (type == "Ordered Logit")
                original <- MASS::polr(.formula, .estimation.data, Hess = TRUE, ...)
            else if (type == "Multinomial Logit")
                original <- nnet::multinom(.formula, .estimation.data, Hess = TRUE, trace = FALSE, maxit = 10000, ...)
            else if (type == "NBD")
                original <- MASS::glm.nb(.formula, .estimation.data)
            else
                stop("Unknown regression 'type'.")

            if (robust.se)
            {
                original$robust.coefficients <- lmtest::coeftest(original,
                        vcov = car::hccm(original, type = "hc1"))
                colnames(original$robust.coefficients)[2] <- "Robust SE"
            }
        }
        else
        {
            if (robust.se)
                warningRobustInappropriate()
            if (type == "Linear")
                original <- survey::svyglm(.formula, weightedSurveyDesign(.estimation.data, weights))
            else if (type == "Ordered Logit")
            {
                .estimation.data$weights <- weights
                original <- MASS::polr(.formula, .estimation.data, weights = weights, Hess = TRUE, ...)
            }
            else if (type == "Multinomial Logit")
            {
                .estimation.data$weights <- weights
                original <- nnet::multinom(.formula, .estimation.data, weights = weights, Hess = TRUE, trace = FALSE, maxit = 10000, ...)
            }
            else if (type == "NBD")
            {
                .estimation.data$weights <- weights
                original <- MASS::glm.nb(.formula, .estimation.data, weights = weights, ...)
            }
            else
            {
                wgt.svy.des <- weightedSurveyDesign(.estimation.data, weights)
                original <- switch(type,
                                   "Binary Logit" = survey::svyglm(.formula, wgt.svy.des, family = quasibinomial()),
                                   "Poisson" = survey::svyglm(.formula, wgt.svy.des, family = poisson()),
                                   "Quasi-Poisson" = survey::svyglm(.formula, wgt.svy.des, family = quasipoisson()))
                assign("wgt.svy.des", wgt.svy.des, envir=.GlobalEnv)
                original$aic <- AIC(original)[2]
                remove("wgt.svy.des", envir=.GlobalEnv)

            }
        }
        result <- list(original = original, call = cl)
        # if(is.null(result$aic))
        # {
        #     result$aic<- AIC(original)
        #     result$bic <- BIC(original)
        # }
        require(car)
        if (missing == "Imputation (replace missing values with estimates)")
            data <- processed.data$data
        # Predicted values and probabilities and fitted values
        # if (type %in% c("Ordered Logit", "Multinomial Logit") )
        #     result$predicted.probabilities <- suppressWarnings(predict(result$original, newdata = data, na.action = na.pass, type = "probs"))
        # else if (type != "Linear")
        #     result$predicted.probabilities <- suppressWarnings(predict(result$original, newdata = data, na.action = na.pass, type = "response"))
        # result$predicted.values <- ifThen(any(class(original) == "glm"),
        #       suppressWarnings(predict.glm(result$original, newdata = data, na.action = na.pass)),
        #       predict(result$original, newdata = data, na.action = na.pass))
        # fitted.values <- fitted(result)
        # result$fitted.values <- ifThen(is.matrix(fitted.values),
        #        fitted.values[match(row.names, rownames(fitted.values)), ],
        #        fitted.values[match(row.names, names(fitted.values))])
        # if (isCount(type))
        #     result$predicted.values <- floor(exp(result$predicted.values))
        # else if (type == "Binary Logit")
        #     result$predicted.values <- factor(as.integer(result$predicted.values >= 0) + 1, labels = levels(outcome.variable))
        result$subset <- row.names %in% rownames(.estimation.data)
        result$sample.description <- processed.data$description
        result$n.predictors <- ncol(.estimation.data) - 1
        result$n.observations <- n
        result$estimation.data <- .estimation.data
        result$summary <- summary(result$original)
    }
    result$summary$call <- cl
    result$formula <- .formula
    # Inserting the coefficients from the partial data.
    result$model <- data #over-riding the data that is automatically saved (which has had missing values removed).
    result$robust.se <- robust.se
    class(result) <- "Regression"
    result$type = type
    result$weights <- unfiltered.weights
    result$detail <- detail
    result$outcome.name <- outcome.name
    result$missing <- missing
    result$terms <- mt
    result$coef <- result$original$coef
    # if (type != "Multinomial Logit")
    #     result$residuals <- unclassIfNecessary(outcome.variable) -
    #     unclassIfNecessary(result$predicted.values)#Note this occurs after summary, to avoid stuffing up summary, but before Breusch Pagan, for the same reason.
    if (robust.se)
        result$summary$coefficients <- result$original$robust.coefficients
    return(result)
    }

linearRegressionFromCorrelations <- function(formula, data, subset = NULL,
                                             weights = NULL, outcome.variable, outcome.name, ...)
{
    result <- lm(formula, data, ...)
    result$summary <- summary(result)
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
    subset.data <- if(is.null(subset)) data else subset(data, subset)
    n.subset <- nrow(subset.data)
    if (n.subset < length(predictors.index) + 1)
        stop(warningSampleSizeTooSmall())
    n.total <- nrow(data)
    weighted <- !is.null(weights)
    if (n.subset < n.total & weighted)
        weights <- subset(weights, subset)
    # Taking the print chart statement out of setCor.
    .pairwise.regression <- psych::setCor
    n <- length(body(.pairwise.regression))
    while(as.character(body(.pairwise.regression)[n]) != "setCor.diagram(set.cor, main = main)")
        n <- n - 1
    body(.pairwise.regression)[n] <- NULL
    .estimation.data <- if (weighted)
            flipU::AdjustDataToReflectWeights(subset.data, weights)
        else subset.data
    #     result$lm.cov <- lm.cov <- .pairwise.regression(outcome.index, predictors.index,
    #         data = estimation.data, std = FALSE)
    result$lm.cov <- lm.cov <- .pairwise.regression(outcome.index, predictors.index,
                                                    data = .estimation.data, std = TRUE)
    scaled.beta <- as.matrix(lm.cov$beta)
    sds.independent = apply(.estimation.data[, predictors.index], 2, sd, na.rm = TRUE)
    sd.dependent <- sd(.estimation.data[, outcome.index], na.rm = TRUE)
    beta <- scaled.beta / (sds.independent / sd.dependent)
    se <- lm.cov$se / (sds.independent / sd.dependent)
    partial.coefs <- cbind(beta, se, lm.cov$t, lm.cov$Probability)
    dimnames(partial.coefs) <- list(variable.names[predictors.index],
                                    c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    beta <- as.matrix(lm.cov$beta)
    fitted <- as.matrix(.estimation.data[, predictors.index]) %*% beta
    intercept <- mean(.estimation.data[, outcome.name], na.rm = TRUE) - mean(fitted, na.rm = TRUE)
    fitted <- as.matrix(data[, predictors.index]) %*% beta
    result$predicted.values <- result$fitted.values <- fitted + intercept
    partial.coefs <- partial.coefs[c(1,1:nrow(partial.coefs)),]
    partial.coefs[1,] <- c(intercept, NA, NA, NA)
    rownames(partial.coefs)[1] <- "(Intercept)"
    #result$partial.coefs <- partial.coefs
    result$coef <- partial.coefs[, 1]
    result$subset <- !is.na(data[outcome.index]) & !is.na(fitted) & (rownames(data) %in% rownames(.estimation.data))
    # Sample description.
    pairwise.n <- crossprod(!is.na(.estimation.data))
    n.total <- nrow(data)
    weight.label <- if(weighted <- !is.null(weights)) attr(weights, "label") else  ""
    rng <- range(pairwise.n[lower.tri(pairwise.n)])
    n.min <- rng[1]
    if (n.min == rng[2])
        description <- paste0("n = ", n.min,
                              " cases used in estimation\n")
    else
        description <- paste0("Pairwise correlations have been used to estimate this regression.\n",
                              "Sample sizes for the correlations range from ", n.min, " to ", rng[2], "")
    description <- baseDescription(description,
                                   n.total, attr(subset, "n.subset"), n.min, attr(subset, "label"), NULL, "")
    if (!is.null(weights))
        description <- paste0(description, " Data have been resampled with probabilities proportional to the weights(",
                              weight.label, ").\n")
    result$sample.description <- description
    result$n.predictors <- length(predictors.index)
    result$estimation.data <- .estimation.data
    result$summary$coefs <- partial.coefs
    result
}

#' @export
print.Regression <- function(x, p.cutoff = 0.05, digits = max(3L, getOption("digits") - 3L), ...)
{
    weighted <- !is.null(x$weights)
    # Cook's distance.
    if (!is.null(x$cooks.distance))
    {
        k <- x$n.predictors + 1
        n <- x$n.observations
        cutoff <- qf(0.5, k, n - k)
        max.d <- max(x$cooks.distance)
        if (any(max.d > cutoff))
            warning(paste0("Influential observations detected. The largest Cook's distance is ",
                           round(max.d,2), ". The threshhold is ", round(cutoff, 2), " (the median of the F(k = ",
                           k, ",  n - k  = ", n - k, ") distribution)."))

    }
    # Testing to see if there is multicollinearity.
    if (ncol(x$model) > 2 & x$type == "Linear" & x$missing != "Use partial data (pairwise correlations)")
    {
        vifs <- vif(x)
        if (!is.null(vifs))
        {
            max.vif <- max(vifs)
            if (max.vif >= 4)
            {
                pref <- if(x$type == "Linear") "" else "Generalized "
                nms <- rownames(x$summary$coefficients)[-1]
                VIFs <- paste0(nms,": ", round(vifs, 1), c(rep("; ", length(nms) - 1), ""), collapse = "")
                warning(paste0("The ",pref, "Variance Inflation Factor of the coefficients are: ", VIFs,". A value of 4 or more indicates the confidence interval for the coefficient is twice as wide as they would be for uncorrelated predictors. A value of 10 or more indicates high multicollinearity."))
            }
        }
    }
    #Testing to see if the variance is non-constant.
    partial <- x$missing == "Use partial data (pairwise correlations)"
    if (x$type == "Linear" & !partial & !weighted)
    {
        bp.test <- ncvTest(x)#BreuschPagan(x$original)
        # assign(".estimation.data", x$estimation.data, envir=.GlobalEnv)
        # assign(".formula", x$formula, envir=.GlobalEnv)
        # bp.test <- car::ncvTest(x$original)#BreuschPagan(x$original)
        # remove(".formula",envir=.GlobalEnv)
        # remove(".estimation.data",envir=.GlobalEnv)
        if (bp.test$p <= 0.05)
        {
            suggest <- if(is.null(x$partial.coefs)) " Or, consider using Robust Standard Errors." else ""
            warning(paste0("A Breusch Pagan Test for non-constant variance has been failed (p = ",
                           FormatAsPValue(bp.test$p), "). A plot of the residuals versus the fitted values of the outcome variable may be useful (Insert > Advanced > Regression > Plots > Residuals vs Fitted). A transformation of the outcome or predictor variables may solve this problem.",
                           suggest, "\n"))
            outcome.variable <- outcomeVariableFromModel(x)
        }
    }
    outcome.variable <- outcomeVariableFromModel(x)
    if (length(unique(outcome.variable)) == 2 && x$type == "Linear")
        warning(paste0("The outcome variable contains only two unique values. A Binary Logit may be
                       more appropriate."))
    else
    {
        if (x$type == "Linear" & isCount(outcome.variable))
            warning(paste0("The outcome variable appears to contain count data (i.e., the values are non-negative integers). A limited dependent variable regression may be more appropriate (e.g., Quasi-Poisson Regression, Ordered Logit)."))
    }
    # Creating a nicely formatted text description of the model.
    require(formattable)
    aic <- if(partial) NA else AIC(x)
    rho.2 <- if(partial | x$type == "Linear") NA else McFaddensRhoSquared(x)
    caption <- x$sample.description
    if (!partial)
    caption <- paste0(caption,
        " R-Squared: ", round(GoodnessOfFit(x)$value, 4),
        "; Correct predictions: ", percent(Accuracy(x)),
        if (is.null(rho.2)) "" else paste0("; McFadden's rho-squared: ", round(rho.2, 4)),
        if (is.na(aic)) "" else paste0("; AIC: ",comma(aic), "."))

    # print(caption)
    # caption <- sub("[\\.]$", "", caption)
    # print(caption)
    # caption <- gsub("[\\.][[:space:]+]", "; ", caption)
    # print(caption)
    # stop()
    # Work out which R^2 / Objective function to add
    # caption <- paste0(caption, "; R-Squared: ", formatC(GoodnessOfFit(x)$value), digits = digits)
    #
    #     extra.caption <- paste0("R-Squared: ", formatC(GoodnessOfFit(x)$value), digits = digits)
    # extra.caption <- paste0(extra.caption, "; Correct predictions: ", formatC(Accuracy(x)*100, digits = digits),"%")
    # if (x$type == "Ordered Logit" | x$type == "Multinomial Logit")
    #     aic <- paste0(extra.caption, "; AIC: ", formatC(x$summary$deviance, digits = digits))
    # else if (!is.null(x$summary$aic))
    #     aic <- paste0(extra.caption, "; AIC: ", formatC(x$summary$aic, digits = digits))
    # if (!is.null(extra.caption) & nchar(extra.caption) > 0)
    #     caption <- paste0(caption, "; ", extra.caption)
    # When detail is false, print a nicely-formatted table
    if (x$detail)
    {
        cat(paste0(x$type, " regression\n"))
        print(x$summary, ...)
        if (!is.null(x$original$lm.cov))
            cat(paste0("Partial-data R-squared ", FormatAsReal(x$original$lm.cov$R2, 4), " (the R-squared and F above are based only on complete cases).\n"))
        cat(caption)
        if (x$robust.se)
            cat("Heteroscedastic-robust standard errors.")
    }
    else
    {
        caption <- c(paste0(x$type, " Regression; ", caption))
        dt <- createRegressionDataTable(x, p.cutoff = p.cutoff, caption = caption)
        print(dt)
    }
}

# #' @export
# resid.Regression <- function(object, ...)
# {
#     object$flip.residuals
# }
notValidForPartial <- function(object, method)
{
    ms <- "Use partial data (pairwise correlations)"
    if (object$missing == ms)
        stop(paste0("'", method, "' not available when 'missing' = ",ms, "'." ))
}

#' @export
residuals.Regression <- function(object, type = "raw", ...)
{
    notValidForPartial(object, "residuals")
    if (type == "raw" & object$type %in% c("Linear", "Multinomial Logit", "Binary Logit"))
        return(unclassIfNecessary(Observed(object)) - unclassIfNecessary(predict(object)))
    resids <- residuals(object$original, ...)
    fillInMissingRowNames(rownames(object$model), resids)
}

#' @export
probabilities <- function(x, ...)
{
    notValidForPartial(object, "probabilities")
    if (type == "Linear")
        stop("'probabilities' is not applicable to linear regression models.")
    if (x$type %in% c("Ordered Logit", "Multinomial Logit"))
        return(suppressWarnings(predict(x$original, newdata = x$model, na.action = na.pass, type = "probs")))
    if (x$type == "Binary Logit")
        return(suppressWarnings(predict(x$original, newdata = x$model, na.action = na.pass, type = "response")))[, 2]
    xs <- 0:max(Observed(x), na.rm = TRUE)
    if (x$type == "Poisson"){
        log.lambdas <- suppressWarnings(predict(x$original, newdata = x$model, na.action = na.pass, type = "link"))
        lambdas <- exp(log.lambdas)
        return(computePoissonEsqueProbabilities(xs, lambdas, dpois))
    }
    stop(paste0("Probabilities are not computed for models of type '", x$type, "."))
}

computePoissonEsqueProbabilities <- function(xs, lambdas, density)
{
    n <- length(lambdas)
    k <- length(xs)
    result <- matrix(density(rep(xs[-k], rep(n, k - 1)), lambdas), nrow = n)
    result <- cbind(result, 1 - apply(result, 1, sum))
    dimnames(result) <- list(rownames(lambdas), c(xs[-k], paste(">=", xs[k])))
    result
}

#' @export
predict.Regression <- function(object, newdata = object$model, na.action = na.pass, ...)
{
    notValidForPartial(object, "predict")
    predicted <- if (any(class(object$original) == "glm"))
            suppressWarnings(predict.glm(object$original, newdata = newdata, na.action = na.action, type = "response"))
        else
            predict(object$original, newdata = newdata, na.action = na.action)
    if (isCount(object$type))
         return(floor(predicted))
    if (object$type == "Binary Logit")
        return(factor(as.integer(predicted >= 0.5) + 1, labels = levels(Observed(object))))
    predicted
}

#' @export
fitted.Regression <- function(object, ...)
{
    notValidForPartial(object, "fitted")
    fitted.values <- fitted(object$original)
    fillInMissingRowNames(rownames(object$model), fitted.values)
}

fillInMissingRowNames <- function(row.names, variable)
{
    if(is.matrix(variable))
        return(variable[match(row.names, rownames(variable)), ])
    variable[match(row.names, names(variable))]
}

#' @export
fitted.values.Regression <- function(object, ...)
{
    fitted(object, ...)
}

#' \code{observed} Observed values used in fitting a model with an outcome variable.
#'
#' @export
Observed <- function(x) UseMethod("Observed", x)

#' #' @export
#' Observed.default <- function(x)
#' {
#'     fitted(x)
#' }

##' #@method observed Regression
#' @export
Observed.Regression <- function(x)
{
    x$model[[x$outcome.name]]
}



# Create an HTML widget data table (package DT) from the coefficients
# table in a regression summary.
createRegressionDataTable <- function(x, p.cutoff, caption = NULL)
{
    # Given a table of coefficients from a summary of a regression
    # figure out which test has been used and which column the test
    # statistics are found in.
    .findTestInCoefficientTable <- function(coefficient.table) {
        col.names <- colnames(coefficient.table)
        t.col <- which(col.names == "t value")
        z.col <- which(col.names == "z value")
        if (length(t.col) == 0 && length(z.col) == 0)
        {
            test.type <- "none"
            test.column <- NULL

        } else if (length(t.col) > 0 && length(z.col) > 0 || length(t.col) > 1 || length(z.col) > 1) {
            stop("Ambiguous statistical testing information in coefficients table.")
        } else if (length(t.col) > 0) {
            test.type <- "t"
            test.column <- t.col
        } else {
            test.type <- "z"
            test.column <- z.col
        }

        return(list(test.type = test.type, test.column = test.column))
    }

    # Create a formatted array of regression coefficient information that can be passed to an HTMLwidget
    # DataTable.
    # This function is a copy of \code{\link{printCoefmat}} which returns the formatted output
    # instead of printing it, and which does not do the significance testing stars by default.
    .formatRegressionCoefficientMatrix <- function (x, digits = max(3L, getOption("digits") - 2L), signif.stars = FALSE,
                                                    signif.legend = signif.stars, dig.tst = max(1L, min(5L, digits -
                                                                                                            1L)), cs.ind = 1:k, tst.ind = k + 1, zap.ind = integer(),
                                                    P.values = NULL, has.Pvalue = nc >= 4 && substr(colnames(x)[nc],
                                                                                                    1, 3) == "Pr(", eps.Pvalue = .Machine$double.eps, na.print = "NA",
                                                    ...)
    {
        if (is.null(d <- dim(x)) || length(d) != 2L)
            stop("'x' must be coefficient matrix/data frame")
        nc <- d[2L]
        if (is.null(P.values)) {
            scp <- getOption("show.coef.Pvalues")
            if (!is.logical(scp) || is.na(scp)) {
                warning("option \"show.coef.Pvalues\" is invalid: assuming TRUE")
                scp <- TRUE
            }
            P.values <- has.Pvalue && scp
        }
        else if (P.values && !has.Pvalue)
            stop("'P.values' is TRUE, but 'has.Pvalue' is not")
        if (has.Pvalue && !P.values) {
            d <- dim(xm <- data.matrix(x[, -nc, drop = FALSE]))
            nc <- nc - 1
            has.Pvalue <- FALSE
        }
        else xm <- data.matrix(x)
        k <- nc - has.Pvalue - (if (missing(tst.ind))
            1
            else length(tst.ind))
        if (!missing(cs.ind) && length(cs.ind) > k)
            stop("wrong k / cs.ind")
        Cf <- array("", dim = d, dimnames = dimnames(xm))
        ok <- !(ina <- is.na(xm))
        for (i in zap.ind) xm[, i] <- zapsmall(xm[, i], digits)
        if (length(cs.ind)) {
            acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
            if (any(ia <- is.finite(acs))) {
                digmin <- 1 + if (length(acs <- acs[ia & acs != 0]))
                    floor(log10(range(acs[acs != 0], finite = TRUE)))
                else 0
                Cf[, cs.ind] <- format(round(coef.se, max(1L, digits -
                                                              digmin)), digits = digits)
            }
        }
        if (length(tst.ind))
            Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst),
                                    digits = digits)
        if (any(r.ind <- !((1L:nc) %in% c(cs.ind, tst.ind, if (has.Pvalue) nc))))
            for (i in which(r.ind)) Cf[, i] <- format(xm[, i], digits = digits)
        ok[, tst.ind] <- FALSE
        okP <- if (has.Pvalue)
            ok[, -nc]
        else ok
        x1 <- Cf[okP]
        dec <- getOption("OutDec")
        if (dec != ".")
            x1 <- chartr(dec, ".", x1)
        x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
        if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
            Cf[okP][not.both.0] <- format(xm[okP][not.both.0], digits = max(1L,
                                                                            digits - 1L))
        }
        if (any(ina))
            Cf[ina] <- na.print
        if (P.values) {
            if (!is.logical(signif.stars) || is.na(signif.stars)) {
                warning("option \"show.signif.stars\" is invalid: assuming TRUE")
                signif.stars <- TRUE
            }
            if (any(okP <- ok[, nc])) {
                pv <- as.vector(xm[, nc])
                Cf[okP, nc] <- format.pval(pv[okP], digits = dig.tst,
                                           eps = eps.Pvalue)
                signif.stars <- signif.stars && any(pv[okP] < 0.1)
                if (signif.stars) {
                    Signif <- symnum(pv, corr = FALSE, na = FALSE,
                                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                                     symbols = c("***", "**", "*", ".", " "))
                    Cf <- cbind(Cf, format(Signif))
                }
            }
            else signif.stars <- FALSE
        }
        else signif.stars <- FALSE
        if (signif.stars && signif.legend) {
            if ((w <- getOption("width")) < nchar(sleg <- attr(Signif,
                                                               "legend")))
                sleg <- strwrap(sleg, width = w - 2, prefix = "  ")
            cat("---\nSignif. codes:  ", sleg, sep = "", fill = w +
                    4 + max(nchar(sleg, "bytes") - nchar(sleg)))
        }
        return(as.data.frame(Cf))
    }


    # Make a pretty table with a caption
    coefs <- x$summary$coefficients
    pretty.coefs <- .formatRegressionCoefficientMatrix(coefs)
    pretty.coefs <- as.data.frame(pretty.coefs, stringsAsFactors = FALSE)

    caption <- paste0(caption, "; Results highlighted when p < " , p.cutoff)


    dt <- flipU::DataTableWithRItemFormat(pretty.coefs,
                                          caption = caption,
                                          header.alignments = rep("right", ncol(pretty.coefs)),
                                          page.length = nrow(pretty.coefs),
                                          allow.paging = FALSE,
                                          show.info = FALSE)
    test.info <- .findTestInCoefficientTable(pretty.coefs)

    # Highlight significant coefficients

    if (test.info$test.type == "t")
    {
        t.val <- qt(p.cutoff / 2, df = x$original$df.residual)
        dt <- flipU::AddSignificanceHighlightingToDataTable(dt, columns.to.color = 1,
                                                            column.to.check = "t value",#test.info$test.column,
                                                            red.value = t.val, blue.value = -1L * t.val)
    } else if (test.info$test.type == "z") {
        z.val <- qnorm(p.cutoff / 2)
        dt <- flipU::AddSignificanceHighlightingToDataTable(dt, columns.to.color = 1,
                                                            column.to.check = test.info$test.column,
                                                            red.value = z.val, blue.value = -1L * z.val)
    }

    return(dt)
}



