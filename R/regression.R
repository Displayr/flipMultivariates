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
#'   \code{"Quasi-Poisson"}, \code{"Binary Logit"}, \code{"NBD"}, and
#'   \code{"Ordered Logit"}
#' @param robust.se Computes standard errors that are robust to violations of
#'   the assumption of constant variance, using the HC1 (degrees of freedom)
#'   modification of White's (1980) estimator (Long and Ervin, 2000).
#' @param r.output More detailed outputs.
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
Regression <- function(formula, data, subset = NULL,
                             weights = NULL,
                             missing = "Exclude cases with missing data",
                             type = "Linear",
                             robust.se = FALSE,
                             r.output = TRUE, ...)
{
    cl <- match.call()
    outcome.name <- outcomeName(formula)
    outcome.variable <- data[[outcome.name]]
    if (type == "Binary Logit")
        data <- CreatingBinaryDependentVariableIfNecessary(formula, data)
    else if (type == "Ordered Logit" & !is.factor(outcome.variable))
        data[, outcome.name] <- ordered(outcome.variable)
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
        result <- linearRegressionFromCorrelations(formula, data, subset,
                             weights, outcome.variable, outcome.name, ...)
    }
    else
    {
        processed.data <- EstimationData(formula, data, subset, weights, missing)
        unfiltered.weights <- processed.data$unfiltered.weights
        estimation.data <- processed.data$estimation.data
        if (nrow(estimation.data) < ncol(estimation.data) + 1)
            stop(warningSampleSizeTooSmall())
        post.missing.data.estimation.sample <- processed.data$post.missing.data.estimation.sample
        weights <- processed.data$weights
        subset <-  processed.data$subset
        if (is.null(weights))
        {
            if (type == "Linear")
                result <- lm(formula, estimation.data)
            else if (type == "Poisson" | type == "Quasi-Poisson" | type == "Binary Logit")
                result <- glm(formula, estimation.data, family = switch(type,
                        "Poisson" = poisson,
                        "Quasi-Poisson" = quasipoisson,
                        "Binary Logit" = "binomial"))
            else if (type == "Ordered Logit")
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
            else if (type == "Ordered Logit")
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
                                 "Binary Logit" = survey::svyglm(formula, weightedSurveyDesign(estimation.data, weights), family = quasibinomial()),
                                 "Poisson" = survey::svyglm(formula, weightedSurveyDesign(estimation.data, weights), family = poisson()),
                                 "Quasi-Poisson" = survey::svyglm(formula, weightedSurveyDesign(estimation.data, weights), family = quasipoisson()))
            }
        }
        if (missing == "Imputation (replace missing values with estimates)")
            data <- processed.data$data
        result$flip.predicted.values <- ifThen(any(class(result) == "glm"),
            suppressWarnings(predict.glm(result, newdata = data, na.action = na.pass)),
            predict(result, newdata = data, na.action = na.pass))
        fitted.values <- fitted(result)
        result$flip.fitted.values <- ifThen(is.matrix(fitted.values),
            fitted.values[match(row.names, rownames(fitted.values)), ],
            fitted.values[match(row.names, names(fitted.values))])
        result$flip.subset <- row.names %in% rownames(estimation.data)
        result$sample.description <- processed.data$description
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
    result$r.output <- r.output
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
    estimation.data <- ifThen(weighted,
                              flipU::AdjustDataToReflectWeights(subset.data, weights),
                              subset.data)
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
    result$flip.subset <- !is.na(data[outcome.index]) & !is.na(fitted) & (rownames(data) %in% rownames(estimation.data))
    # Sample description.
    pairwise.n <- crossprod(!is.na(estimation.data))
    n.total <- nrow(data)
    weight.label <- ifThen(weighted <- !is.null(weights), attr(weights, "label"), "")
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
    result
}

#' @export
print.Regression <- function(x, p.cutoff = 0.05, digits = max(3L, getOption("digits") - 3L), ...)
{
    Regression.object <- x
    Regression.summary <- Regression.object$summary

    if (Regression.object$robust.se)
        Regression.summary$coefficients <- Regression.object$robust.coefficients
    else
    {   #Testing to see if the variance is non-constant.
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
    if (length(unique(outcome.variable)) == 2 && Regression.object$type == "Linear")
        warning(paste0("The outcome variable contains only two unique values. A BinaryLogit may be
                       more appropriate."))
    else
    {
        if (Regression.object$type == "Linear" & isCount(outcome.variable))
            warning(paste0("The outcome variable appears to contain count data (i.e., the values are non-negative integers). A limited dependent variable regression may be more appropriate (e.g., Quasi-Poisson Regression, Ordered Logit)."))
    }
    # When r.output is false, print a nicely-formatted table
    if (!Regression.object$r.output)
    {
        sample.description <- Regression.object$sample.description
        sample.description <- sub("[\\.]$", "", sample.description)
        sample.description <- gsub("[\\.][[:space:]+]", "; ", sample.description)
        caption <- c(paste0(Regression.object$type, " Regression; ", sample.description))
        # Work out which R^2 / Objective function to add
        if (!is.null(Regression.object$lm.cov))
            extra.caption <- partial.data.r2.message
        else if (Regression.object$type == "Linear")
            extra.caption <- paste0("Multiple R-squared: ", formatC(Regression.summary$r.squared, digits = digits))
        else if (Regression.object$type == "Ordered")
            extra.caption <- paste0("AIC: ", formatC(Regression.summary$deviance, digits = digits))
        else if (!is.null(Regression.summary$aic))
            extra.caption <- paste0("AIC: ", formatC(Regression.summary$aic, digits = digits))

        if (!is.null(extra.caption))
            caption <- paste0(caption, "; ", extra.caption)

        dt <- createRegressionDataTable(Regression.object, p.cutoff = p.cutoff, caption = caption)
        print(dt)
    }
    else
    {
        print(Regression.summary, ...)
        if (!is.null(Regression.object$lm.cov))
            cat(paste0("Partial-data Multiple R-squared ", FormatAsReal(Regression.object$lm.cov$R2, 4), " (the R-squared and F above are based only on complete cases).\n"))
        cat(Regression.object$sample.description)
        if (Regression.object$robust.se)
            cat("Heteroscedastic-robust standard errors.")
    }


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

    # Check for p values in last column
    num.col <- ncol(coefs)
    has.p.values <- substr(colnames(coefs)[num.col], 1, 3) == "Pr("
    any.significant <- has.p.values && length(which(coefs[, num.col] < p.cutoff)) > 0
    if (has.p.values && any.significant)
    {
        caption <- paste0(caption, "; Results highlighted when p < " , p.cutoff)
    }

    dt <- flipU::DataTableWithRItemFormat(pretty.coefs, caption = caption, header.alignments = rep("right", ncol(pretty.coefs)))
    test.info <- .findTestInCoefficientTable(pretty.coefs)

    # Highlight significant coefficients
    if (has.p.values && any.significant)
    {
        if (test.info$test.type == "t")
        {
            t.val <- qt(p.cutoff / 2, df = x$df.residual)
            dt <- flipU::AddSignificanceHighlightingToDataTable(dt, columns.to.color = "Estimate",
                                                                column.to.check = "t value",#test.info$test.column,
                                                                red.value = t.val, blue.value = -1L * t.val)
        } else if (test.info$test.type == "z") {
            z.val <- qnorm(p.cutoff / 2)
            dt <- flipU::AddSignificanceHighlightingToDataTable(dt, columns.to.color = 1,
                                                                column.to.check = test.info$test.column,
                                                                red.value = z.val, blue.value = -1L * z.val)
        }
    }
    return(dt)
}

