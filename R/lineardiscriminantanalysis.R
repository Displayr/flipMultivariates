#' \code{LDA} Linear Discriminant Analysis.
#'
#' @param formula A formula of the form \code{groups ~ x1 + x2 + ...}
#' That is, the response is the grouping factor and the right hand side
#' specifies the (non-factor) discriminators, and any transformations, interactions,
#' or other non-additive operators will be ignored.
#' transformations nor
#' @param data A \code{\link{data.frame}} from which variables specified
#' in formula are preferentially to be taken.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param missing How missing data is to be treated in the regression. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"},
#'   \code{"Imputation (replace missing values with estimates)"}.
#' @param prior The assumed probability of each value of y occurring in the
#'   population.  By default this is set to "observed" and the value is computed
#'   based on the observed data.  If set to "equal" the prior will be set to
#'   be equal for each group (this is the default in SPSS).  Alternatively, a
#'   vector of probabilities can be provided.
#' @param output One of \code{"Means"}, \code{"Confusion matrix"}, or \code{"Detail"}.
#' @param variance The method used to estimate the variance; either \code{\link{"moment"}} for
#' the method of moments or \code{\link{"mle"}} for maximum likelihood estimaion.
#' @param seed The random number seed used in imputation.
#' @param statistical.assumptions A Statistical Assumptions object.
#' @param auxiliary.data A \code{\link{data.frame}} containing additional variables
#'  to be used in imputation (if required). While adding more variables will improve
#'  the quality of the imputation, it will dramatically slow down the time to estimate.
#'  Factors and Character variables with a large number of categories should not be included,
#'  as they will both slow down the data and are unlikely to be useful
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param ... Additional argments to be past to  \code{\link{LDA.formula}}.
#' @details "Imputation (replace missing values with estimates): All selected
#'   outcome and predictor variables are included in the imputation, along with
#'   all \code{auxiliary.data}, excluding cases that are excluded via subset or
#'    have invalid weights, but including
#'   cases with missing values of the outcome variable.
#'   Then, cases with missing values in the outcome variable are excluded from
#'   the analysis (von Hippel 2007). See \code{\link[flipImputation]{Imputation}}.
#' @references von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#'   Improved Strategy for Analyzing Multiply Imputed Data." Sociological
#'   Methodology 37:83-117. White, H. (1980), A heteroskedastic-consistent
#'   covariance matrix estimator and a direct test of heteroskedasticity.
#'   Econometrica, 48, 817-838. Long, J. S. and Ervin, L. H. (2000). Using
#'   heteroscedasticity consistent standard errors in the linear regression
#'   model. The American Statistician, 54(3): 217-224.
#' @importFrom flipData GetData CleanSubset CleanWeights EstimationData DataFormula
#' @importFrom flipFormat GetLabels
#' @importFrom flipData CalibrateWeight
#' @importFrom flipTransformations CreatingFactorDependentVariableIfNecessary
#' @importFrom flipU OutcomeName
#' @importFrom flipRegression ConfusionMatrix
#' @export
LDA <- function(formula,
                data = NULL,
                subset = NULL,
                weights = NULL,
                prior = "Equal",
                missing = "Exclude cases with missing data",
                output = "Means",
                variance = "moment",
                seed = 12321,
                statistical.assumptions,
                auxiliary.data = NULL,
                show.labels = FALSE,
                ...)
{
    ####################################################################
    ##### Reading in the data and doing some basic tidying        ######
    ####################################################################
    cl <- match.call()
    input.formula <- formula # To work past scoping issues in car package: https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf.
    subset.description <- if (is.null(substitute(subset))) NULL else deparse(substitute(subset))
    subset <- eval(substitute(subset), data, parent.frame())
    if(!missing(statistical.assumptions))
        stop("'statistical.assumptions' objects are not yet supported.")
    if (!is.null(subset.description))
        attr(subset, "description") <- subset.description
    weights <- eval(substitute(weights), data, parent.frame())
    data <- GetData(input.formula, data, auxiliary.data)
    row.names <- rownames(data)
    outcome.name <- OutcomeName(input.formula)
    outcome.label <- if(show.labels) attr(data[, outcome.name], "label") else outcome.name
    data <- CreatingFactorDependentVariableIfNecessary(formula, data)
    outcome.variable <- data[[outcome.name]]
    if (!is.null(weights) & length(weights) != nrow(data))
        stop("'weights' and 'data' are required to have the same number of observations. They do not.")
    if (!is.null(subset) & length(subset) > 1 & length(subset) != nrow(data))
        stop("'subset' and 'data' are required to have the same number of observations. They do not.")
    # Treatment of missing values.
    processed.data <- EstimationData(input.formula, data, subset, weights, missing, m = m, seed = seed)
    unfiltered.weights <- processed.data$unfiltered.weights
    .estimation.data <- processed.data$estimation.data
    n <- nrow(.estimation.data)
    if (n < ncol(.estimation.data) + 1)
        stop("The sample size is too small for it to be possible to conduct the analysis.")
    post.missing.data.estimation.sample <- processed.data$post.missing.data.estimation.sample
    .weights <- processed.data$weights
    subset <-  processed.data$subset
    .formula <- DataFormula(input.formula)
    .estimation.data <- processed.data$estimation.data
    ####################################################################
    ##### Fitting the model. Ideally, this should be a call to     #####
    ##### another function, with the output of that function       #####
    ##### called 'original'.                                       #####
    ####################################################################
    n.levels <- nlevels(outcome.variable)
    if (is.character(prior))
    {
        outcome.variable <- factor(factor(outcome.variable))
        prior <- switch(prior,
            "Observed" = prop.table(table(outcome.variable)),
            "Equal" = rep(1 / n.levels, n.levels))
    }
    error <- paste0("The 'prior' must be one of: (1) 'Equal', ",
                    "(2) 'Observed', ",
                    "(3) or a vector of length ", n.levels, " containing values greater than 0 and less than 1 which sum to 1.")
    if (is.null(prior))
        stop(error)
    else if (!is.numeric(prior))
        stop(error)
    else if (length(prior) != n.levels)
        stop(error)
    else if (sum(prior) != 1 | min(prior) <= 0 | max(prior) >= 1)
        stop(error)
    x <- .estimation.data[, -match(outcome.name, names(.estimation.data))]
    result <- list(call = cl,
                   original = LDA.fit(x,
                                      grouping = .estimation.data[, outcome.name],
                                      prior = prior,
                                      method = variance,
                                      weights = .weights))
    ####################################################################
    ##### Saving results, parameters, and tidying up               #####
    ####################################################################
    # 1. Saving data.
    if (missing == "Imputation (replace missing values with estimates)")
        data <- processed.data$data
    result$subset <- subset <- row.names %in% rownames(.estimation.data)
    result$weights <- unfiltered.weights
    result$model <- data
    # 2. Saving descriptive information.
    class(result) <- "LDA"
    result$sample.description <- processed.data$description
    result$n.predictors <- ncol(.estimation.data) - 1
    result$n.observations <- n
    result$estimation.data <- .estimation.data
    result$outcome.name <- outcome.name
    result$outcome.label <- outcome.label
    confusion <- ConfusionMatrix(result, subset, unfiltered.weights)
    result$confusion <- confusion / sum(confusion)
    # 3. Replacing names with labels
    result$show.labels <- show.labels
    # 4.Saving parameters
    result$formula <- input.formula
    result$output <- output
    result$missing <- missing
    result
}


#' \code{LDA.fit} Fits linear discriminant analysis models.
#'
#' @param x A matrix or data frame of explanatory variables.
#' @param grouping A variable containg the group memberships (i.e., to be
#'   predicted or explained by x).
#' @param prior The assumed probability of each value of y occurring in the
#'   population.  By default this is set to "observed" and the value is computed
#'   based on the observed data.  If set to "constant" the prior will be set to
#'   be equal for each group (this is the default in SPSS).  Alternatively, a
#'   vector of probabilities can be provided.
#' @param tol TODO
#' @param method TODO
#' @param weights TODO
#' @param CV TODO
#' @param nu TODO
#' @param ... Additional arguments.
#' @details This is a wrapper for MASS::lda and MASS::qda.
#'
#'   #### Linear discriminant analysis #####
#'   \url{http://www.ats.ucla.edu/stat/spss/output/SPSS_discrim.htm}
#' @importFrom flipStatistics WeightedCounts
#' @importFrom flipTransformations WeightedSVD
#' @export

LDA.fit = function (x,
                   grouping,
                   prior = proportions,
                   tol = 1e-04,
                   method = c("moment", "mle"),
                   weights = NULL,
                   CV = FALSE, nu = 5, ...)
{
    # Generalization of MASS::lda
    if (is.null(dim(x)))
        stop("'x' is not a matrix")
    x <- as.matrix(x)
    if (any(!is.finite(x)))
    stop("infinite, NA or NaN values in 'x'")
    if (is.null(weights))
        weights <- rep(1, nrow(x))
    n <- sum(weights)
    k <- ncol(x)
    if (nrow(x) != length(grouping))
        stop("'nrow(x)' and 'length(grouping)' are different")
    if (nrow(x) != length(grouping))
        stop("'nrow(x)' and 'length(weights)' are different")
    g <- as.factor(grouping)
    lev <- lev1 <- levels(g)
    nv <- nlevels(g) - 1L
    counts <- WeightedCounts(g, weights)
    if (!missing(prior))
    {
        if (any(prior < 0) || round(sum(prior), 5) != 1)
            stop("invalid 'prior'")
        if (length(prior) != nlevels(g))
            stop("'prior' is of incorrect length")
        prior <- prior[counts > 0L]
    }
    if (any(counts == 0L))
    {
        empty <- lev[counts == 0L]
        warning(sprintf(ngettext(length(empty), "group %s is empty",
                "groups %s are empty"), paste(empty, collapse = " ")), domain = NA)
        lev1 <- lev[counts > 0L]
        g <- factor(g, levels = lev1)
        counts <- as.vector(WeightedCounts(g, weights))
    }
    proportions <- prop.table(counts)
    ng <- length(proportions)
    names(prior) <- names(counts) <- lev1
    method <- match.arg(method)
    x.by.weights <- sweep(x, 1, weights, "*")
    group.sums <- tapply(x.by.weights, list(rep(g, k), col(x)), sum)
    group.means <- sweep(group.sums, 1, counts, "/")
    #print(group.means)
    var.weighted = cov.wt(x - group.means[g, ], weights, method == "mle")$cov
    f1 <- sqrt(diag(var.weighted))
    if (any(f1 < tol))
    {
        const <- format((1L:k)[f1 < tol])
        stop(sprintf(ngettext(length(const), "variable %s appears to be constant within groups",
                "variables %s appear to be constant within groups"),
                 paste(const, collapse = " ")), domain = NA)
    }
    weights.sum = sum(weights)
    scaling <- diag(1/f1, , k)
    fac <- if (method == "moment") 1 / (weights.sum - ng) else 1 / weights.sum
    if (method == "moment")
        var.weighted = var.weighted * weights.sum / (weights.sum - ng)
    X <- sqrt(fac) * (x - group.means[g, ]) %*% scaling
#    print(cbind(x,X,g, weights))
    X.s <- WeightedSVD(X, weights, nu = 0L)
    #print(X.s)
    #stop("dog")
    #print(X.s)
    rank <- sum(X.s$d > tol)
    if (rank == 0L)
        stop("rank = 0: variables are numerically constant")
    if (rank < k)
        warning("variables are collinear")
    scaling <- scaling %*% X.s$v[, 1L:rank] %*% diag(1/X.s$d[1L:rank],, rank)
    # if (CV)
    # {
    #     x <- x %*% scaling
    #     dm <- group.means %*% scaling
    #     K <- if (method == "moment") ng else 0L
    #     dist <- matrix(0, n, ng)
    #     for (i in 1L:ng)
    #     {
    #         dev <- x - matrix(dm[i, ], n, rank, byrow = TRUE)
    #         dist[, i] <- rowSums(dev^2)
    #     }
    #     ind <- cbind(1L:n, g)
    #     nc <- counts[g]
    #     cc <- nc/((nc - 1) * (n - K))
    #     dist2 <- dist
    #     for (i in 1L:ng)
    #     {
    #         dev <- x - matrix(dm[i, ], n, rank, byrow = TRUE)
    #         dev2 <- x - dm[g, ]
    #         tmp <- rowSums(dev * dev2)
    #         dist[, i] <- (n - 1L - K)/(n - K) * (dist2[, i] +
    #             cc * tmp^2/(1 - cc * dist2[ind]))
    #     }
    #     dist[ind] <- dist2[ind] * (n - 1L - K)/(n - K) * (nc/(nc -
    #         1))^2/(1 - cc * dist2[ind])
    #     dist <- 0.5 * dist - matrix(log(prior), n, ng, byrow = TRUE)
    #     dist <- exp(-(dist - min(dist, na.rm = TRUE)))
    #     cl <- factor(lev1[max.col(dist)], levels = lev)
    #     posterior <- dist/drop(dist %*% rep(1, length(prior)))
    #     dimnames(posterior) <- list(rownames(x), lev1)
    #     return(list(class = cl, posterior = posterior))
    # }
    xbar <- colSums(prior %*% group.means)
#    print(x)
    fac <- if (method == "mle") 1/ng  else 1/(ng - 1)
    X <- sqrt((n * prior) * fac) * scale(group.means, center = xbar,
                                       scale = FALSE) %*% scaling
    X.s <- svd(X, nu = 0L)
    #print(X.s$d)
    #X.s <- WeightedSVD(X, weights, nu = 0L, nv = nv)
    #print(X.s$d)

    #print(X.s)
    rank <- sum(X.s$d > tol * X.s$d[1L])
    if (rank == 0L)
        stop("group means are numerically identical")
    scaling <- scaling %*% X.s$v[, 1L:rank]
    if (is.null(dimnames(x)))
      dimnames(scaling) <- list(NULL, paste("LD", 1L:rank,
                                          sep = ""))
    else
    {
        dimnames(scaling) <- list(colnames(x), paste("LD", 1L:rank,
                                                 sep = ""))
        dimnames(group.means)[[2L]] <- colnames(x)
    }
    cl <- match.call()
    cl[[1L]] <- as.name("lda")
    structure(list(prior = prior, counts = counts, means = group.means,
                 scaling = scaling, lev = lev, svd = X.s$d[1L:rank], N = n,
                 call = cl), class = "lda")
}

#' @importFrom MASS lda
#' @importFrom flipFormat FormatAsPercent MeanComparisonsTable
#' @importFrom flipAnalysisOfVariance CompareMultipleMeans MeansTables
#' @export
print.LDA <- function(x, p.cutoff = 0.05, digits = max(3L, getOption("digits") - 3L), ...)
{
    output <- x$output
    if (output == "Confusion Matrix")
    {
        return(print(x$confusion * 100))
    }
    else if (output == "Means")
    {
        outcome.name <- x$outcome.name
        data <- x$estimation.data
        outcome <- data[, outcome.name]
        predictors <- data[, -match(outcome.name, names(data))]
        means <- CompareMultipleMeans(predictors, outcome, compare = "Columns")
        m <- MeansTables(means)
        nms <- names(predictors)
        if (x$show.labels)
            for (i in 1:ncol(predictors))
            {
                label <- attr(predictors[, i], "label")
                if (!is.null(label))
                    nms[i] <- label
            }
        rownames(m$means) <- nms
#        rows <- list(Outdoor = dat$outdoor, Social = dat$social, "Conservative" = dat$conservative)

        # Statistics.
        title = paste("Linear Discriminant Analysis:", x$outcome.label)
        confusion <- x$confusion
        accuracy <- sum(diag(confusion))
        accuracy <- FormatAsPercent(accuracy)
        accuracy.by.group <- diag(confusion) / apply(confusion, 1, sum)
        nms <- names(accuracy.by.group)
        accuracy.by.group <- FormatAsPercent(accuracy.by.group)
        accuracy.by.group <- paste0(nms, ": ", accuracy.by.group)
        accuracy.by.group <- paste(accuracy.by.group, collapse = ", ")
        subtitle = paste0("Accuracy: ", accuracy, " (", accuracy.by.group, ")")
        result <- MeanComparisonsTable(m$means, m$zs, m$ps, m$r.squared, m$overall.p, m$column.names, "", title = title, subtitle = subtitle)
        return(print(result))

    }
    print(x$original, ...)
    # weighted <- !is.null(x$weights)
    # # Checking for unusual observations.
    # if (x$type != "Ordered Logit" & x$type != "Multinomial Logit" & x$missing != "Use partial data (pairwise correlations)")
    # {
    #     capture.output(unusual <- UnusualObservations(x))
    #     if (!is.null(unusual))
    #         warning(unusual)
    # }
    # # Testing to see if there is multicollinearity.
    # if (ncol(x$model) > 2 & x$type == "Linear" & x$missing != "Use partial data (pairwise correlations)")
    # {
    #     vifs <- vif(x)
    #     if (!is.null(vifs))
    #     {
    #         max.vif <- max(vifs)
    #         if (max.vif >= 4)
    #         {
    #             pref <- if(x$type == "Linear") "" else "Generalized "
    #             nms <- rownames(x$summary$coefficients)[-1]
    #             VIFs <- paste0(nms,": ", FormatAsReal(vifs, 2), c(rep("; ", length(nms) - 1), ""), collapse = "")
    #             warning(paste0("The ",pref, "Variance Inflation Factor of the coefficients are: ", VIFs,". A value of 4 or more indicates the confidence interval for the coefficient is twice as wide as they would be for uncorrelated predictors. A value of 10 or more indicates high multicollinearity."))
    #         }
    #     }
    # }
    # #Testing to see if the variance is non-constant.
    # partial <- x$missing == "Use partial data (pairwise correlations)"
    # if (x$type == "Linear" & !partial & !weighted)
    # {
    #     bp.test <- ncvTest(x)#BreuschPagan(x$original)
    #     if (bp.test$p <= 0.05)
    #     {
    #         suggest <- if(is.null(x$partial.coefs)) " Or, consider using Robust Standard Errors." else ""
    #         warning(paste0("A Breusch Pagan Test for non-constant variance has been failed (p = ",
    #                        FormatAsPValue(bp.test$p), "). A plot of the residuals versus the fitted values of the outcome variable may be useful (Insert > Advanced > Regression > Plots > Residuals vs Fitted). A transformation of the outcome or predictor variables may solve this problem.",
    #                        suggest, "\n"))
    #         outcome.variable <- outcomeVariableFromModel(x)
    #     }
    # }
    # outcome.variable <- outcomeVariableFromModel(x)
    # if (length(unique(outcome.variable)) == 2 && x$type == "Linear")
    #     warning(paste0("The outcome variable contains only two unique values. A Binary Logit may be
    #                    more appropriate."))
    # else
    # {
    #     if (x$type == "Linear" & IsCount(outcome.variable))
    #         warning(paste0("The outcome variable appears to contain count data (i.e., the values are non-negative integers). A limited dependent variable regression may be more appropriate (e.g., Quasi-Poisson Regression, Ordered Logit)."))
    # }
    # # Creating a nicely formatted text description of the model.
    # aic <- if(partial) NA else AIC(x)
    # rho.2 <- if(partial | x$type == "Linear") NA else McFaddensRhoSquared(x)
    # caption <- x$sample.description
    # caption <- if (partial)
    #     paste0(caption," R-squared: ", FormatAsReal(x$original$original$R2, 4), "; ")
    # else
    #     paste0(caption," R-squared: ", FormatAsReal(GoodnessOfFit(x)$value, 4),
    #            "; Correct predictions: ", FormatAsPercent(Accuracy(x), 4),
    #            if (is.null(rho.2) | is.na(rho.2)) "" else paste0("; McFadden's rho-squared: ", round(rho.2, 4)),
    #            if (is.na(aic)) "" else paste0("; AIC: ", FormatAsReal(aic, 5), "; "))
    # if (x$detail) # Detailed text output.
    # {
    #     cat(paste0(x$type, " regression\n"))
    #     if (x$missing == "Multiple imputation")
    #     {
    #         printCoefmat(x$summary$coefficients)
    #         cat(caption)
    #     }
    #     else
    #     {
    #         print(x$summary, ...)
    #         # if (!is.null(x$original$original))
    #         #     cat(paste0("Partial-data R-squared ", flipU::FormatAsReal(x$original$original$R2, 4), " (the R-squared and F above are based only on complete cases).\n"))
    #         cat(caption)
    #         if (x$robust.se)
    #             cat("Heteroscedastic-robust standard errors.")
    #     }
    #
    #
    # }
    # else # Pretty table.
    # {
    #     add.regression <- x$type %in% c("Linear", "Poisson", "Quasi-Poisson", "NBD")
    #     title <- c(paste0(x$type, (if(add.regression) " Regression" else ""), ": ",
    #                       if(x$show.labels) x$outcome.label else x$outcome.name))
    #     coefs <- x$summary$coefficients
    #     t <- "t" == substr(colnames(coefs)[3], 1, 1)
    #     caption <- paste0(caption, "results highlighted when p <= " , p.cutoff)
    #     dt <- PrettyRegressionTable(coefs,
    #                                 t,
    #                                 title = title,
    #                                 #subtitle = x$call,
    #                                 footer = caption)
    #     #dt <- createRegressionDataTable(x, p.cutoff = p.cutoff, caption = caption)
    #     print(dt)
    # }
}

