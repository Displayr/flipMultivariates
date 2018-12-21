#' Perform linear discriminant analysis
#'
#' @param formula A formula of the form \code{groups ~ x1 + x2 + ...}
#' That is, the response is the grouping factor and the right hand side
#' specifies the (non-factor) discriminators, and any transformations, interactions,
#' or other non-additive operators apart from \code{.} will be ignored.
#' @param data A \code{\link{data.frame}} from which variables specified
#' in formula are preferentially to be taken.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression.
#' @param weights An optional vector of sampling weights, or the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param missing How missing data is to be treated in the regression. Options:
#'   \code{"Error if missing data"}
#'   \code{"Exclude cases with missing data"}
#'   \code{"Imputation (replace missing values with estimates)"}
#' @param prior The assumed probability of each value of y occurring in the
#'   population.  By default this is set to "Observed" and the value is computed
#'   based on the observed data.  If set to "Equal" the prior will be set to
#'   be equal for each group (this is the default in SPSS).  Alternatively, a
#'   vector of probabilities can be provided.
#' @param output One of \code{"Means"}, \code{"Prediction-Accuracy Table"}, or \code{"Detail"}.
#' \code{"Scatterplot"}, \code{"Moonplot"} or \code{"Discriminant Functions"}.
#' @param outcome.color Color used to display centroids in \code{"Scatterplot"} output.
#' @param predictors.color Color used to display variable correlations in \code{"Scatterplot"} output.
#' @param variance The method used to estimate the variance; either \code{"moment"} for
#' the method of moments or \code{"mle"} for maximum likelihood estimaion.
#' @param seed The random number seed used in imputation.
#' @param auxiliary.data A \code{\link{data.frame}} containing additional variables
#'  to be used in imputation (if required). While adding more variables will improve
#'  the quality of the imputation, it will dramatically slow down the time to estimate.
#'  Factors and character variables with a large number of categories should not be included,
#'  as they will both slow down the data and are unlikely to be useful.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#'  variable's label is an attribute (e.g., attr(foo, "label")).
#' @param ... Additional argments to be past to  \code{LDA.formula}.
#' @details Imputation (replace missing values with estimates): All selected
#'   outcome and predictor variables are included in the imputation, along with
#'   all \code{auxiliary.data}, excluding cases that are excluded via subset or
#'   have invalid weights, but including cases with missing values of the outcome variable.
#'   Then, cases with missing values in the outcome variable are excluded from
#'   the analysis (von Hippel 2007). See \code{\link[flipImputation]{Imputation}}.
#' @references von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#'   Improved Strategy for Analyzing Multiply Imputed Data." Sociological
#'   Methodology 37:83-117. White, H. (1980), A heteroskedastic-consistent
#'   covariance matrix estimator and a direct test of heteroskedasticity.
#'   Econometrica, 48, 817-838. Long, J. S. and Ervin, L. H. (2000). Using
#'   heteroscedasticity consistent standard errors in the linear regression
#'   model. The American Statistician, 54(3): 217-224.
#' @importFrom flipData CalibrateWeight GetData CleanSubset CleanWeights EstimationData
#' @importFrom flipFormat Labels Names
#' @importFrom flipTransformations CreatingFactorDependentVariableIfNecessary AsNumeric Factor StandardizeData
#' @importFrom flipStatistics Correlation MeanByGroup
#' @importFrom flipU OutcomeName IsCount
#' @importFrom stats aggregate
#' @importFrom flipRegression ConfusionMatrix
#' @aliases LinearDiscriminantAnalysis
#' @export
LDA <- LinearDiscriminantAnalysis <- function(formula,
                data = NULL,
                subset = NULL,
                weights = NULL,
                prior = "Observed",
                missing = "Exclude cases with missing data",
                output = "Means",
                outcome.color = '#5B9BD5',
                predictors.color = '#ED7D31',
                variance = "moment",
                seed = 12321,
                auxiliary.data = NULL,
                show.labels = FALSE,
                ...)
{

    ####################################################################
    ##### Reading in the data and doing some basic tidying        ######
    ####################################################################

    # Identify whether subset and weights are variables in the environment or in data.
    subset.description <- try(deparse(substitute(subset)), silent = TRUE)
    subset <- eval(substitute(subset), data, parent.frame())
    weights.description <- try(deparse(substitute(weights)), silent = TRUE)
    weights <- eval(substitute(weights), data, parent.frame())

    ####################################################################
    ##### Data manipulation specific to LDA                        #####
    ####################################################################

    ####################################################################
    ##### Prepare the data                                        ######
    ####################################################################

    prepared.data <- prepareMachineLearningData(formula, data, subset, subset.description,
                                                weights, weights.description, missing, seed,
                                                bootstrap.weights = FALSE, dummy = TRUE)

    unweighted.training.data <- prepared.data$unweighted.training.data
    required.data <- prepared.data$required.data
    outcome.name <- prepared.data$outcome.name
    outcome.i <- prepared.data$outcome.i

    ####################################################################
    ##### Data manipulation specific to LDA                        #####
    ####################################################################

    if (!is.factor(required.data[, outcome.name]) & !IsCount(required.data[, outcome.name]))
        stop("LDA requires the outcome variable to be categorical or a count.")
    factor.levels <- attr(required.data, "factor.levels")
    required.data <- CreatingFactorDependentVariableIfNecessary(formula, required.data)
    unweighted.training.data <- CreatingFactorDependentVariableIfNecessary(formula, unweighted.training.data)

    extracted <- ExtractCommonPrefix(prepared.data$variable.labels[-outcome.i])
    by.label <- if(is.na(extracted$common.prefix)) "" else paste0(" by ", extracted$common.prefix)
    labels <- extracted$shortened.labels
    prepared.data$outcome.label <- paste0(prepared.data$outcome.label, if (output == "Scatterplot") "" else by.label)

    # Computing and checking the prior.
    filtered.outcome.variable <- Factor(unweighted.training.data[, outcome.name])
    if (is.null(weights))
        observed.prior <- as.numeric(prop.table(table(filtered.outcome.variable)))
    else
    {
        df <- data.frame(x = filtered.outcome.variable, w = prepared.data$cleaned.weights)
        observed.prior <- aggregate(w ~ x, data = df, FUN = sum)
        observed.prior <- as.numeric(prop.table(observed.prior[, 2]))
    }
    n.levels <- nlevels(filtered.outcome.variable)
    if (n.levels > 10)
        warning(paste("The outcome variable contains", n.levels, "categories. Consider either merging categories, or, using a model more appropriate for such data (e.g., Linear Regression)."))
    if (n.levels == 1)
        stop("The outcome variable contains only one category, after applying any filter. At least 2 categories are required to produce a model.")
    n.smallest <- round((min.o <- min(observed.prior)) * prepared.data$n)
    if (n.smallest < 30)
    {
        smallest.category <- levels(required.data[, outcome.i])[match(min.o, observed.prior)[1]]
        warning(paste0("The smallest category of the outcome variable (", smallest.category, ") contains ",
                       n.smallest, " observations; a robust model is unlikely."))
    }
    equal.prior <- rep(1 / n.levels, n.levels)
    if (is.character(prior))
        prior <- switch(prior,
            "Observed" = observed.prior,
            "Equal" = equal.prior)
    error <- paste0("The 'prior' must be one of: (1) 'Equal', ",
                    "(2) 'Observed', ",
                    "(3) or a vector of length ", n.levels, " containing values greater than 0 and less than 1 which sum to 1.")
    if (is.null(prior))
        stop(error)
    else if (!is.numeric(prior))
        stop(error)
    else if (length(prior) != n.levels)
        stop(error)
    else if (abs(sum(prior) - 1) > 1e-10 | min(prior) <= 0 | max(prior) >= 1)
        stop(error)

    ####################################################################
    ##### Fitting the model. Ideally, this should be a call to     #####
    ##### another function, with the output of that function       #####
    ##### called 'original'.                                       #####
    ####################################################################

    x <- unweighted.training.data[, -outcome.i, drop = FALSE]
    group <- unweighted.training.data[, outcome.name]
    labels <- labels[match(names(x), names(labels))]
    result <- list(call = match.call(),
                   original = LDA.fit(x,
                   grouping = group,
                   prior = prior,
                   method = variance,
                   weights = prepared.data$cleaned.weights,
                   labels = labels,
                   functions.output = output == "Discriminant Functions"))

    ####################################################################
    ##### Saving direct input and model-specific parameters        #####
    ####################################################################

    result$original$call <- match.call()
    class(result) <- c("LDA", class(result))
    result$output <- output
    result$outcome.color <- outcome.color
    result$predictors.color <- predictors.color
    result$missing <- missing
    result$predictors.label <- if (by.label == "" | !show.labels) "Predictors" else extracted$common.prefix
    if (missing == "Imputation (replace missing values with estimates)")
        prepared.data$required.data <- prepared.data$imputed.data

    result$observed.prior <- observed.prior
    result$equal.prior <- equal.prior
    result$prior <- prior
    dv <- predict(result$original,prior = observed.prior, newdata = x, na.action = na.pass)[["x"]]
    result$centroids <- MeanByGroup(dv, group, prepared.data$cleaned.weights)
    result$factor.levels <- factor.levels
    result$correlations <- Correlation(x, dv)
    rownames(result$correlations) <- labels

    if (show.labels)
    {
        colnames(result$original$means) <- labels
        row.names(result$original$scaling) <- labels
        if (!is.null(result$original$discriminant.functions))
            rownames(result$original$discriminant.functions) <- c("Intercept", labels)
    }

    ####################################################################
    ##### Saving processed information                             #####
    ####################################################################

    result <- saveMachineLearningResults(result, prepared.data, show.labels)
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
#' @param tol Tolerance to decide if a matrix is singular.
#' @param method The method used to estimate the variance; either \code{"moment"} for
#' the method of moments or \code{"mle"} for maximum likelihood estimaion.
#' @param weights An optional vector of sampling weights.
#' @param CV Not used.
#' @param nu the number of left singular vectors to be computed.
#' @param labels The labels of the predictor variables.
#' @param functions.output Logical; whether the discriminant functions are the
#'   required output of \code{\link{LDA}}.
#' @param ... Additional arguments.
#' @details This is a wrapper for MASS::lda and MASS::qda.
#'   #### Linear discriminant analysis #####
#'   \url{http://www.ats.ucla.edu/stat/spss/output/SPSS_discrim.htm}
#' @importFrom flipStatistics WeightedCounts
#' @importFrom flipTransformations WeightedSVD
#' @importFrom stats cov.wt cor alias
#' @export
LDA.fit <- function (x,
                   grouping,
                   prior = proportions,
                   tol = 1e-04,
                   method = c("moment", "mle"),
                   weights = NULL,
                   CV = FALSE,
                   nu = 5,
                   labels = NULL,
                   functions.output = FALSE, ...)
{
    # Generalization of MASS::lda
    if (is.null(dim(x)))
        stop("'x' is not a matrix")
    x <- as.matrix(x)
    if (any(!is.finite(x)))
    stop("Input data contains infinite, NA or NaN values.")
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
        if (any(prior < 0))
            stop("Prior probabilities of class membership must not be negative.")
        if (round(sum(prior), 5) != 1)
            stop("Prior probabilities of class membership must sum to 1.")
        if (length(prior) != nlevels(g))
            stop("There shoul be ", nlevels(g), " prior probabilities but ",
                 length(prior), " have been supplied.")
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

    al <- suppressWarnings(alias(grouping ~ . , data = data.frame(x)))
    if(!is.null(al$Complete))
        warning(paste0("Variables are colinear which may cause LDA to fail. Removing variable(s) ",
                       paste(labels[match(rownames(al$Complete), colnames(x))], collapse = ", "), " may help."))

    proportions <- prop.table(counts)
    ng <- length(proportions)
    names(prior) <- names(counts) <- lev1
    method <- match.arg(method)
    x.by.weights <- sweep(x, 1, weights, "*")
    group.sums <- tapply(x.by.weights, list(rep(g, k), col(x)), sum)
    group.means <- sweep(group.sums, 1, counts, "/")

    var.weighted = cov.wt(x - group.means[g, ], weights, method == "mle")$cov
    f1 <- sqrt(diag(var.weighted))
    if (any(f1 < tol))
    {
        const <- format((1L:k)[f1 < tol])
        stop(sprintf(ngettext(length(const),
                              "Variable %s is constant within groups and an LDA model cannot be fitted. Please remove the variable.",
                              "Variables %s are constant within groups and an LDA model cannot be fitted. Please remove the variables."),
                     paste(const, collapse = " ")), domain = NA)
    }
    weights.sum = sum(weights)
    scaling <- diag(1/f1, , k)
    fac <- if (method == "moment") 1 / (weights.sum - ng) else 1 / weights.sum
    if (method == "moment")
        var.weighted = var.weighted * weights.sum / (weights.sum - ng)
    X <- sqrt(fac) * (x - group.means[g, ]) %*% scaling
    X.s <- suppressWarnings(WeightedSVD(X, weights, nu = 0L)) # warning is better handled below
    X.s$d[is.nan(X.s$d)] <- 0
    rank <- sum(X.s$d > tol)
    if (rank == 0L)
        stop("Variable(s) are constant but must contain different values.")

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
    fac <- if (method == "mle") 1/ng  else 1/(ng - 1)
    X <- sqrt((n * prior) * fac) * scale(group.means, center = xbar,
                                       scale = FALSE) %*% scaling
    X.s <- svd(X, nu = 0L)
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

    discriminant.functions <- lda.functions(x, grouping, group.means, prior, weights, functions.output)

    cl <- match.call()
    cl[[1L]] <- as.name("lda")
    result <- list(prior = prior, counts = counts, means = group.means,
                 scaling = scaling, lev = lev, svd = X.s$d[1L:rank], N = n,
                 discriminant.functions = discriminant.functions, call = cl)
    class(result) <- "lda"
    result
}

# Calculate linear discriminant functions
# as per equation 4.10 in Hastie, Elements of Statistical Learning
lda.functions <- function(x, groups, grp.means, prior, weights, functions.output){

    gr <- length(unique(groups))
    num.var <- ncol(x)

    W <- array(0, dim = c(num.var, num.var))

    for(i in 1:gr){
        filter <- groups == unique(groups)[i]
        cov <- cov.wt(x[filter, , drop = FALSE], wt = weights[filter], method = "ML")$cov * sum(weights[filter])
        W <- W + cov
    }

    V <- W / (sum(weights) - gr)
    iV <- try(solve(V), TRUE)
    if (inherits(iV, "try-error"))
    {
        message <- "Error calculating discriminant functions. This may sometimes be fixed by removing colinear variables."
        if (functions.output)
            stop(message)
        warning(message)
        return(NULL)
    }


    class.funs <- matrix(NA, nrow = num.var + 1, ncol = gr)
    colnames(class.funs) <- rownames(grp.means)
    rownames(class.funs) <- c("Intercept", colnames(grp.means))

    for(i in 1:gr) {
        class.funs[1, i] <- -0.5 * t(grp.means[i, ]) %*% iV %*% (grp.means[i, ])
        class.funs[2:(num.var + 1), i] <- iV %*% (grp.means[i, ])
    }

    class.funs[1,] <- class.funs[1,] + log(prior)
    return(class.funs)
}


#' print.LDA
#'
#' @param x The \link{LDA} object.
#' @param p.cutoff The alpha value to use when highlighting results.
#' @param digits The number of digits when printing the \code{"detail"} output.
#' @param ... Generic print arguments.
#' @importFrom flipFormat Labels Labels ExtractCommonPrefix
#' @importFrom flipAnalysisOfVariance CompareMultipleMeans
#' @importFrom MASS lda
#' @importFrom rhtmlLabeledScatter LabeledScatter
#' @importFrom rhtmlMoonPlot moonplot
#' @export
print.LDA <- function(x, p.cutoff = 0.05, digits = max(3L, getOption("digits") - 3L), ...)
{
    dependent.name <- x$outcome.name
    data <- x$estimation.data
    dependent <- data[, dependent.name]
    show.labels <- x$show.labels
    column.names <- levels(dependent)

    output <- x$output
    if (output == "Prediction-Accuracy Table")
    {
        print(x$confusion)
    }
    else if (output == "Means")
    {
        independents <- data[, -match(dependent.name, names(data))]
        subset <- x$subset
        weights <- x$weights[subset]
        confusion <- x$confusion
        confusion <- confusion / sum(confusion)
        subtitle = correctPredictionsText(sum(diag(confusion)), column.names,
                                          diag(confusion) / apply(confusion, 1, sum))

        title <- paste0("Linear Discriminant Analysis: ", x$outcome.label)
        if (show.labels)
                for (i in 1:ncol(independents))
                    attr(independents[, i], "label") <- x$variable.labels[i]
        table <- CompareMultipleMeans(independents,
                    dependent,
                    weights = weights,
                    show.labels = show.labels,
                    title = title,
                    subtitle = subtitle,
                    footer = x$sample.description)
        print(table)
    } else if (output == "Scatterplot" | output == "Moonplot")
    {
        scale <-  apply(abs(x$centroids), 2, mean) / apply(abs(x$correlations), 2, mean)
        correlations <- sweep(x$correlations, 2, scale, "*")
        if (output == "Moonplot")
            print(moonplot(x$centroids, correlations))
        else
        {
            coords <- rbind(x$centroids, correlations)
            groups <- c(rep(x$outcome.label, nrow(x$centroids)), rep(x$predictors.label, nrow(correlations)))
            gcolors <- c(x$outcome.color, x$predictors.color)
            print(LabeledScatter(X = coords[, 1],
                                             Y = coords[, 2],
                                             label = rownames(coords),
                                             group = groups,
                                             colors = gcolors,
                                             fixed.aspect = TRUE,
                                             title = "Linear Discriminant Analysis",
                                             x.title = "First linear discriminant",
                                             y.title = "Second linear discriminant",
                                             axis.font.size = 10,
                                             labels.font.size = 12,
                                             title.font.size = 20,
                                             y.title.font.size = 16,
                                             x.title.font.size = 16))
        }
    }
    else if (output == "Discriminant Functions") {
        print(x$original$discriminant.functions)
    }
    else
    {
        x$original$call <- x$formula
        print(x$original, ...)
    }
}
