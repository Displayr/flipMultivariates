#' \code{SupportVectorMachine}
#'
#' SupportVectorMachine
#' @param formula A formula of the form \code{groups ~ x1 + x2 + ...}
#' That is, the response is the grouping factor and the right hand side
#' specifies the (non-factor) discriminators, and any transformations, interactions,
#' or other non-additive operators will be ignored.
#' @param data A \code{\link{data.frame}} from which variables specified
#' in formula are preferentially to be taken.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression.
#' @param weights An optional vector of sampling weights, or the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param output One of \code{"Accuracy"}, or \code{"Detail"}.
#' @param missing How missing data is to be treated. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"},
#' @param cost A positive number controlling the compromoise between exactly fitting the training data
#' (higher cost) and the abailaity to generalise to unseen data (lower cost).
#' @param seed The random number seed used in imputation.
#' @param statistical.assumptions A Statistical Assumptions object.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param ... Other arguments to be supplied to \code{\link{svm}}.
#' @importFrom flipData GetData CleanSubset CleanWeights EstimationData DataFormula
#' @importFrom stats pnorm
#' @importFrom flipFormat Labels
#' @importFrom flipU OutcomeName
#' @importFrom e1071 svm
#' @importFrom flipTransformations AdjustDataToReflectWeights
#' @export
SupportVectorMachine <- function(formula,
                                 data = NULL,
                                 subset = NULL,
                                 weights = NULL,
                                 output = "Accuracy",
                                 missing  = "Exclude cases with missing data",
                                 cost = 1,
                                 seed = 12321,
                                 statistical.assumptions,
                                 show.labels = FALSE,
                                 ...)
{
    ####################################################################
    ##### Reading in the data and doing some basic tidying        ######
    ####################################################################
    cl <- match.call()
    if(!missing(statistical.assumptions))
        stop("'statistical.assumptions' objects are not yet supported.")
    if (cost <= 0)
        stop("cost must be positive.")
    input.formula <- formula # To work past scoping issues in car package: https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf.
    subset.description <- try(deparse(substitute(subset)), silent = TRUE) #We don't know whether subset is a variable in the environment or in data.
    subset <- eval(substitute(subset), data, parent.frame())
    if (!is.null(subset))
    {
        if (is.null(subset.description) | (class(subset.description) == "try-error") | !is.null(attr(subset, "name")))
            subset.description <- Labels(subset)
        if (is.null(attr(subset, "name")))
            attr(subset, "name") <- subset.description
    }
    if(!is.null(weights))
        if (is.null(attr(weights, "name")))
            attr(weights, "name") <- deparse(substitute(weights))
    weights <- eval(substitute(weights), data, parent.frame())
    data <- GetData(input.formula, data, auxiliary.data = NULL)
    row.names <- rownames(data)
    outcome.name <- OutcomeName(input.formula)
    outcome.i <- match(outcome.name, names(data))
    outcome.variable <- data[, outcome.i]
    numeric.outcome <- !is.factor(outcome.variable)
    variable.labels <- Labels(data)
    outcome.label <- variable.labels[outcome.i]
    if (outcome.label == "data[, outcome.name]")
        outcome.label <- outcome.name
    if (!is.null(weights) & length(weights) != nrow(data))
        stop("'weights' and 'data' are required to have the same number of observations. They do not.")
    if (!is.null(subset) & length(subset) > 1 & length(subset) != nrow(data))
        stop("'subset' and 'data' are required to have the same number of observations. They do not.")

    # Treatment of missing values.
    #print(paste("pre-EstimationData", str(data)))
    processed.data <- EstimationData(input.formula, data, subset, weights, missing, seed = seed)
    #print(table(processed.data$estimation.data$A7))
    #print(table(data$A7))
    #print(paste("post-EstimationData", str(processed.data$estimation.data)))

    # Reinstate levels of factors
    #factor.levels <- lapply(data, levels)
    #for (col in names(processed.data$estimation.data)) {
    #    print(col)
    #    print(factor.levels[col])
    #    print(levels(processed.data$estimation.data[col]))
    #}

    #print(paste("post-ReLevel", str(processed.data$estimation.data)))

    unfiltered.weights <- processed.data$unfiltered.weights
    .estimation.data <- processed.data$estimation.data
    n.predictors <- ncol(.estimation.data)
    n <- nrow(.estimation.data)
    if (n < ncol(.estimation.data) + 1)
        stop("The sample size is too small for it to be possible to conduct the analysis.")
    post.missing.data.estimation.sample <- processed.data$post.missing.data.estimation.sample
    .weights <- processed.data$weights
    .formula <- DataFormula(input.formula)
    # Resampling to generate a weighted sample, if necessary.
    .estimation.data.1 <- if (is.null(weights))
        .estimation.data
    else
        AdjustDataToReflectWeights(.estimation.data, .weights)
    ####################################################################
    ##### Fitting the model. Ideally, this should be a call to     #####
    ##### another function, with the output of that function       #####
    ##### called 'original'.                                       #####
    ####################################################################
    set.seed(seed)
    result <- list(original = svm(.formula, data = .estimation.data.1,
                                  probability = TRUE, cost = cost, ...))
    result$original$call <- cl
    ####################################################################
    ##### Saving results, parameters, and tidying up               #####
    ####################################################################
    # 1. Saving data.
    result$subset <- subset <- row.names %in% rownames(.estimation.data)
    result$weights <- unfiltered.weights
    result$model <- data
    # 2. Saving descriptive information.
    class(result) <- "SupportVectorMachine"
    result$outcome.name <- outcome.name
    result$sample.description <- processed.data$description
    result$n.observations <- n
    result$estimation.data <- .estimation.data
    result$numeric.outcome <- numeric.outcome
    result$confusion <- ConfusionMatrix(result, subset, unfiltered.weights)
    # 3. Replacing names with labels
    if (result$show.labels <- show.labels)
    {
        result$outcome.label <- outcome.label
        # Removing the outcome variable
        result$variable.labels <- variable.labels <- variable.labels[-outcome.i]
        # As predict.lda uses the variable labels as an input, the final swap to labels for the importance tables appears in print.RandomForest
    }
    else
        result$outcome.label <- outcome.name
    # 4.Saving parameters
    result$formula <- input.formula
    result$output <- output
    result$missing <- missing
    #result$sort.by.importance <- sort.by.importance
    # 5. Statistics - no importance for svm
    #result$z.statistics <- result$original$importance[, 1:(ncol(result$original$importance) - 1)] / result$original$importanceSD
    #result$p.values <- 2 * (1 - pnorm(abs(result$z.statistics)))
    result
}

#' @importFrom flipFormat DeepLearningTable FormatWithDecimals ExtractCommonPrefix
#' @export
print.SupportVectorMachine <- function(x, ...)
{
    if (x$output == "Accuracy")
    {
        title <- paste0("Support Vector Machine: ", x$outcome.name)
        subtitle <- ""
        if (!x$numeric.outcome)
        {
            #confM <- ConfusionMatrix(x)
            confM <- x$confusion
            tot.cor <- sum(diag(confM))/sum(confM)
            class.cor <- unlist(lapply(1:nrow(confM), function(i) {confM[i,i]/sum(confM[i,])}))
            names(class.cor) <- colnames(confM)
            #tmp.text <- paste(paste0(rownames(confM), ":"),
            #                  paste0(FormatWithDecimals(class.cor*100, 2), "%"),
            #                  collapse=", ")
            subtitle <- sprintf("Overall Accuracy: %.2f%%",
                                tot.cor*100)
            tbl <- DeepLearningTable(class.cor,
                                     column.labels = "Accuracy by class",
                                     order.values = FALSE,
                                     title = title,
                                     subtitle = subtitle,
                                     footer = x$sample.description)
            print(tbl)
        } else
        {
            pred <- x$original$fitted
            rmse <- sqrt(mean((pred - x$model[,1])^2))
            rsq <- (cor(pred, x$model[,1]))^2
            print(paste("R-squared : ", FormatWithDecimals(rsq, 2)))
            print(paste("RMSE      : ", FormatWithDecimals(rmse, 2)))
        }
    }
    else
    {
        print(x$original)
        invisible(x)
    }
}



#' \code{ConfusionMatrix}
#'
#' @param obj A model with an outcome variable.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression.
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @details Produces a confusion matrix for a trained model. Predictions are based on the
#' training data (not a separate test set).
#' The proportion of observed values that take the same values as the predicted values.
#' Where the outcome variable in the model is not a factor and not a count, predicted values
#' are assigned to the closest observed value.
#' @importFrom stats predict
#' @importFrom methods is
#' @importFrom flipData Observed
#' @importFrom flipRegression ConfusionMatrixFromVariables
#' @export
ConfusionMatrix.SupportVectorMachine <- function(obj, subset = NULL, weights = NULL)
{
    observed <- obj$estimation.data[ , obj$outcome.name]
    predicted <- obj$original$fitted
    return(ConfusionMatrixFromVariables(observed, predicted, subset, weights))
}
