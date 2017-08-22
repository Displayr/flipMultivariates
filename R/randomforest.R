#' \code{RandomForest}
#'
#' RandomForest
#' @param formula A formula of the form \code{groups ~ x1 + x2 + ...}
#' That is, the response is the grouping factor and the right hand side
#' specifies the (non-factor) discriminators, and any transformations, interactions,
#' or other non-additive operators will be ignored.
#' @param data A \code{\link{data.frame}} from which variables specified
#' in formula are preferentially to be taken.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression.
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param output One of \code{"Importance"}, \code{"Prediction-Accuracy Table"} or \code{"Detail"}.
#' @param missing How missing data is to be treated in the regression. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"},
#' @param seed The random number seed used in imputation.
#' @param statistical.assumptions A Statistical Assumptions object.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param sort.by.importance Sort the last column of the importance table
#' in descending order.
#' @param ... Other arguments to be supplied to \code{\link{randomForest}}.
#' @importFrom flipData GetData CleanSubset CleanWeights EstimationData DataFormula
#' @importFrom stats pnorm
#' @importFrom flipFormat Labels
#' @importFrom flipU OutcomeName
#' @importFrom randomForest randomForest
#' @importFrom flipTransformations AdjustDataToReflectWeights
#' @export
RandomForest <- function(formula,
                         data = NULL,
                         subset = NULL,
                         weights = NULL,
                         output = "Importance",
                         missing  = "Exclude cases with missing data",
                         seed = 12321,
                         statistical.assumptions,
                         show.labels = FALSE,
                         sort.by.importance = TRUE,
                         ...)
{
    ####################################################################
    ##### Reading in the data and doing some basic tidying        ######
    ####################################################################
    cl <- match.call()
    if(!missing(statistical.assumptions))
        stop("'statistical.assumptions' objects are not yet supported.")
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
    if (setequal(data[!is.na(data[, outcome.i]), outcome.i], c(0, 1)))
        data[, outcome.i] <- as.factor(data[, outcome.i])
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
    processed.data <- EstimationData(input.formula, data, subset, weights, missing,seed = seed)
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
    result <- list(original = randomForest(.formula,
                                           importance = TRUE, data = .estimation.data.1))
    result$original$call <- cl
    ####################################################################
    ##### Saving results, parameters, and tidying up               #####
    ####################################################################
    # 1. Saving data.
    result$subset <- subset <- row.names %in% rownames(.estimation.data)
    result$weights <- unfiltered.weights
    result$model <- data

    # 2. Saving descriptive information.
    class(result) <- "RandomForest"
    result$outcome.name <- outcome.name
    result$sample.description <- processed.data$description
    result$n.observations <- n
    result$estimation.data <- .estimation.data
    result$numeric.outcome <- numeric.outcome

    # 3. Replacing names with labels
    if (result$show.labels <- show.labels)
    {
        result$outcome.label <- outcome.label
        # Removing the outcome variable
        result$variable.labels <- variable.labels <- variable.labels[-outcome.i]
        if (numeric.outcome)
            names(result$original$importanceSD) <- variable.labels
        else
            rownames(result$original$importanceSD) <- variable.labels
        # As predict.lda uses the variable labels as an input, the final swap to labels for the importance tables appears in print.RandomForest
    }
    else
        result$outcome.label <- outcome.name

    # 4.Saving parameters and confusion matrix
    result$formula <- input.formula
    result$output <- output
    result$missing <- missing
    result$sort.by.importance <- sort.by.importance
    result$confusion <- ConfusionMatrix(result, subset, unfiltered.weights)

    # 5. Statistics
    result$z.statistics <- result$original$importance[, 1:(ncol(result$original$importance) - 1)] / result$original$importanceSD
    result$p.values <- 2 * (1 - pnorm(abs(result$z.statistics)))
    result
}

#' @import randomForest
#' @importFrom flipFormat RandomForestTable FormatAsReal RandomForestTable ExtractCommonPrefix
#' @export
print.RandomForest <- function(x, ...)
{
    if (x$show.labels)
        rownames(x$original$importance) <- x$variable.labels
    if (x$output == "Importance")
    {
        title <- paste0("Random Forest: ", x$outcome.label)
        imp <- x$original$importance
        extracted <- ExtractCommonPrefix(rownames(imp))
        if (!is.na(extracted$common.prefix))
        {
            title <- paste0(title, " by ", extracted$common.prefix)
            rownames(imp) <- extracted$shortened.labels
        }
        subtitle <- if (x$numeric.outcome)
            paste("R-squared:", FormatAsReal(x$original$rsq[length(x$original$rsq)], decimals = 3))
        else
        {
            err <- x$original$err.rate
            accuracies <- 1 - err[nrow(err), ]
            k <- length(accuracies)
            correctPredictionsText(accuracies[1], colnames(err)[2:k], accuracies[2:k])
        }
        tbl <- RandomForestTable(imp,
                                 x$z.statistics,
                                 x$p.values,
                                 x$sort.by.importance,
                                 title = title,
                                 subtitle = subtitle,
                                 footer = x$sample.description)
        print(tbl)
    }
    else if (x$output == "Prediction-Accuracy Table")
    {
        print(x$confusion)
    }
    else
    {
        print(x$original)
        invisible(x)
    }
}

