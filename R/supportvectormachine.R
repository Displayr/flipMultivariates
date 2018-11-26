#' Fit a support vector machine model
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
#' @param output One of \code{"Accuracy"}, \code{"Prediction-Accuracy Table"} or \code{"Detail"}.
#' @param missing How missing data is to be treated. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"}, or
#'   \code{"Imputation (replace missing values with estimates)"}.
#' @param cost A positive number controlling the compromoise between exactly fitting the training data
#' (higher cost) and the ability to generalise to unseen data (lower cost).
#' @param seed The random number seed.
#' @param statistical.assumptions A Statistical Assumptions object.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param ... Other arguments to be supplied to \code{\link{svm}}.
#'
#' @importFrom e1071 svm
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
    ##### Error checking specific to this function                ######
    ####################################################################

    if (cost <= 0)
        stop("cost must be positive but is ", cost)

    ####################################################################
    ##### Reading in the data and doing some basic tidying        ######
    ####################################################################

    # Identify whether subset and weights are variables in the environment or in data.
    subset.description <- try(deparse(substitute(subset)), silent = TRUE)
    subset <- eval(substitute(subset), data, parent.frame())
    weights.description <- try(deparse(substitute(weights)), silent = TRUE)
    weights <- eval(substitute(weights), data, parent.frame())

    prepared.data <- prepareMachineLearningData(formula, data, subset, subset.description,
                                                weights, weights.description, missing, seed,
                                                allow.single.categories = FALSE)

    unweighted.training.data <- prepared.data$unweighted.training.data
    weighted.training.data <- prepared.data$weighted.training.data
    data.formula <- prepared.data$data.formula

    ####################################################################
    ##### Fitting the model. Ideally, this should be a call to     #####
    ##### another function, with the output of that function       #####
    ##### called 'original'.                                       #####
    ####################################################################

    set.seed(seed)
    result <- list(original = svm(data.formula, data = weighted.training.data,
                                  probability = TRUE, cost = cost, ...))

    ####################################################################
    ##### Saving direct input and model-specific parameters        #####
    ####################################################################

    result$original$call <- match.call()
    result$output <- output
    result$missing <- missing
    class(result) <- c("SupportVectorMachine", class(result))

    ####################################################################
    ##### Saving processed information                             #####
    ####################################################################

    result <- saveMachineLearningResults(result, prepared.data, show.labels)
    result
}




#' @importFrom flipFormat DeepLearningTable ExtractCommonPrefix
#' @importFrom flipData Observed
#' @importFrom flipU IsCount
#' @importFrom utils read.table
#' @export
#' @method print SupportVectorMachine
print.SupportVectorMachine <- function(x, ...)
{
    if (x$output == "Accuracy")
    {
        title <- paste0("Support Vector Machine: ", x$outcome.label)
        if (x$show.labels)
        {
            predictors <- x$variable.labels
        }
        else
        {
            predictors <- attr(x$original$terms, "term.labels")
        }

        extracted <- ExtractCommonPrefix(predictors)
        if (!is.na(extracted$common.prefix))
        {
            predictors <- extracted$shortened.labels
        }
        predictors <- paste(predictors, collapse = ", ")

        if (!x$numeric.outcome)
        {
            confM <- x$confusion
            tot.cor <- sum(diag(confM))/sum(confM)
            class.cor <- unlist(lapply(1:nrow(confM), function(i) {confM[i,i]/sum(confM[i,])}))
            names(class.cor) <- colnames(confM)
            subtitle <- sprintf("Overall Accuracy: %.2f%%", tot.cor*100)
            tbl <- DeepLearningTable(class.cor*100,
                                     column.labels = "Accuracy by class (%)",
                                     order.values = FALSE,
                                     title = title,
                                     subtitle = paste(subtitle, " (Predictors: ", predictors, ")", sep = ""),
                                     footer = x$sample.description)
        }
        else
        {
            metrics <- numericOutcomeMetrics(Observed(x)[x$subset],
                                               predict(x)[x$subset],
                                               x$weights[x$subset])
            subtitle <- "Measure of fit"
            tbl <- DeepLearningTable(c("Root Mean Squared Error" = metrics$rmse,
                                       "R-squared" = metrics$r.squared),
                                     column.labels = " ",
                                     order.values = FALSE,
                                     title = title,
                                     subtitle = paste(subtitle, " (Predictors: ", predictors, ")", sep = ""),
                                     footer = x$sample.description)
        }
        print(tbl)

    }
    else if (x$output == "Prediction-Accuracy Table")
    {
        print(x$confusion)
    }
    else
    {
        x$original$call <- x$formula
        print(x$original)
        invisible(x)
    }
}

