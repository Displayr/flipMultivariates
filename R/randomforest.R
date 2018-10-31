#' Fit a random forest model
#'
#' @param formula A formula of the form \code{groups ~ x1 + x2 + ...}
#' That is, the response is the grouping factor and the right hand side
#' specifies the (non-factor) discriminators, and any transformations, interactions,
#' or other non-additive operators apart from \code{.} will be ignored.
#' @param data A \code{\link{data.frame}} from which variables specified
#' in formula are preferentially to be taken.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or the name of a variable in \code{data}. It
#'   may not be an expression.
#' @param weights An optional vector of sampling weights, or the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param output One of \code{"Importance"}, \code{"Prediction-Accuracy Table"} or \code{"Detail"}.
#' @param missing How missing data is to be treated. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"}, or
#'   \code{"Imputation (replace missing values with estimates)"}.
#' @param seed The random number seed.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param sort.by.importance Sort the last column of the importance table
#' in descending order.
#' @param ... Other arguments to be supplied to \code{\link{randomForest}}.
#' @importFrom stats pnorm
#' @importFrom randomForest randomForest
#' @export
RandomForest <- function(formula,
                         data = NULL,
                         subset = NULL,
                         weights = NULL,
                         output = "Importance",
                         missing = "Exclude cases with missing data",
                         seed = 12321,
                         show.labels = FALSE,
                         sort.by.importance = TRUE,
                         ...)
{
    ####################################################################
    ##### Error checking specific to this function                ######
    ####################################################################

    # prepareMachineLearningData called with strict.var.names

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
                                                strict.var.names = TRUE)

    unweighted.training.data <- prepared.data$unweighted.training.data
    weighted.training.data <- prepared.data$weighted.training.data

    ####################################################################
    ##### Fitting the model. Ideally, this should be a call to     #####
    ##### another function, with the output of that function       #####
    ##### called 'original'.                                       #####
    ####################################################################

    set.seed(seed)
    result <- list(original = suppressWarnings(randomForest(prepared.data$input.formula,
                                                            importance = TRUE,
                                                            data = weighted.training.data
                                                            , ...)))

    ####################################################################
    ##### Saving direct input and model-specific parameters        #####
    ####################################################################

    result$original$call <- match.call()
    #result$original.subset <- CleanSubset(subset, nrow(data))
    result$output <- output
    result$missing <- missing
    result$sort.by.importance <- sort.by.importance
    result$z.statistics <- result$original$importance[, 1:(ncol(result$original$importance) - 1)] / result$original$importanceSD
    result$p.values <- 2 * (1 - pnorm(abs(result$z.statistics)))
    class(result) <- c("RandomForest", class(result))

    ####################################################################
    ##### Saving processed information                             #####
    ####################################################################

    result <- saveMachineLearningResults(result, prepared.data, show.labels)
    if (result$show.labels)
    {
        if (result$numeric.outcome)
            names(result$original$importanceSD) <- result$variable.labels
        else
            rownames(result$original$importanceSD) <- result$variable.labels
    }
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
            correctPredictionsText(accuracies[1], colnames(err)[2:k], accuracies[2:k], out.of.bag = TRUE)
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
        x$original$call <- x$formula
        print(x$original)
        invisible(x)
    }
}
