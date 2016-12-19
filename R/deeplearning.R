#' \code{DeepLearning}
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
#' @param output One of \code{"Importance"}, or \code{"Detail"}.
#' @param missing How missing data is to be treated in the regression. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"},
#' @param seed The random number seed used in imputation.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param ... Other arguments to be supplied to \code{\link{randomForest}}.
#' @importFrom flipData GetData CleanSubset CleanWeights EstimationData DataFormula
#' @importFrom flipFormat Labels
#' @importFrom flipU OutcomeName
#' @importFrom flipTransformations AdjustDataToReflectWeights
#' @importFrom h2o h2o.init as.h2o h2o.deeplearning
#' @export
DeepLearning <- function(formula,
                data = NULL,
                subset = NULL,
                weights = NULL,
                output = "Importance",
                missing  = "Exclude cases with missing data",
                seed = 12321,
                show.labels = FALSE,
                ...)
{
    ####################################################################
    ##### Reading in the data and doing some basic tidying        ######
    ####################################################################
    cl <- match.call()
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
    processed.data <- EstimationData(input.formula, data, subset, weights, missing,seed = seed)
    unfiltered.weights <- processed.data$unfiltered.weights
    .estimation.data <- processed.data$estimation.data
    n.predictors <- ncol(.estimation.data)
    n <- nrow(.estimation.data)
    #if (n < ncol(.estimation.data) + 1)
    #    stop("The sample size is too small for it to be possible to conduct the analysis.")
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
    #set.seed(seed)
    #result <- list(original = randomForest(.formula,
    #    importance = TRUE, data = .estimation.data.1))

    result <- list()
    h2o.init()
    train.h2o <- as.h2o(as.data.frame(lapply(.estimation.data.1, as.numeric)),
                    destination_frame="train.h2o")
    obj <- h2o.deeplearning(x=(2:ncol(train.h2o)), y=1,
                            training_frame=train.h2o,
                            epochs = 50,
                            missing_values_handling = "Skip",
                            variable_importances = T,
                            max_confusion_matrix_size = 20,
                            diagnostics = T)
    result <- list()
    result$original <- obj
    result$call <- cl


    ####################################################################
    ##### Saving results, parameters, and tidying up               #####
    ####################################################################
    # 1. Saving data.
    result$subset <- subset <- row.names %in% rownames(.estimation.data)
    result$weights <- unfiltered.weights
    result$model <- data
    # 2. Saving descriptive information.
    class(result) <- "DeepLearning"
    result$outcome.name <- outcome.name
    result$sample.description <- processed.data$description
    result$n.observations <- n
    result$estimation.data <- .estimation.data
    result$numeric.outcome <- numeric.outcome
    #result$confusion <- ConfusionMatrix(result, subset, unfiltered.weights)
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
    # 4.Saving parameters
    if (missing == "Imputation (replace missing values with estimates)")
            data <- processed.data$data
    result$model <- data
    result$formula <- input.formula
    result$output <- output
    result$missing <- missing
    # 5. Statistics
    #result$z.statistics <- result$original$importance[, 1:(ncol(result$original$importance) - 1)] / result$original$importanceSD
    #result$p.values <- 2 * (1 - pnorm(abs(result$z.statistics)))
    result
}

#' \code{predict.DeepLearning}
#' @param object Object of class \code{"DeepLearning"}.
#' @param newdata Data to which to apply the prediction
#' @param ... Not used
#' @importFrom h2o as.h2o h2o.deeplearning
#' @export
predict.DeepLearning <- function(object, newdata = NULL)
{
    if (is.null(newdata))
        newdata <- object$estimation.data

    h2o.init(startH2O = FALSE)
    nd.h2o <- as.h2o(as.data.frame(lapply(newdata, as.numeric)),
                    destination_frame="nd.h2o")
    return(as.data.frame(predict(object$original, newdata=nd.h2o)))
}
