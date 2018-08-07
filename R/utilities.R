
#' Perform standard data checking and tidying actions before fitting a machine learning model
#'
#' @importFrom flipData GetData EstimationData DataFormula
#' @importFrom flipFormat Labels
#' @importFrom flipU OutcomeName
#' @importFrom flipTransformations AdjustDataToReflectWeights
#' @importFrom stats as.formula var
#' @noRd
prepareMachineLearningData <- function(formula, data, subset, subset.description,
                                       weights, weights.description, missing, seed,
                                       bootstrap.weights = TRUE, dummy = FALSE)
{
    input.formula <- formula # To work past scoping issues in car package: https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf.

    if (!is.null(subset))
    {
        if (is.null(subset.description) | (class(subset.description) == "try-error") | !is.null(attr(subset, "name")))
            subset.description <- Labels(subset)
        if (is.null(attr(subset, "name")))
            attr(subset, "name") <- subset.description
    }

    if(!is.null(weights))
    {
        if (is.null(weights.description) | (class(weights.description) == "try-error") | !is.null(attr(weights, "name")))
            subset.description <- Labels(weights)
        if (is.null(attr(weights, "name")))
            attr(weights, "name") <- weights.description
    }

    data <- GetData(input.formula, data, auxiliary.data = NULL)
    row.names <- rownames(data)
    outcome.name <- OutcomeName(input.formula, data)
    outcome.i <- match(outcome.name, names(data))

    if (dummy)
    {
        # convert factors witn N levels to N-1 binary variables, used by LDA
        data <- cbind(data[outcome.i], AsNumeric(data[, -outcome.i], binary = TRUE, remove.first = TRUE))
        # remove constant variables caused by unpopulated levels
        data <- data[, c(TRUE, sapply(data[, -1], var, na.rm = TRUE) != 0)]
    }

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
    # rebuild formula because there are new variables for each level of factorial predictors
    form <- as.formula(paste(outcome.name, "~ ."))
    processed.data <- EstimationData(form, data, subset, weights, missing, seed = seed)
    unweighted.training.data <- processed.data$estimation.data
    n <- nrow(unweighted.training.data)
    if (n <= ncol(unweighted.training.data))
        stop("The sample size is too small. There should be more samples than predictor variables, but there are ",
             n, " samples and ", ncol(unweighted.training.data), " predictors.")
    cleaned.weights <- processed.data$weights

    # Resampling to generate a weighted sample, if necessary.
    weighted.training.data <- if (is.null(weights))
        unweighted.training.data
    else if (!bootstrap.weights)
        NULL
    else
        AdjustDataToReflectWeights(unweighted.training.data, cleaned.weights)

    return(list(unweighted.training.data = unweighted.training.data,
                weighted.training.data = weighted.training.data,
                required.data = data,
                imputed.data = processed.data$data,
                cleaned.weights = cleaned.weights,
                data.formula = DataFormula(input.formula, data),
                row.names = row.names,
                unfiltered.weights = processed.data$unfiltered.weights,
                outcome.name = outcome.name,
                sample.description = processed.data$description,
                n = n,
                numeric.outcome = numeric.outcome,
                outcome.label = outcome.label,
                variable.labels = variable.labels,
                outcome.i = outcome.i,
                input.formula = input.formula))
}

#' Save standard data after fitting a machine learning model
#'
#' @importFrom flipU IsCount
#' @noRd
saveMachineLearningResults <- function(result, prepared.data, show.labels)
{
    # Save data, subset and weights
    result$model <- prepared.data$required.data
    result$subset <- subset <- prepared.data$row.names %in% rownames(prepared.data$unweighted.training.data)
    result$weights <- prepared.data$unfiltered.weights
    result$formula <- prepared.data$input.formula

    # Save descriptive information.
    class(result) <- c("MachineLearning", class(result))
    result$outcome.name <- prepared.data$outcome.name
    result$sample.description <- prepared.data$sample.description
    result$n.observations <- prepared.data$n
    result$n.predictors <- ncol(prepared.data$unweighted.training.data) - 1
    result$estimation.data <- prepared.data$unweighted.training.data
    result$numeric.outcome <- prepared.data$numeric.outcome
    result$outcome.levels <- levels(result$estimation.data[result$outcome.name][[1]])

    # Replacing names with labels
    if (result$show.labels <- show.labels)
    {
        result$outcome.label <- prepared.data$outcome.label
        result$variable.labels <- prepared.data$variable.labels[-prepared.data$outcome.i]
    }
    else
    {
        result$outcome.label <- result$outcome.name
        result$variable.labels <- names(prepared.data$required.data[-prepared.data$outcome.i])
    }

    # Save confusion matrix
    result$confusion <- ConfusionMatrix(result, subset, prepared.data$unfiltered.weights, decimals)

    return(result)
}


#' @importFrom flipFormat FormatAsPercent
correctPredictionsText <- function(overall.accuracy, group.labels, group.accuracies, digits = 4, out.of.bag = FALSE)
{
    prefix <- if (out.of.bag)
        "Correct predictions (based on out-of-bag sample): "
    else
        "Correct predictions: "

    paste0(prefix, FormatAsPercent(overall.accuracy, digits), " (",
           paste0(group.labels, ": " , FormatAsPercent(group.accuracies, digits), collapse = "; "),
           ")" )
}

