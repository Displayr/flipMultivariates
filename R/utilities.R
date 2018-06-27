rowNamesOrNames <- function(x)
{
    if (is.matrix(x))
        return(rownames(x))
    names(x)
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

#' @importFrom flipData GetData EstimationData DataFormula
#' @importFrom flipFormat Labels
#' @importFrom flipU OutcomeName
#' @importFrom flipTransformations AdjustDataToReflectWeights
prepareMachineLearningData <- function(formula, data, subset, subset.description,
                                       weights, weights.description, missing, seed)
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
    processed.data <- EstimationData(input.formula, data, subset, weights, missing, seed = seed)
    unfiltered.weights <- processed.data$unfiltered.weights
    unweighted.training.data <- processed.data$estimation.data
    n <- nrow(unweighted.training.data)
    if (n < ncol(unweighted.training.data) + 1)
        stop("The sample size is too small for it to be possible to conduct the analysis.")
    cleaned.weights <- processed.data$weights
    data.formula <- DataFormula(input.formula, data)

    # Resampling to generate a weighted sample, if necessary.
    weighted.training.data <- if (is.null(weights))
        unweighted.training.data
    else
        AdjustDataToReflectWeights(unweighted.training.data, cleaned.weights)

    return(list(unweighted.training.data = unweighted.training.data,
                weighted.training.data = weighted.training.data,
                data.formula = data.formula,
                row.names = row.names,
                unfiltered.weights = unfiltered.weights,
                outcome.name = outcome.name,
                sample.description = processed.data$description,
                n = n,
                numeric.outcome = numeric.outcome,
                outcome.label = outcome.label,
                variable.labels = variable.labels,
                outcome.i = outcome.i,
                input.formula = input.formula))
}
