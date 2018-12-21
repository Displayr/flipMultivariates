
#' Perform standard data checking and tidying actions before fitting a machine learning model
#'
#' @importFrom flipData GetData EstimationData DataFormula ErrorIfInfinity
#' @importFrom flipFormat Labels
#' @importFrom flipU OutcomeName AllVariablesNames
#' @importFrom flipTransformations AdjustDataToReflectWeights
#' @importFrom stats as.formula var
#' @noRd
prepareMachineLearningData <- function(formula, data, subset, subset.description,
                                       weights, weights.description, missing, seed,
                                       bootstrap.weights = TRUE, dummy = FALSE,
                                       strict.var.names = FALSE,
                                       allow.single.categories = TRUE)
{
    input.formula <- formula # To work past scoping issues in car package: https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf.

    if (!is.null(subset))
    {
        if (is.null(subset.description) | (class(subset.description) == "try-error") | !is.null(attr(subset, "name"))
            | length(subset.description) > 0)
            subset.description <- Labels(subset)
        if (is.null(attr(subset, "name")))
            attr(subset, "name") <- subset.description
    }

    if(!is.null(weights))
    {
        if (is.null(weights.description) | (class(weights.description) == "try-error") | !is.null(attr(weights, "name"))
            | length(weights.description) > 0)
            weights.description <- Labels(weights)
        if (is.null(attr(weights, "name")))
            attr(weights, "name") <- weights.description
    }

    data <- GetData(input.formula, data, auxiliary.data = NULL)

    # randomForest fails with data when variable names contain "$" even if surrounded by backticks
    if (strict.var.names)
    {
        vars <- AllVariablesNames(input.formula, data = data)
        vars <- gsub("\\`", "", vars)   # remove backticks
        vars <- gsub("[$ ]", ".", vars) # replace dollar and space with dot
        input.formula <- formula(paste0(vars[1], "~", paste0(vars[-1], collapse = "+")))
        colnames(data) <- gsub("\\`", "", colnames(data))
        colnames(data) <- gsub("[$ ]", ".", colnames(data))
    }

    row.names <- rownames(data)
    outcome.name <- OutcomeName(input.formula, data)
    outcome.i <- match(outcome.name, names(data))

    if (dummy)
    {
        factor.levels <- sapply(data[, -outcome.i], levels)
        # convert factors with N levels to N-1 binary variables, used by LDA
        data <- cbind(data[outcome.i], AsNumeric(data[, -outcome.i, drop = FALSE], binary = TRUE, remove.first = TRUE))
        # remove constant variables caused by unpopulated levels
        ErrorIfInfinity(data)
        data <- data[, c(TRUE, sapply(data[, -1, drop = FALSE], var, na.rm = TRUE) != 0)]
        attr(data, "factor.levels") <- factor.levels
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
    predictor.names <- colnames(data)[colnames(data) != outcome.name]
    form <- as.formula(paste(outcome.name, "~",
                             paste(predictor.names[predictor.names != outcome.name], collapse = "+")))
    processed.data <- EstimationData(form, data, subset, weights, missing, seed = seed)
    unweighted.training.data <- processed.data$estimation.data
    ErrorIfInfinity(unweighted.training.data)

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

    level.counts <- sapply(weighted.training.data, function(x) length(levels(x)))
    if (!allow.single.categories && any(level.counts == 1))
    {
        stop("Categorical predictors must have more than one category, after applying any ",
                "filter, weights and missing data treatment. This is not the case for: ",
                paste(names(level.counts)[level.counts == 1], collapse = ", "),
                ". Please remove those variables to proceed.")
        weighted.training.data <- weighted.training.data[, level.counts != 1]
    }

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
                input.formula = input.formula,
                subset.missing.removed = processed.data$post.missing.data.estimation.sample))
}

#' Save standard data after fitting a machine learning model
#'
#' @importFrom flipU IsCount
#' @noRd
saveMachineLearningResults <- function(result, prepared.data, show.labels)
{
    # Save data, subset and weights
    result$model <- prepared.data$required.data
    result$subset <- prepared.data$subset.missing.removed # original subset with missing data removed
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
    result$confusion <- ConfusionMatrix(result, result$subset, prepared.data$unfiltered.weights)

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


# Compute performance metrics for a model with numeric outcome.
# Calculation uses cases when both pred and obs are not NA.
# The alternative definition of r^2 as cor(obs, pred)^2 is
#   only valid when optimizing sum of squared errors.
# https://stats.stackexchange.com/questions/83826/is-a-weighted-r2-in-robust-linear-model-meaningful-for-goodness-of-fit-analys
numericOutcomeMetrics <- function(obs, pred, weights) {

    obs.and.pred <- complete.cases(obs, pred)
    if (sum(obs.and.pred) == 0)
        return(list(rmse = NA, r.squared = NA))

    if (is.null(weights))
        weights <- rep(1, length(obs))

    obs <- obs[obs.and.pred]
    pred <- pred[obs.and.pred]
    weights <- weights[obs.and.pred]

    sse <- sum(weights * (obs - pred)^2)
    rmse <- sqrt(sse / sum(weights))

    weighted.mean <- sum(obs * weights) / sum(weights)
    sst <- sum(weights * (obs - weighted.mean)^2)
    r.sq <- 1 - sse /sst
    if (r.sq < 0)
        r.sq <- NA

    return(list(rmse = rmse, r.squared = r.sq))
}

