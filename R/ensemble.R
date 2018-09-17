#' Create an ensemble of MachineLearning and/or Regression models
#'
#' @param models A \code{list} of models, all of which are of class
#'     \code{MachineLearning} or \code{Regression}.
#' @param compare.only Logical; whether to just produce a table comparing the models or
#'     additionally combine them to make a new ensemble model.
#' @param evaluation.subset An optional vector specifying a subset of observations to be
#'     used for evaluating the models. If not specified, models will only be compared on the
#'     training data. To evaluate on the whole sample, a subset must still be specified.
#' @param evaluation.weights An optional vector of weights to be used for evaluating the models.
#'     Ignored if no evaluation.subset is supplied. A warning is given if these differ from the
#'     training weights.
#' @param output If \code{compare.only} is \code{FALSE}, one of \code{"Comparison"} which
#'     produces a table comparing the models, or \code{"Ensemble"} which produces a
#'     \code{\link{ConfusionMatrix}}.
#' @importFrom flipFormat Labels
#' @importFrom flipData Probabilities
#' @export
MachineLearningEnsemble <- function(models,
                           compare.only = FALSE,
                           evaluation.subset = NULL,
                           evaluation.weights = NULL,
                           output = "Comparison") {

    n.models <- length(models)
    if (n.models <= 1)
        stop("At least 2 models are required to create an ensemble.")

    # Treat TRUE filter as NULL, i.e., no evaluation.subset statistics are calculated.
    if (!is.null(evaluation.subset) && length(evaluation.subset) == 1 && evaluation.subset == TRUE)
        evaluation.subset <- NULL

    # Test that models are of the same class and outcome. Not necessary to use same data
    # unless evaluation.subset and/or weights are specified.
    numeric.outcome <- checkModelsComparable(models, evaluation.subset, evaluation.weights)
    comparison <- data.frame(t(sapply(models, class.and.type)), stringsAsFactors = FALSE)
    rownames(comparison) <- paste("Model", seq(n.models))

    result <- list()
    class(result) <- c("MachineLearningEnsemble", class(result))

    if (!compare.only)
    {
        ensemble.type <- if (numeric.outcome) "Average" else "Average probabilities"
        comparison <- rbind(comparison, c("Ensemble", ensemble.type), stringsAsFactors = FALSE)
        rownames(comparison)[n.models + 1] <- "Ensemble"

        common <-  extractCommonData(models)
        outcome <- common$outcome
        subset <- common$subset

        if (numeric.outcome) {
            result$prediction <- rowMeans(sapply(models, predict))
        }
        else
        {
            probabilities <- Reduce("+", lapply(models, Probabilities)) / n.models
            result$prediction <- as.factor(colnames(probabilities)[max.col(probabilities, ties.method = "first")])
            result$probabilities <- probabilities
        }

        result$outcome <- outcome
        result$outcome.label <- models[[1]]$outcome.label
        result$sample.description <- models[[1]]$sample.description
        result$subset <- subset          # subsets used to fit the models
        result$weights <- common$weights # weights used to fit the models
        result$confusion <- ConfusionMatrix(result) # for the fitted data
        models$ensemble <- result
    }

    if (!is.null(evaluation.subset))
    {
        if (!identical(evaluation.weights, models[[1]]$weights))
            warning("Weights used for training the models differ from evaluation weights.")
    }
    else if (!is.null(evaluation.weights))
        warning("Weights will hve no effect because a filter to evaluate the models was not specified.")

    statistic.names <- c("Underlying model", "Model type")
    if (numeric.outcome)
    {
        perfomances <- sapply(models, numeric.performance, evaluation.subset, evaluation.weights)
        comparison <- cbind(comparison, t(perfomances))
        colnames(comparison) <- c(statistic.names, c("Training RMSE", "Evaluation RMSE",
                                                     "Training R^2", "Evaluation R^2"))
        if (all(is.na(comparison$`Evaluation R^2`)))
            comparison$`Evaluation R^2` <- comparison$`Evaluation RMSE` <- NULL
    }
    else
    {
        perfomances <- sapply(models, categorical.performance, evaluation.subset, evaluation.weights)
        comparison <- cbind(comparison, t(perfomances))
        colnames(comparison) <- c(statistic.names, c("Training accuracy", "Evaluation accuracy"))
        if (all(is.na(comparison$`Evaluation accuracy`)))
            comparison$`Evaluation accuracy` <- NULL
    }

    result$numeric.outcome <- numeric.outcome
    result$comparison <- comparison
    result$ensemble <- !compare.only
    result$n.models <- n.models
    result$output <- output
    return(result)
}

######### Helper functions #########

class.and.type <- function(model)
{
    model.class <- setdiff(class(model), c("list", "MachineLearning"))[1]
    model.type <- switch(model.class,
                         "Regression" = model$type,
                         "SupportVectorMachine" = paste0("cost = ", model$original$cost),
                         "RandomForest" = paste0("trees = ", model$original$ntree),
                         "GradientBoost" = paste0("booster = ", model$original$params$booster),
                         "DeepLearning" = paste0("hidden nodes = ", paste(model$hidden.nodes, collapse = ", ")),
                         "NA")
    result <- c(model.class, model.type)
    names(result) <- c("model.class", "model.type")
    return(result)
}

has.numeric.outcome <- function(x) {
    if ("Regression" %in% class(x))
        x$type == "Linear"
    else
        x$numeric.outcome
}


numeric.performance <- function(x, evaluation.subset, evaluation.weights) {

    pred <- predict(x)
    obs <- Observed(x)

    training.pred <- pred[x$subset]
    training.obs <- obs[x$subset]
    evaluation.pred <- pred[evaluation.subset]
    evaluation.obs <- obs[evaluation.subset]

    training.metrics <- numeric.outcome.metrics(training.obs, training.pred,
                                                evaluation.weights[x$subset])
    evaluation.metrics <- numeric.outcome.metrics(evaluation.obs, evaluation.pred,
                                                  evaluation.weights[evaluation.subset])

    result <- c(training.metrics$rmse, evaluation.metrics$rmse,
                training.metrics$r.squared, evaluation.metrics$r.squared)
    names(result) <- c("training.rmse", "evaluation.rmse",
                       "training.r.squared", "evaluation.r.squared")
    return(result)
}

#' @importFrom flipRegression Accuracy
categorical.performance <- function(x, evaluation.subset, evaluation.weights) {

    training.accuracy <- if (!is.null(x$confusion))
        attr(x$confusion, "accuracy")
    else
        Accuracy(x, subset = x$subset)

    evaluation.accuracy <- if (is.null(evaluation.subset))
        NA
    else
        suppressWarnings(Accuracy(x, evaluation.subset, evaluation.weights))

    result <- c(training.accuracy, evaluation.accuracy)
    names(result) <- c("training.accuracy", "evaluation.accuracy")
    return(result)
}

checkModelsComparable <- function(models, evaluation.subset, evaluation.weights) {

    numeric.outcomes <- sapply(models, has.numeric.outcome)
    if (length(unique(numeric.outcomes)) != 1)
        stop("Outcomes must be all either numeric or categorical to comapre models.")

    valid.classes <- sapply(models, function(model) (any(c("Regression", "MachineLearning") %in% class(model))))
    if (!all(valid.classes))
        stop("Ensemble requires all models to be MachineLearning or Regression.")

    outcome.names <- sapply(models, function(model) model$outcome.name)
    if (length(unique(outcome.names)) != 1)
        stop("All models must predict the same outcome but they do not.")

    data.lengths <- sapply(models, function(m) nrow(m$model))
    if (!is.null(evaluation.subset) && length(unique(c(length(evaluation.subset), data.lengths))) != 1)
            stop("Filter must be the same length as the input data for each model, but is not.")
    if (!is.null(evaluation.weights) && length(unique(c(length(evaluation.weights), data.lengths))) != 1)
        stop("Weights must be the same length as the input data for each model, but is not.")

    return(numeric.outcomes[[1]])
}

extractCommonData <- function(models) {

    outcomes <- unique(lapply(models, Observed))
    if (length(outcomes) != 1)
        stop("Models must have the same outcome variable.")
    subsets <- unique(lapply(models, function(x) x$subset))
    if (length(subsets) != 1)
        stop("Models must have the same filter and missing data cases.")
    weights <- unique(lapply(models, function(x) x$weights))
    if (length(weights) != 1)
        stop("Models must have the same weights.")

    return(list(outcome = outcomes[[1]], subset = subsets[[1]],
                weights = weights[[1]]))
}


#' @importFrom flipFormat ComparisonTable
#' @export
print.MachineLearningEnsemble <- function(x, ...) {

    if (x$output == "Ensemble")
    {
        print(x$confusion)
    }
    else
    {
        title <- paste("Comparison of", x$n.models, " models")
        if (x$ensemble)
            title <- paste0(title, " and Ensemble")
        tbl <- ComparisonTable(x$comparison,
                               title = title)
        tbl
    }
}

#' \code{predict.MachineLearningEnsemble}
#'
#' Predicts values for numeric outcomes and group membership for categories based on fitted data
#' before filtering for a \code{MachineLearningEnsemble} object.  A value (which may be NA) is
#' returned for every instance including those with missing data.
#' @param object A \code{MachineLearningEnsemble} object.
#' @param ... Not used.
#' @export
predict.MachineLearningEnsemble <- function(object, ...)
{
    return(object$prediction)
}

#' \code{Probabilities.MachineLearningEnsemble}
#'
#' Estimates probabilities of group membership for the entire sample passed into the original analysis (including
#' missing and filtered values).
#' @param object A \code{MachineLearningEnsemble} object.
#' @export
Probabilities.MachineLearningEnsemble <- function(object)
{
    if(object$numeric.outcome)
        stop("Probabilities are only applicable to models with categorical outcome variables.")

    return(object$probabilities)
}

