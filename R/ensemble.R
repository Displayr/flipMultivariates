#' Create an ensemble or comparison table of existing MachineLearning
#' and/or Regression models
#'
#' @param models A \code{list} of models, all of which are of class
#'     \code{MachineLearning} or \code{Regression}.
#' @param compare.only Logical; whether to just produce a table comparing the models or
#'     additionally combine them to make a new ensemble model.
#' @param optimal.ensemble Logical; whether to find the ensemble with the best accuracy or
#'     r-squared, calculated on the \code{evaluation.subset} if given, else on the training
#'     data. Ignored if \code{compare.only} is TRUE.
#' @param evaluation.subset An optional vector specifying a subset of observations to be
#'     used for evaluating the models. If not specified, models will only be compared on the
#'     training data. If models are not trained on the whole sample To evaluate on the whole sample,
#'     a subset must still be specified.
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
                           output = "Comparison",
                           optimal.ensemble = FALSE) {

    n.models <- length(models)
    if (n.models <= 1 && !compare.only)
        stop("At least 2 models are required to create an ensemble.")
    if (optimal.ensemble && compare.only)
        stop("Cannot create an optimal ensemble if only performing comparison.")

    # Treat TRUE filter as NULL, i.e., no evaluation.subset statistics are calculated.
    if (!is.null(evaluation.subset))
    {
        if (length(evaluation.subset) == 1 && evaluation.subset == TRUE)
            evaluation.subset <- NULL
        else
            evaluation.subset <- as.logical(evaluation.subset)
    }

    # Test that models are of the same class and outcome. Not necessary to use same data
    # unless evaluation.subset and/or weights are specified.
    numeric.outcome <- checkModelsComparable(models, evaluation.subset, evaluation.weights)
    performance.function <- if (numeric.outcome) numericPerformance else categoricalPerformance
    comparison <- data.frame(t(sapply(models, classAndType)), stringsAsFactors = FALSE)

    rownames(comparison) <- if (is.null(names(models)))
        paste("Model", seq(n.models))
    else
        names(models)


    result <- list()
    class(result) <- c("MachineLearningEnsemble", class(result))

    if (!compare.only)
    {
        comparison <- rbind(comparison, c("Ensemble", "All models"), stringsAsFactors = FALSE)
        rownames(comparison)[n.models + 1] <- "Ensemble"

        common <-  extractCommonData(models)
        outcome <- common$outcome
        subset <- common$subset

        preds.and.probs <- ensemblePredictionsAndProbabilities(models, numeric.outcome)
        result$prediction <- preds.and.probs$prediction
        result$probabilities <- preds.and.probs$probabilities

        result$outcome <- outcome
        result$outcome.label <- models[[1]]$outcome.label
        result$sample.description <- models[[1]]$sample.description
        result$subset <- subset          # subsets used to fit the models
        result$weights <- common$weights # weights used to fit the models
        models$ensemble <- result        # add to list of models for evaluation

        if (optimal.ensemble)
        {
            comparison <- rbind(comparison, c("Ensemble", "Optimal"), stringsAsFactors = FALSE)
            rownames(comparison)[n.models + 2] <- "Optimal Ensemble"

            optimal.performance <- 0
            optimal.models <- NULL

            # Test all combinations of models
            combinations <- 2^n.models - 1
            for (combo in seq(combinations))
            {
                which.models <- c(as.logical(intToBits(combo))[1:n.models], FALSE)
                combo.models <- models[which.models]

                preds.and.probs <- ensemblePredictionsAndProbabilities(combo.models, numeric.outcome)
                result$prediction <- preds.and.probs$prediction

                perf <- performance.function(result, evaluation.subset, evaluation.weights)
                combo.performance <- if(is.null(evaluation.subset)) perf[length(perf) - 1] else perf[length(perf)]
                if (!is.na(combo.performance) && combo.performance > optimal.performance)
                {
                    optimal.performance <- combo.performance
                    optimal.models <- which.models
                }
            }

            result$optimal.models <- c(optimal.models, FALSE)

            # Update prediction and probabilities of result with optimal model
            preds.and.probs <- ensemblePredictionsAndProbabilities(models[optimal.models],
                                                                   numeric.outcome)
            result$prediction <- preds.and.probs$prediction
            result$probabilities <- preds.and.probs$probabilities
            models$optimal.ensemble <- result  # add optimal to list of models for evaluation
        }
        result$confusion <- ConfusionMatrix(result) # for the fitted data
    }

    if (!is.null(evaluation.subset))
    {
        if (!identical(evaluation.weights, models[[1]]$weights))
            warning("Weights used for training the models differ from evaluation weights.")
    }
    else if (!is.null(evaluation.weights))
        warning("Weights will have no effect because a filter to evaluate the models was not specified.")

    statistic.names <- c("Underlying model", "Model type")
    perfomances <- sapply(models, performance.function, evaluation.subset, evaluation.weights)
    comparison <- cbind(comparison, t(perfomances))

    if (numeric.outcome)
    {
        colnames(comparison) <- c(statistic.names, c("Training RMSE", "Evaluation RMSE",
                                                     "Training R^2", "Evaluation R^2"))
        if (all(is.na(comparison$`Evaluation RMSE`)))
            comparison$`Evaluation RMSE` <- comparison$`Evaluation R^2` <- NULL
    }
    else
    {
        colnames(comparison) <- c(statistic.names, c("Training accuracy", "Evaluation accuracy"))
        if (all(is.na(comparison$`Evaluation accuracy`)))
            comparison$`Evaluation accuracy` <- NULL
    }

    result$numeric.outcome <- numeric.outcome
    result$comparison <- comparison
    result$ensemble <- !compare.only
    result$optimal.ensemble <- optimal.ensemble
    result$n.models <- n.models
    result$output <- output
    return(result)
}

######### Helper functions #########

classAndType <- function(model)
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

hasNumericOutcome <- function(x) {
    if ("Regression" %in% class(x))
        x$type == "Linear"
    else
        x$numeric.outcome
}

ensemblePredictionsAndProbabilities <- function(models, numeric.outcome) {

    if (numeric.outcome) {
        probabilities <- NULL
        prediction <- rowMeans(sapply(models, predict))
    }
    else
    {
        probabilities <- Reduce("+", lapply(models, Probabilities)) / length(models)
        prediction <- as.factor(colnames(probabilities)[max.col(probabilities, ties.method = "first")])
    }
    return(list(prediction = prediction, probabilities = probabilities))
}

numericPerformance <- function(x, evaluation.subset, evaluation.weights) {

    pred <- predict(x)
    obs <- Observed(x)

    training.pred <- pred[x$subset]
    training.obs <- obs[x$subset]
    evaluation.pred <- pred[evaluation.subset]
    evaluation.obs <- obs[evaluation.subset]

    training.metrics <- numericOutcomeMetrics(training.obs, training.pred,
                                                evaluation.weights[x$subset])
    evaluation.metrics <- numericOutcomeMetrics(evaluation.obs, evaluation.pred,
                                                  evaluation.weights[evaluation.subset])

    result <- c(training.metrics$rmse, evaluation.metrics$rmse,
                training.metrics$r.squared, evaluation.metrics$r.squared)
    names(result) <- c("training.rmse", "evaluation.rmse",
                       "training.r.squared", "evaluation.r.squared")
    return(result)
}

#' @importFrom flipRegression Accuracy
categoricalPerformance <- function(x, evaluation.subset, evaluation.weights) {

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

    numeric.outcomes <- sapply(models, hasNumericOutcome)
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

        footer <- NULL
        if (x$optimal.ensemble)
        {
            footer <- x$comparison[x$optimal.models, 1, drop = FALSE]
            footer <- cbind(rownames(footer), footer)
            footer <- apply(footer, 1, function(x) paste0(x[1], " (", x[2], ")", collapse = ""))
            footer <- paste0("Optimal ensemble models: ", paste(footer, collapse = ", "))
        }

        tbl <- ComparisonTable(x$comparison,
                               title = title,
                               footer = footer)
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


#' Create an ensemble or comparison table of new MachineLearning and/or Regression models
#'
#' @param formula A formula of the form \code{groups ~ x1 + x2 + ...}
#'   That is, the response is the grouping factor and the right hand side
#'   specifies the (non-factor) discriminators, and any transformations, interactions,
#'   or other non-additive operators apart from \code{.} will be ignored.
#' @param data A \code{\link{data.frame}} from which variables specified
#'   in formula are preferentially to be taken.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression.
#' @param weights An optional vector of sampling weights, or the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param evaluation.subset An optional vector specifying a subset of observations to be
#'   used for evaluating the models. If not specified, models will only be compared on the
#'   training data. If models are not trained on the whole sample To evaluate on the whole sample,
#'   a subset must still be specified.
#' @param missing How missing data is to be treated. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"}, or
#'   \code{"Imputation (replace missing values with estimates)"}.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#'   variables label is an attribute (e.g., attr(foo, "label")).
#' @param seed The random number seed.
#' @param models.args A \code{\link{list}} of lists of arguments to be passed to \code{\link{MachineLearning}}
#'   to create each model.
#' @param compare.only Logical; whether to just produce a table comparing the models or
#'     additionally combine them to make a new ensemble model.
#' @param optimal.ensemble Logical; whether to find the ensemble with the best accuracy or
#'     r-squared, calculated on the \code{evaluation.subset} if given, else on the training
#'     data. Ignored if \code{compare.only} is TRUE.
#' @param output If \code{compare.only} is \code{FALSE}, one of \code{"Comparison"} which
#'     produces a table comparing the models, or \code{"Ensemble"} which produces a
#'     \code{\link{ConfusionMatrix}}.
#' @export
MachineLearningMulti <- function(formula,
                                 data = NULL,
                                 subset = NULL,
                                 weights = NULL,
                                 evaluation.subset = NULL,
                                 missing = "Exclude cases with missing data",
                                 show.labels = FALSE,
                                 seed = 12321,
                                 models.args = NULL,
                                 compare.only = FALSE,
                                 optimal.ensemble = FALSE,
                                 output = "Comparison") {

    n.models <- length(models.args)
    common.args <- list(formula = formula, data = data, weights = weights, subset = subset,
                        missing = missing, show.labels = show.labels, seed = seed)
    fitted.models <- list()

    for (i in seq(n.models))
    {
        model.args <- c(common.args, models.args[[i]])
        fitted.models[[i]] <- do.call(MachineLearning, model.args)
    }

    MachineLearningEnsemble(fitted.models,
                            compare.only = compare.only,
                            optimal.ensemble = optimal.ensemble,
                            evaluation.subset = evaluation.subset,
                            evaluation.weights = if (is.null(evaluation.subset)) NULL else weights,
                            output = output)
}
