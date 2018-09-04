#' Create an ensemble of MachineLearning and/or Regression models
#'
#' @param models A \code{list} of models, all of which are of class
#'     \code{MachineLearning} or \code{Regression}.
#' @param compare.only Logical; whether to just produce a table comparing the models or
#'     additionally combine them to make a new ensemble model.
#' @param evaluation.filter An optional vector specifying a subset of observations to be
#'     used for evaluating the models.
#' @param output If \code{compare.only} is \code{FALSE}, one of \code{"Comparison"} which
#'     produces a table comparing the models, or \code{"Ensemble"} which produces a
#'     \code{\link{ConfusionMatrix}}.
#' @importFrom flipFormat Labels
#' @importFrom flipData Probabilities
#' @export
MachineLearningEnsemble <- function(models,
                           compare.only = FALSE,
                           evaluation.filter = NULL,
                           output = "Comparison") {

    n.models <- length(models)
    if (n.models <= 1)
        stop("At least 2 models are required to create an ensemble.")

    # Test that models are of the same class and outcome. Not necessary to use same data
    # unless evaluation.filter is specified.
    checkModelsComparable(models, evaluation.filter)
    comparison <- data.frame(t(sapply(models, class.and.type)), stringsAsFactors = FALSE)

    statistic.names <- c("Underlying model", "Model type")

    numeric.outcomes <- sapply(models, has.numeric.outcome)
    if (length(unique(numeric.outcomes)) != 1)
        stop("Outcomes must be all either numeric or categorical to comapre models.")
    numeric.outcome <- numeric.outcomes[[1]]

    if (numeric.outcome)
    {
        perfomances <- sapply(models, numericPerformance, evaluation.filter)
        comparison <- cbind(comparison, t(perfomances))
        colnames(comparison) <- c(statistic.names, c("Training RMSE", "Evaluation RMSE",
                                                     "Training R^2", "Evaluation R^2"))
        if (all(is.na(comparison$`Evaluation R^2`)))
            comparison$`Evaluation R^2` <- comparison$`Evaluation RMSE` <- NULL
    }
    else
    {
        perfomances <- sapply(models, categoricalPerformance, evaluation.filter)
        comparison <- cbind(comparison, t(perfomances))
        colnames(comparison) <- c(statistic.names, c("Training accuracy", "Evaluation accuracy"))
        if (all(is.na(comparison$`Evaluation accuracy`)))
            comparison$`Evaluation accuracy` <- NULL
    }

    rownames(comparison) <- paste("Model", seq(n.models))

    result <- list()
    class(result) <- c("MachineLearningEnsemble", class(result))

    if (!compare.only)
    {
        comparison <- rbind(comparison, rep(NA, ncol(comparison)), stringsAsFactors = FALSE)
        #comparison$training.accuracy[n.models + 1] <- result$training.accuracy
        #comparison$evaluation.accuracy[n.models + 1] <- result$evaluation.accuracy
        comparison[["Underlying model"]][n.models + 1] <- "Ensemble"
        rownames(comparison)[n.models + 1] <- "Ensemble"

        common <-  extractCommonData(models)
        outcome <- common$outcome
        subset <- common$subset
        weights <- common$weights

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
        result$subset <- subset
        result$weights <- weights
        result$confusion <- ConfusionMatrix(result, subset, weights)

        if (numeric.outcome) {
            comparison[["Training RMSE"]][n.models + 1] <- result$training.rmse <- rmse(outcome[subset], result$prediction[subset])
            if (!is.null(evaluation.filter))
                comparison[["Evaluation RMSE"]][n.models + 1] <- result$evaluation.rmse <- rmse(outcome[evaluation.filter], result$prediction[evaluation.filter])
            comparison[["Training R^2"]][n.models + 1] <- result$training.r.squared <- r.squared(outcome[subset], result$prediction[subset])
            if (!is.null(evaluation.filter))
                comparison[["Evaluation R^2"]][n.models + 1] <- result$evaluation.r.squared <- r.squared(outcome[evaluation.filter], result$prediction[evaluation.filter])
            comparison[["Model type"]][n.models + 1] <- "Average"
        }
        else
        {
            comparison[["Training accuracy"]][n.models + 1] <- result$training.accuracy <- attr(result$confusion, "accuracy")
            if (!is.null(evaluation.filter))
                comparison[["Evaluation accuracy"]][n.models + 1] <- result$evaluation.accuracy <- attr(ConfusionMatrix(result, subset = evaluation.filter), "accuracy")
            comparison[["Model type"]][n.models + 1] <- "Average probabilities"
        }
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

# stats below are calculated when both pred and obs are not NA
rmse <- function(obs, pred) sqrt(mean((obs - pred)^2, na.rm = TRUE))
r.squared <- function(obs, pred) {
    obs.and.pred <- complete.cases(obs, pred)
    obs <- obs[obs.and.pred]
    pred <- pred[obs.and.pred]
    1 - (sum((obs - pred)^2) / sum((obs - mean(obs))^2))
}
#r.squared <- function(obs, pred) cor(obs, pred, use = "complete.obs")^2

numericPerformance <- function(x, evaluation.filter) {

    # Use the same weights as fitting the model
    weights <- if (is.null(x$weights)) rep(1, nrow(x$model)) else x$weights
    pred <- predict(x) * weights
    obs <- Observed(x) * weights
    training.pred <- pred[x$subset]
    training.obs <- obs[x$subset]
    evaluation.pred <- pred[evaluation.filter]
    evaluation.obs <- obs[evaluation.filter]

    result <- c(rmse(training.obs, training.pred),
                if (is.null(evaluation.filter)) NA else rmse(evaluation.obs, evaluation.pred),
                r.squared(training.obs, training.pred),
                if (is.null(evaluation.filter)) NA else r.squared(evaluation.obs, evaluation.pred))
    names(result) <- c("training.rmse", "evaluation.rmse",
                       "training.r.squared", "evaluation.r.squared")
    return(result)
}

categoricalPerformance <- function(x, evaluation.filter) {

    training.accuracy <- attr(ConfusionMatrix(x), "accuracy")
    evaluation.accuracy <- attr(ConfusionMatrix(x, subset = evaluation.filter), "accuracy")
    result <- c(training.accuracy,
                if (is.null(evaluation.filter)) NA else evaluation.accuracy)
    names(result) <- c("training.accuracy", "evaluation.accuracy")
    return(result)
}

checkModelsComparable <- function(models, evaluation.filter) {

    valid.classes <- sapply(models, function(model) (any(c("Regression", "MachineLearning") %in% class(model))))
    if (!all(valid.classes))
        stop("Ensemble requires all models to be MachineLearning or Regression.")

    outcome.names <- sapply(models, function(model) model$outcome.name)
    if (length(unique(outcome.names)) != 1)
        stop("All models must predict the same outcome but they do not.")

    if (!is.null(evaluation.filter))
    {
        data.lengths <- c(sapply(models, function(model) length(model$subset)),
                          length(evaluation.filter))
        if (length(unique(data.lengths)) != 1)
            stop("Lengths of evaluation filter and input data for each model must be the same.")
    }
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
        x$confusion
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

