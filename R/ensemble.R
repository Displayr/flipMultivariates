#' Create an ensemble of MachineLearning and/or Regression models
#'
#' @param models A \code{list} of models, all of which are of class
#'     \code{MachineLearning} or \code{Regression}.
#' @param compare.only Logical; whether to just produce a table comparing the models or
#'     additionally combine them to make a new ensemble model.
#' @param output If \code{compare.only} is \code{FALSE}, one of \code{"Comparison"} which
#'     produces a table comparing the models, or \code{"Ensemble"} which produces a
#'     \code{\link{ConfusionMatrix}}.
#' @importFrom flipFormat Labels
#' @importFrom flipData Probabilities
#' @export
MachineLearningEnsemble <- function(models,
                           compare.only = FALSE,
                           output = "Comparison") {

    n.models <- length(models)
    if (n.models <= 1)
        stop("At least 2 models are required to create an ensemble.")

    # loose test that models are of the same class and outcome, not necessary to use same data
    checkModelsComparable(models)
    comparison <- data.frame(t(sapply(models, class.and.type)), stringsAsFactors = FALSE)

    statistic.names <- c("Underlying model", "Model type")

    numeric.outcomes <- sapply(models, has.numeric.outcome)
    if (length(unique(numeric.outcomes)) != 1)
        stop("Outcomes must be all either numeric or categorical to comapre models.")
    numeric.outcome <- numeric.outcomes[[1]]

    if (numeric.outcome)
    {
        perfomances <- sapply(models, numericPerformance)
        comparison <- cbind(comparison, t(perfomances))
        colnames(comparison) <- c(statistic.names, c("In-sample RMSE", "Out-sample RMSE",
                                                     "In-sample R^2", "Out-sample R^2"))
        if (all(is.na(comparison$`Out-sample R^2`)))
            comparison$`Out-sample R^2` <- comparison$`Out-sample RMSE` <- NULL
    }
    else
    {
        perfomances <- sapply(models, categoricalPerformance)
        comparison <- cbind(comparison, t(perfomances))
        colnames(comparison) <- c(statistic.names, c("In-sample accuracy", "Out-sample accuracy"))
        if (all(is.na(comparison$`Out-sample accuracy`)))
            comparison$`Out-sample accuracy` <- NULL
    }

    rownames(comparison) <- paste("Model", seq(n.models))

    result <- list()
    class(result) <- c("MachineLearningEnsemble", class(result))

    if (!compare.only)
    {
        comparison <- rbind(comparison, rep(NA, ncol(comparison)), stringsAsFactors = FALSE)
        comparison$in.sample.accuracy[n.models + 1] <- result$in.sample.accuracy
        comparison$out.sample.accuracy[n.models + 1] <- result$out.sample.accuracy
        comparison[["Underlying model"]][n.models + 1] <- "Ensemble"
        rownames(comparison)[n.models + 1] <- "Ensemble"

        common <-  extractCommonData(models)
        outcome <- common$outcome   # DO NOT NEED
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
            comparison[["In-sample RMSE"]][n.models + 1] <- result$in.sample.rmse <- rmse(outcome[subset], result$prediction[subset])
            if (!all(subset))
                comparison[["Out-sample RMSE"]][n.models + 1] <- result$out.sample.rmse <- rmse(outcome[!subset], result$prediction[!subset])
            comparison[["In-sample R^2"]][n.models + 1] <- result$in.sample.r.squared <- r.squared(outcome[subset], result$prediction[subset])
            if (!all(subset))
                comparison[["Out-sample R^2"]][n.models + 1] <- result$out.sample.r.squared <- r.squared(outcome[!subset], result$prediction[!subset])
            comparison[["Model type"]][n.models + 1] <- "Average"
        }
        else
        {
            comparison[["In-sample accuracy"]][n.models + 1] <- result$in.sample.accuracy <- attr(result$confusion, "accuracy")
            if (!all(subset))
                comparison[["Out-sample accuracy"]][n.models + 1] <- result$out.sample.accuracy <- attr(ConfusionMatrix(result, subset = !subset), "accuracy")
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

rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
r.squared <- function(obs, pred) 1 - (sum((obs - pred)^2) / sum((obs - mean(obs))^2))

numericPerformance <- function(x) {

    # Use the same weights as fitting the model
    weights <- if (is.null(x$weights)) rep(1, x$n.observations) else x$weights
    pred <- predict(x) * weights
    obs <- Observed(x) * weights
    in.pred <- pred[x$subset]
    in.obs <- obs[x$subset]
    out.pred <- pred[!x$subset]
    out.obs <- obs[!x$subset]

    result <- c(rmse(in.obs, in.pred), rmse(out.obs, out.pred),
                r.squared(in.obs, in.pred), r.squared(out.obs, out.pred))
    names(result) <- c("in.sample.rmse", "out.sample.rmse",
                       "in.sample.r.squared", "out.sample.r.squared")
    return(result)
}

categoricalPerformance <- function(x) {

    in.sample.accuracy <- attr(ConfusionMatrix(x), "accuracy")
    out.sample.accuracy <- attr(ConfusionMatrix(x, subset = !x$subset), "accuracy")
    result <- c(in.sample.accuracy, out.sample.accuracy)
    names(result) <- c("in.sample.accuracy", "out.sample.accuracy")
    return(result)
}

checkModelsComparable <- function(models) {

    valid.classes <- sapply(models, function(model) (any(c("Regression", "MachineLearning") %in% class(model))))
    if (!all(valid.classes))
        stop("Ensemble requires all models to be MachineLearning or Regression.")

    outcome.names <- sapply(models, function(model) model$outcome.name)
    if (length(unique(outcome.names)) != 1)
        stop("All models must predict the same outcome but they do not.")

}

extractCommonData <- function(models) {

    outcomes <- unique(lapply(models, Observed))
    if (length(outcomes) != 1)
        stop("Models must have the same outcome variable.")
    subsets <- unique(lapply(models, function(x) x$subset))
    if (length(subsets) != 1)
        stop("Models must have the same filter.")
    weights <- unique(lapply(models, function(x) x$weights))
    if (length(weights) != 1)
        stop("Models must have the same weights.")
    formulae <- unique(lapply(models, function(x) {form <- x$formula
                                                   attributes(form) <- NULL # remove environment
                                                   form}))
    if (length(formulae) != 1)
        stop("Models must have the same formula specifying predictor and outcome variables.")
    #data <- unique(lapply(models, function(x) x$model))
    #if (length(data) != 1)
    #    stop("Models must have the same training data.")

    return(list(outcome = outcomes[[1]], subset = subsets[[1]],
                weights = weights[[1]], formula = formulae[[1]]))
                #data = data[[1]]))
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

