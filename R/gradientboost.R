#' Fit a gradient boosted ensemble of underlying tree or regression models
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
#' @param output One of \code{"Accuracy"}, \code{"Importance"},
#'   \code{"Prediction-Accuracy Table"} or \code{"Detail"}.
#' @param missing How missing data is to be treated. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"}, or
#'   \code{"Imputation (replace missing values with estimates)"}.
#' @param booster Whether the underlying model is a tree or linear regression. Options:
#'   \code{"gbtree"},
#'   \code{"gblinear"}.
#' @param grid.search Whether to search the parameter space in order to tune the model.
#' @param seed The random number seed.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#'
#' @importFrom xgboost xgboost xgb.cv
#' @importFrom flipTransformations OneHot
#' @aliases GradientBoosting
#' @export
GradientBoost <- GradientBoosting <- function(formula,
                                 data = NULL,
                                 subset = NULL,
                                 weights = NULL,
                                 output = "Accuracy",
                                 missing  = "Exclude cases with missing data",
                                 booster = "gbtree",
                                 grid.search = FALSE,
                                 seed = 12321,
                                 show.labels = FALSE)
{
    ####################################################################
    ##### Error checking specific to this function                ######
    ####################################################################

    if (booster == "gblinear" && output == "Importance")
        stop("Importance is only available for gbtree booster.") # https://github.com/dmlc/xgboost/issues/2331

    ####################################################################
    ##### Reading in the data and doing some basic tidying        ######
    ####################################################################

    # Identify whether subset and weights are variables in the environment or in data.
    subset.description <- try(deparse(substitute(subset)), silent = TRUE)
    subset <- eval(substitute(subset), data, parent.frame())
    weights.description <- try(deparse(substitute(weights)), silent = TRUE)
    weights <- eval(substitute(weights), data, parent.frame())

    prepared.data <- prepareMachineLearningData(formula, data, subset, subset.description,
                                                weights, weights.description, missing, seed)

    unweighted.training.data <- prepared.data$unweighted.training.data
    weighted.training.data <- prepared.data$weighted.training.data

    ####################################################################
    ##### Processing specific to this function                    ######
    ####################################################################

    numeric.data <- OneHot(weighted.training.data, prepared.data$outcome.name)
    n.class <- 1

    if (prepared.data$numeric.outcome)
    {
        objective <- "reg:linear"
        xval.metric <- "test_rmse_mean"
    }
    else if (length(numeric.data$outcome.levels) == 2)
    {
        objective <- "binary:logistic"
        xval.metric <- "test_error_mean"
    }
    else
    {
        objective <- "multi:softprob"
        n.class <- length(numeric.data$outcome.levels)
        xval.metric <- "test_merror_mean"
    }

    ####################################################################
    ##### Fitting the model. Ideally, this should be a call to     #####
    ##### another function, with the output of that function       #####
    ##### called 'original'.                                       #####
    ####################################################################
    set.seed(seed)

    if (booster == "gbtree")
        params.default <- list(booster = booster, objective = objective, num_class = n.class,
                        max_depth = 6, eta = 0.3, gamma = 0, subsample = 1, colsample_bytree = 1,
                        alpha = 0, lambda = 1)
    else
        params.default <- list(booster = booster, objective = objective, num_class = n.class,
                        lambda = 0, alpha = 0, lambda_bias = 0, nthread = 1)

    if (!grid.search)
    {
        xval <- xgb.cv(data = numeric.data$X, label = numeric.data$y, nrounds = 1000, nfold = 10,
                       params = params.default, early_stopping_rounds = 8, maximize = FALSE, verbose = 0)
        best.rounds <- which.min(xval$evaluation_log[, xval.metric])
        result <- list(original = xgboost(data = numeric.data$X, label = numeric.data$y, params = params.default,
                                          save_period = NULL, nrounds = best.rounds, verbose = 0))
    }
    else
    {
        cross.validate <- function(variable.params, all.params, seed){

            for (param in names(variable.params))
                all.params[param] <- variable.params[[param]]

            xgbcv <- xgb.cv(data = numeric.data$X, label = numeric.data$y, params = all.params,
                            nfold = cv.nfold, nrounds = n.rounds,
                            verbose = 0, early_stopping_rounds = 8, maximize = FALSE, seed = seed)

            return(c(min.error = min(xgbcv$evaluation_log[, xval.metric]), rounds = xgbcv$best_iteration, all.params))
        }

        n.rounds <- 1000
        cv.nfold <- 5

        if (booster == "gbtree")
            search.grid <- expand.grid(subsample = c(0.7, 1),
                                       colsample_bytree = c(0.7, 1),
                                       eta = c(0.1, 0.2, 0.3),
                                       max_depth = c(4, 6, 9))
        else
            search.grid <- expand.grid(lambda = c(0, 0.1, 1),
                                       alpha = c(0, 0.1, 1),
                                       lambda_bias = c(0))

        search.results <- apply(search.grid, 1, cross.validate, params.default, seed)

        search.results <- do.call(rbind.data.frame, c(search.results, stringsAsFactors = FALSE))
        best.index <- which.min(search.results[, "min.error"])
        best.error.rounds <- search.results[best.index, "rounds"]
        best.param <- search.results[best.index, -(1:2)]
        set.seed(seed)      # reset seed after searching
        result <- list(original = xgboost(data = numeric.data$X,
                                          label = numeric.data$y,
                                          verbose = 0,
                                          params = best.param,
                                          save_period = NULL,
                                          nrounds = best.error.rounds))
    }

    ####################################################################
    ##### Saving direct input parameters                           #####
    ####################################################################

    result$original$call <- match.call()
    result$output <- output
    result$missing <- missing
    class(result) <- c("GradientBoost", class(result))

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
#' @importFrom xgboost xgb.importance xgb.ggplot.importance
#' @importFrom ggplot2 ggtitle
#' @export
#' @method print GradientBoost
print.GradientBoost <- function(x, ...)
{
    if (x$output == "Accuracy")
    {
        title <- paste0("Gradient Boosting: ", x$outcome.label)
        predictors <- x$variable.labels
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
    else if (x$output == "Importance")
    {
        importance <- xgb.importance(feature_names = x$prediction.columns, model = x$original)
        #xgb.plot.importance(importance, rel_to_first = TRUE, xlab = "Relative importance") base graphics plot
        gg <- xgb.ggplot.importance(importance, rel_to_first = TRUE, top_n = 10)
        print(gg + ggtitle(paste0("Importance: ", x$outcome.label)))
    }
    else
    {
        x$original$call <- x$formula
        print(x$original)
        invisible(x)
    }
}
