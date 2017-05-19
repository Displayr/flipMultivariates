#' \code{GradientBoost}
#'
#' GradientBoost
#' @param formula A formula of the form \code{groups ~ x1 + x2 + ...}
#' That is, the response is the grouping factor and the right hand side
#' specifies the (non-factor) discriminators, and any transformations, interactions,
#' or other non-additive operators will be ignored.
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
#'   \code{"Exclude cases with missing data"}.
#' @param booster Whether the underlying model is a tree or linear regression. Options:
#'   \code{"gbtree"},
#'   \code{"gblinear"}.
#' @param grid.search Whether to search the parameter space in order to tune the model.
#' @param seed The random number seed used in imputation.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @importFrom flipData GetData EstimationData DataFormula
#' @importFrom flipFormat Labels
#' @importFrom flipU OutcomeName
#' @importFrom xgboost xgboost xgb.cv
#' @importFrom stats runif
#' @importFrom flipTransformations AdjustDataToReflectWeights OneHot
#' @export
GradientBoost <- function(formula,
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
    processed.data <- EstimationData(input.formula, data, subset, weights, missing, seed = seed)
    unfiltered.weights <- processed.data$unfiltered.weights
    .estimation.data <- processed.data$estimation.data
    n.predictors <- ncol(.estimation.data)
    n <- nrow(.estimation.data)
    if (n < ncol(.estimation.data) + 1)
        stop("The sample size is too small for it to be possible to conduct the analysis.")
    .weights <- processed.data$weights
    .formula <- DataFormula(input.formula)

    # Resampling to generate a weighted sample, if necessary.
    .estimation.data.1 <- if (is.null(weights))
        .estimation.data
    else
        AdjustDataToReflectWeights(.estimation.data, .weights)

    numeric.data <- OneHot(.estimation.data.1, outcome.name)
    n.class <- 1

    if (numeric.outcome)
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
                        lambda = 0, alpha = 0, lambda_bias = 0)


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

        search.results <- do.call(rbind.data.frame, search.results)
        best.index <- which.min(search.results[, "min.error"])
        best.error.rounds <- search.results[best.index, "rounds"]
        best.param <- search.results[best.index, -(1:2)]
        set.seed(seed)      # reset seed after searching
        result <- list(original = xgboost(data = numeric.data$X, label = numeric.data$y, verbose = 0,
                                          params = best.param, save_period = NULL, nrounds = best.error.rounds))
    }
    result$original$call <- cl

    ####################################################################
    ##### Saving results, parameters, and tidying up               #####
    ####################################################################
    # 1. Saving data.
    result$subset <- subset <- row.names %in% rownames(.estimation.data)
    result$weights <- unfiltered.weights
    result$model <- data

    # 2. Saving descriptive information.
    class(result) <- "GradientBoost"
    result$outcome.name <- outcome.name
    result$sample.description <- processed.data$description
    result$n.observations <- n
    result$estimation.data <- .estimation.data
    result$numeric.outcome <- numeric.outcome
    result$outcome.levels <- numeric.data$outcome.levels
    result$prediction.columns <- colnames(numeric.data$X)

    # 3. Replacing names with labels
    if (result$show.labels <- show.labels)
    {
        result$outcome.label <- outcome.label
        result$variable.labels <- variable.labels <- variable.labels[-outcome.i]
    }
    else
        result$outcome.label <- outcome.name

    # 4. Saving parameters and confusion matrix
    result$formula <- input.formula
    result$output <- output
    result$missing <- missing
    result$confusion <- ConfusionMatrix(result, subset, unfiltered.weights)
    result
}

#' @importFrom flipFormat DeepLearningTable FormatWithDecimals ExtractCommonPrefix
#' @importFrom flipData GetTidyTwoDimensionalArray Observed
#' @importFrom flipU IsCount
#' @importFrom utils read.table
#' @importFrom xgboost xgb.importance xgb.plot.importance
#' @export
print.GradientBoost <- function(x, ...)
{
    if (x$output == "Accuracy")
    {
        title <- paste0("Gradient Boost: ", x$outcome.label)
        if (x$show.labels)
        {
            predictors <- x$variable.labels
        }
        else
        {
            predictors <- all.vars(x$formula[[3]])
        }

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
            obs <- Observed(x)[x$subset == TRUE]    # subset also accounts for NAs
            pred <- predict(x)[x$subset == TRUE]
            rmse <- sqrt(mean((pred - obs)^2))
            rsq <- (cor(pred, obs))^2
            subtitle <- "Measure of fit"
            tbl <- DeepLearningTable(c("Root Mean Squared Error" = rmse, "R-squared" = rsq),
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
        xgb.plot.importance(importance, rel_to_first = TRUE, xlab = "Relative importance")
    }
    else
    {
        print(x$original)
        invisible(x)
    }
}


