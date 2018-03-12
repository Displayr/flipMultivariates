#' Fit a neural network model
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
#' @param output One of \code{"Accuracy"}, \code{"Prediction-Accuracy Table"} or \code{"Detail"}.
#' @param missing How missing data is to be treated. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"},
#' @param seed The random number seed used in imputation.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param ... Other arguments to be supplied to \code{\link{svm}}.
#' @export
DeepLearning <- function(formula,
                                 data = NULL,
                                 subset = NULL,
                                 weights = NULL,
                                 output = "Accuracy",
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

    outcome.name <- OutcomeName(input.formula, data)
    outcome.i <- match(outcome.name, names(data))
    outcome.variable <- data[, outcome.i]
    numeric.outcome <- !is.factor(outcome.variable)
    variable.labels <- if (show.labels) Labels(data) else names(data)
    outcome.label <- variable.labels[outcome.i]
    #if (outcome.label == "data[, outcome.name]")
    #    outcome.label <- outcome.name
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
    set.seed(seed)
    result <- list(original = neuralNetwork(as.matrix(.estimation.data.1[, -outcome.i]),
                                            .estimation.data.1[[outcome.i]],
                                            treat.numeric.as.categorical = TRUE,
                                            iterations = 10))
    #result$original$call <- cl  original is a python object and cannot be modified

    ####################################################################
    ##### Saving results, parameters, and tidying up               #####
    ####################################################################
    # 1. Saving data.
    result$subset <- subset <- row.names %in% rownames(.estimation.data)
    result$weights <- unfiltered.weights
    result$model <- data
    #result$post.missing.data.estimation.sample <- processed.data$post.missing.data.estimation.sample

    # 2. Saving descriptive information.
    class(result) <- c(class(result), "DeepLearning", "MachineLearning")
    result$outcome.name <- outcome.name
    result$sample.description <- processed.data$description
    result$n.observations <- n
    result$estimation.data <- .estimation.data
    result$numeric.outcome <- numeric.outcome

    # 3. Replacing names with labels
    result$show.labels <- show.labels
    result$outcome.label <- outcome.label
    result$variable.labels <- variable.labels <- variable.labels[-outcome.i]

    # 4. Saving parameters and confusion matrix
    result$formula <- input.formula
    result$output <- output
    result$missing <- missing
    result$confusion <- ConfusionMatrix(result, subset, unfiltered.weights)
    result
}


#' @importFrom keras keras_model_sequential optimizer_rmsprop %>% layer_dense layer_dropout to_categorical compile fit
neuralNetwork <- function(X,
                          Y,
                          hidden.nodes = c(256, 128),
                          iterations = 100,
                          activation.functions = c('relu'),
                          dropout.rates = c(),
                          cost.function = 'categorical_crossentropy',
                          optimizer = optimizer_rmsprop(),
                          metrics = c('accuracy'),
                          batch.size = 128,
                          subset.values = c(),
                          treat.numeric.as.categorical = FALSE) {
    # verify the drop rates vector length is compatible with the number of hidden nodes
    if (length(dropout.rates) >= length(hidden.nodes)) {
        stop("Dropout length must be less than the number of layers");
    }

    # ensure if subset.values is set, it equals the length X
    if (length(subset.values) > 0 && length(subset.values) != length(X)) {
        stop("subset.values length must equal the length of X")
    }

    model <- keras_model_sequential()

    # hidden layers
    for (i in 1:(length(hidden.nodes) - 1)) {
        if (i == 1) {
            # input layer
            model %>% layer_dense(units = hidden.nodes[1], activation = activation.functions[1], input_shape = ncol(X))
        } else {
            activation = if (i <= length(activation.functions)) activation.functions[i] else activation.functions[length(activation.functions)]
            model %>% layer_dense(units = hidden.nodes[i], activation = activation)
        }

        if (i <= length(dropout.rates)) {
            model %>% layer_dropout(rate = dropout.rates[i]);
        }
    }

    output_shape = 1
    if (treat.numeric.as.categorical || class(Y) != "numeric") {
        output_shape = length(unique(Y))
        Y <- to_categorical(Y)
    }

    # output layer
    model %>% layer_dense(units = output_shape, activation = 'softmax')

    model %>% compile(
        loss = cost.function,
        optimizer = optimizer,
        metrics = metrics
    )

    print(model)

    if (length(subset.values) > 0) {
        subset.values <- as.logical(subset.values)
        x_train <- X[subset.values]
        y_train <- Y[subset.values]
        x_test <- X[!subset.values]
        y_test <- Y[!subset.values]
    } else {
        x_train <- X
        y_train <- Y
        x_test <- X
        y_test <- Y
    }

    model %>% fit(
        x_train,
        y_train,
        epochs = iterations,
        batch_size = batch.size
    )

    #predictions <- predict_classes(model, X)
    #fitted.weights <- get_weights(model)
    #evaluation <- evaluate(model, x_test, y_test)

    #output <- list(predictions = predictions,
    #               fitted.weights = fitted.weights,
    #               cost = evaluation$loss,
    #               accuracy = evaluation$acc)

    return(model)
}


#' @importFrom flipFormat DeepLearningTable ExtractCommonPrefix
#' @importFrom flipData Observed
#' @importFrom flipU IsCount
#' @importFrom utils read.table
#' @export
#' @method print DeepLearning
print.DeepLearning <- function(x, ...)
{
    if (x$output == "Accuracy")
    {
        title <- paste0("Deep Learning: ", x$outcome.label)
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
    else
    {
        print(x$original)
        invisible(x)
    }
}


#' \code{predict.DeepLearning}
#'
#' Predicts values for numeric outcomes and group membership for categories based on \code{newdata}
#' and a fitted DeepLearning \code{object}.  A value (which may be NA) is returned for every instance
#' including those with missing data and for the fitted \code{data} before filtering in the case
#' that \code{newdata} is not specified.  NA is returned for cases with unfitted factor levels.
#' @param object A \code{DeepLearning} object.
#' @param newdata Optionally, a data frame including the variables used to fit the model.
#' If omitted, the \code{data} supplied to \code{DeepLearning()} is used before any filtering.
#' @param ... Additional arguments to pass to predict.DeepLearning.
#' @importFrom flipData CheckPredictionVariables
#' @importFrom keras predict_classes
#' @export
predict.DeepLearning <- function(object, newdata = object$model, ...)
{
    newdata <- CheckPredictionVariables(object, newdata)
    predictions <- predict_classes(object$original, as.matrix(newdata))
    return(predictions)
}




