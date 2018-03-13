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
#' @param hidden.nodes Numeric; a \code{\link{vector}} specfy the number of hidden nodes in each
#' hideen layer.
#' @param iterations Integer; the number of iterations to train the network.
#' @export
DeepLearning <- function(formula,
                                 data = NULL,
                                 subset = NULL,
                                 weights = NULL,
                                 output = "Accuracy",
                                 missing  = "Exclude cases with missing data",
                                 seed = 12321,
                                 show.labels = FALSE,
                                 hidden.nodes,
                                 iterations = 100)
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

    # X is a numeric matrix with dummy encoding of factors
    X <- as.matrix(AsNumeric(.estimation.data.1[, -outcome.i]))

    Y <- .estimation.data.1[[outcome.i]]
    # Binary factor encoded as a vector of 0s and 1s, multiclass is dummy encoded
    if (!numeric.outcome)
        if (length(levels(Y)) > 2)
            Y <- as.matrix(AsNumeric(.estimation.data.1[[outcome.i]], name = outcome.name))
        else
            Y <- as.numeric(Y) - 1

    ####################################################################
    ##### Fitting the model. Ideally, this should be a call to     #####
    ##### another function, with the output of that function       #####
    ##### called 'original'.                                       #####
    ####################################################################
    set.seed(seed)
    result <- list(original = neuralNetwork(X,
                                            Y,
                                            numeric.outcome = numeric.outcome,
                                            hidden.nodes = hidden.nodes,
                                            iterations = iterations))
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
    result$outcome.levels <- levels(result$estimation.data[result$outcome.name][[1]])

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
                          optimizer = optimizer_rmsprop(),
                          metrics = c('accuracy'),
                          batch.size = 128,
                          numeric.outcome = TRUE) {

    model <- keras_model_sequential()

    # hidden layers
    for (i in 1:(length(hidden.nodes))) {
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

    # defaults for numeric outcome
    output.shape <- 1
    output.activation <- NULL
    cost.function <- "mse"

    if (!numeric.outcome) {
        #if ((n.classes <- length(unique(Y))) > 2) {
        if (NCOL(Y) > 1) {
            output.shape <- NCOL(Y)
            #Y <- to_categorical(Y - 1)
            output.activation <- "softmax"
            cost.function <- "categorical_crossentropy"
        } else {
            output.activation <- "sigmoid"
            cost.function <- "binary_crossentropy"
        }
    }

    # output layer
    model %>% layer_dense(units = output.shape, activation = output.activation)

    model %>% compile(
        loss = cost.function,
        optimizer = optimizer,
        metrics = metrics
    )

    print(model)

    model %>% fit(
        X,
        Y,
        epochs = iterations,
        batch_size = batch.size
    )

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
#' @param new.data Optionally, a data frame including the variables used to fit the model.
#' If omitted, the \code{data} supplied to \code{DeepLearning()} is used before any filtering.
#' @param ... Additional arguments to pass to predict.DeepLearning.
#' @importFrom flipData CheckPredictionVariables
#' @importFrom keras predict_classes
#' @export
predict.DeepLearning <- function(object, new.data = object$model, ...)
{
    new.data <- CheckPredictionVariables(object, new.data)
    X <- as.matrix(AsNumeric(new.data))

    if (!object$numeric.outcome)
    {
        if (length(object$outcome.levels) > 2)
            predictions <- predict_classes(object$original, X)
        else
            predictions <- predict(object$original, X) > 0.5

        predictions <- factor(predictions + 1,
                              levels = seq(length(object$outcome.levels)),
                              labels = object$outcome.levels)
    }
    else
        predictions <- predict(object$original, X)

    return(predictions)
}




