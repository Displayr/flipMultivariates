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
#' @param normalize Logical; if \code{TRUE} all predictor variables are normalized to have zero mean
#'   and unit variance.
#' @param output One of \code{"Accuracy"}, \code{"Prediction-Accuracy Table"}, \code{"Cross Validation"}
#'   or \code{"Network Layers"}.
#' @param missing How missing data is to be treated. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"}, or
#'   \code{"Imputation (replace missing values with estimates)"}.
#' @param seed The random number seed.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param hidden.nodes A \code{\link{vector}} that specifies the number of hidden nodes in each
#' hidden layer (and hence implicitly the number of hidden layers). Alternatively, a comma-delimited
#' string of integers may be provided.
#' @param max.epochs Integer; the maximum number of epochs for which to train the network.
#' @param rand.verbose Prints extra info for checking the random number generation
#' @details Categorical predictor variables are converted to binary (dummy) variables.
#' @details The model is trained first using a random 70% of the data (after any subset) while measuring the
#' cross-validation loss on the remaining 30% of the data. Training is stopped at the sooner of
#' \code{max.epochs} and 3 epochs of no improvement in cross-validation loss. The final model
#' is then retrained on all data (after any \code{"subset"}).
#'
#' @importFrom stats sd
#' @importFrom flipU ConvertCommaSeparatedStringToVector StopForUserError
#' @export
DeepLearning <- function(formula,
                         data = NULL,
                         subset = NULL,
                         weights = NULL,
                         output = "Accuracy",
                         missing  = "Exclude cases with missing data",
                         normalize = TRUE,
                         seed = 12321,
                         rand.verbose = FALSE,
                         show.labels = FALSE,
                         hidden.nodes = 10,
                         max.epochs = 100)
{
    ####################################################################
    ##### Error checking specific to this function                ######
    ####################################################################

    if (!is.numeric(hidden.nodes))
    {
        hidden.nodes <- ConvertCommaSeparatedStringToVector(hidden.nodes)
        hidden.nodes <- suppressWarnings(as.integer(hidden.nodes))
        if (any(is.na(hidden.nodes)) || any(hidden.nodes <= 0))
            StopForUserError("Nodes of hidden layers must be specified as comma separated positive integers.")
    }

    ####################################################################
    ##### Reading in the data and doing some basic tidying        ######
    ####################################################################

    # Identify whether subset and weights are variables in the environment or in data.
    subset.description <- try(deparse(substitute(subset)), silent = TRUE)
    subset <- eval(substitute(subset), data, parent.frame())
    weights.description <- try(deparse(substitute(weights)), silent = TRUE)
    weights <- eval(substitute(weights), data, parent.frame())

    prepared.data <- prepareMachineLearningData(formula, data, subset, subset.description,
                                                weights, weights.description, missing, seed,
                                                bootstrap.weights = FALSE)

    unweighted.training.data <- prepared.data$unweighted.training.data

    ####################################################################
    ##### Processing specific to this function                    ######
    ####################################################################

    # No need to resample weights because keras uses sample_weight for each sample's loss
    cleaned.weights <- prepared.data$cleaned.weights

    # X is a normalized numeric matrix with dummy encoding of factors
    X <- as.matrix(AsNumeric(unweighted.training.data[, -prepared.data$outcome.i]))
    if (normalize)
    {
        means <- apply(X, 2, mean)
        stdevs <- apply(X, 2, sd)
        X[, stdevs != 0] <- scale(X[, stdevs != 0], center = means[stdevs != 0], scale = stdevs[stdevs != 0])
    }

    Y <- unweighted.training.data[[prepared.data$outcome.i]]
    # Binary factor encoded as a vector of 0s and 1s, multiclass is dummy encoded
    if (!prepared.data$numeric.outcome)
        if (nlevels(Y) > 2)
            Y <- AsNumeric(unweighted.training.data[[prepared.data$outcome.i]],
                           name = prepared.data$outcome.name)
    else
        Y <- as.numeric(Y) - 1
    Y <- as.matrix(Y)

    ####################################################################
    ##### Fitting the model. Ideally, this should be a call to     #####
    ##### another function, with the output of that function       #####
    ##### called 'original'.                                       #####
    ####################################################################

    set.seed(seed)
    nn <- neuralNetwork(X,
                        Y,
                        numeric.outcome = prepared.data$numeric.outcome,
                        hidden.nodes = hidden.nodes,
                        max.epochs = max.epochs,
                        seed = seed,
                        rand.verbose = rand.verbose,
                        weights = cleaned.weights)
    result <- list(original = nn$original,
                   original.serial = nn$original.serial,
                   cross.validation = nn$cross.validation)

    ####################################################################
    ##### Saving direct input parameters                           #####
    ####################################################################

    result$hidden.nodes <- hidden.nodes
    result$output <- output
    result$missing <- missing
    result$normalize <- normalize
    result$training.means <- if (normalize) means else NULL
    result$training.stdevs <- if (normalize) stdevs else NULL
    class(result) <- c("DeepLearning", class(result))

    ####################################################################
    ##### Saving processed information                             #####
    ####################################################################

    result <- saveMachineLearningResults(result, prepared.data, show.labels)
    result[["estimation.data.template"]] <- prepared.data[["estimation.data.template"]]
    attr(result, "ChartData") <- prepareDLChartData(result)
    result
}

prepareDLChartData <- function(x, ...)
{
    if (py_is_null_xptr(x$original))
        x$original <- unserialize_model(x$original.serial)

    if (x$output == "Accuracy")
    {
        return(calcAccuracy(x))
    }
    else if (x$output == "Prediction-Accuracy Table")
    {
        return(ExtractChartData(x$confusion))
    }
    else if (x$output == "Cross Validation")
    {
        output.data <- data.frame(x$cross.validation$metrics)
        colnames(output.data) <- c("Training accuracy", "Training loss", "Validation accuracy", "Validation loss")
        return(output.data)
    }
    else
    {
        return(as.matrix(capture.output(print(x$original))))
    }
}

#' @importFrom keras keras_model_sequential optimizer_rmsprop %>% layer_dense layer_dropout to_categorical
#' @importFrom keras compile fit serialize_model callback_early_stopping shape
#' @importFrom reticulate py_set_seed
#' @importFrom tensorflow tf
neuralNetwork <- function(X,
                          Y,
                          hidden.nodes = c(256, 128),
                          max.epochs = 100,
                          activation.functions = c('relu'),
                          dropout.rates = c(),
                          optimizer = optimizer_rmsprop(),
                          metrics = c(),
                          batch.size = 128,
                          numeric.outcome = TRUE,
                          seed = 12321,
                          rand.verbose = FALSE,
                          weights = NULL) {

    # If using tensorflow>=2.12, use legacy optimizer
    tf.above.2.11 <- package_version(tensorflow::tf$version$VERSION) >= package_version("2.12")
    if (tf.above.2.11)
        optimizer <- tensorflow::tf$optimizers$legacy$RMSprop()

    # Note - below disables GPU computations and CPU parallelization by default, so slows performance
    # This should be removed when we require more speed
    # https://keras.rstudio.com/articles/faq.html#how-can-i-obtain-reproducible-results-using-keras-during-development
    #use_session_with_seed(seed)

    # This line is used instead of use_session_with_seed to avoid bug with tf v2.0.0
    # https://github.com/rstudio/keras/issues/890
    py_set_seed(seed)
    tensorflow::tf$random$set_seed(seed)
    rtmp <- tensorflow::tf$random$uniform(shape(2,2)) # to fix DS-2879
    if (rand.verbose)
    {
        cat("Seed:", seed, "\n")
        cat("Some random numbers: ")
        print(rtmp)
    }

    # create a function that builds the model one layer at a time.
    # called for cross-validation then again for the final model
    build.model <- function() {
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
        metrics <- c("mae")

        if (!numeric.outcome) {
            metrics <- c("accuracy")
            if (NCOL(Y) > 1) {
                output.shape <- NCOL(Y)
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
        return(model)
    }

    # randomly shuffle the data
    shuffle <- sample(NROW(X))
    X <- X[shuffle, , drop = FALSE]
    Y <- Y[shuffle, ]

    # train until no improvement in validation loss for 3 epochs
    model <- build.model()
    fit.args <- list(
        model, X, Y,
        epochs = max.epochs,
        batch_size = batch.size,
        validation_split = 0.3,   # last 30% of samples are used for validation
        callbacks = c(callback_early_stopping(patience = 3)),
        verbose = 0
    )
    fit.args[[if (tf.above.2.11) "weighted_metrics" else "sample_weights"]] <- weights
    history <- do.call(fit, fit.args)
    history$metrics <- history$metrics[sort(names(history$metrics))]    # fix unstable ordering
    history$params <- history$params[sort(names(history$params))]

    if ((optimal.epochs <- length(history$metrics$val_loss)) == max.epochs)
        warning("Cross validation loss is still decreasing after maximum number of epochs.",
                " Model may not have converged, consider increasing the maximum number of epochs.")
    else
        optimal.epochs <- optimal.epochs - 3

    # retrain on all data for optimal number of epochs
    model <- build.model()
    model %>% fit(
        X,
        Y,
        epochs = optimal.epochs,
        batch_size = batch.size,
        verbose = 0
    )

    return(list(original = model,
                original.serial = serialize_model(model),
                cross.validation = padMetrics(history)))
}

# This function fixes up a bug in keras::plot.keras_training_history
# which started occurring after tensorflow updated to version 2.2.0
padMetrics <- function(cv)
{
    epochs <- cv$params$epochs
    for (i in 1:4)
    {
        tmp <- cv$metrics[[i]]
        len <- length(tmp)
        if (len < epochs)
            cv$metrics[[i]] <- c(tmp, rep(NA, epochs - len))
    }
    return(cv)
}



#' @importFrom flipFormat DeepLearningTable ExtractCommonPrefix
#' @importFrom flipData Observed
#' @importFrom flipU IsCount
#' @importFrom utils read.table
#' @importFrom keras unserialize_model
#' @importFrom reticulate py_is_null_xptr
#' @importFrom graphics plot
#' @export
#' @method print DeepLearning
print.DeepLearning <- function(x, ...)
{
    if (py_is_null_xptr(x$original))
        x$original <- unserialize_model(x$original.serial)

    if (x$output == "Accuracy")
        print(formatAccuracy(x, "Deep Learning"))
    else if (x$output == "Prediction-Accuracy Table")
        print(x$confusion)
    else if (x$output == "Cross Validation")
        print(plot(x$cross.validation))
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
#' @importFrom flipData CheckPredictionVariables ValidateNewData
#' @importFrom keras k_argmax unserialize_model
#' @importFrom reticulate py_is_null_xptr
#' @export
predict.DeepLearning <- function(object, newdata = NULL, ...)
{
    if (py_is_null_xptr(object$original))
        object$original <- unserialize_model(object$original.serial)

    newdata <- ValidateNewData(object, newdata)

    X <- as.matrix(AsNumeric(newdata))
    constants <- object$training.stdevs == 0
    if (object$normalize)
        X[, !constants] <- scale(X[, !constants, drop = FALSE],
                                 center = object$training.means[!constants],
                                 scale = object$training.stdevs[!constants])

    if (!object$numeric.outcome)
    {
        if (length(object$outcome.levels) > 2L)
            predictions <- as.numeric(k_argmax(predict(object$original, X)))
        else
            predictions <- predict(object$original, X) > 0.5

        predictions <- factor(predictions + 1L,
                              levels = seq(length(object$outcome.levels)),
                              labels = object$outcome.levels)
    }
    else
        predictions <- predict(object$original, X)

    return(predictions)
}

#' @importFrom flipData ValidateNewData
#' @importFrom keras unserialize_model
#' @importFrom reticulate py_is_null_xptr
#' @export
Probabilities.DeepLearning <- function(object, newdata = NULL, ...)
{
    requireCategoricalOutcome(object)
    newdata <- ValidateNewData(object, newdata)

    if (py_is_null_xptr(object$original))
        object$original <- unserialize_model(object$original.serial)

    X <- as.matrix(AsNumeric(newdata))
    constants <- object$training.stdevs == 0
    if (object$normalize)
        X[, !constants] <- scale(X[, !constants],
                                 center = object$training.means[!constants],
                                 scale = object$training.stdevs[!constants])

    probabilities <- predict(object$original, X)
    if (length(object$outcome.levels) == 2)
        probabilities <- cbind(1 - probabilities,probabilities)
    colnames(probabilities) <- object$outcome.levels
    probabilities
}
