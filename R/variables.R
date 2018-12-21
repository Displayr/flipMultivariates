
#' \code{predict.LDA}
#'
#' Predicts a model outcome based on \code{newdata} and a fitted LDA \code{object}.  A value (which
#' may be NA) is returned for every instance including those with missing data and for the
#' fitted \code{data} before filtering in the case that \code{newdata} is not specified.
#' NA is returned for cases with unfitted factor levels.
#' @param object A \code{LDA} object.
#' @param newdata Optionally, a data frame including the variables used to fit the model.
#' If omitted, the \code{data} supplied to \code{LDA()} is used before any filtering.
#' @param na.action Function determining what should be done with missing values in \code{newdata}.
#' The default is to predict \code{NA}.
#' @param ... Additional arguments to pass to predict.lda.
#' @importFrom stats na.pass
#' @importFrom flipData CheckPredictionVariables
#' @export
predict.LDA <- function(object, newdata = NULL, na.action = na.pass, ...)
{
    # CheckPredictionVariables is still required without newdata because empty training levels are removed
    newdata <- if (is.null(newdata))
        # no warnings from CheckPredictionVariables if predicting training data
        suppressWarnings(CheckPredictionVariables(object, object$model))
    else
    {
        predictor.cols <- sapply(colnames(newdata), match, names(object$factor.levels))

        for (newdata.col in seq(length(predictor.cols)))
        {
            predictor.col <- predictor.cols[newdata.col]
            if (!is.na(newdata.col) && !is.null(object$factor.levels[[predictor.col]]))
            {
                newdata[, newdata.col] <- factor(as.character(newdata[, newdata.col]),
                                                 levels = object$factor.levels[[predictor.col]])
            }
        }
        newdata[, is.na(predictor.cols)] <- NULL
        newdata <- AsNumeric(newdata, binary = TRUE, remove.first = TRUE)
        ErrorIfInfinity(newdata)
        CheckPredictionVariables(object, newdata)
    }

    ldaExtractVariables(object, "class", object$prior, newdata = newdata, na.action, ...)
}

#' \code{Probabilities.LDA}
#'
#' Estimates probabilities of group membership for the entire sample passed into the original analysis (including missing and filtered values).
#' @param x A \code{LDA} object.
#' @importFrom stats na.pass
#' @export
Probabilities.LDA <- function(x)
{
    ldaExtractVariables(x, "posterior", x$prior, newdata = x$model, na.action = na.pass)
}

#' \code{DiscriminantVariables}
#'
#' Computes discriminant variables for the entire sample passed into the original analysis (including missing and filtered values).
#' @param x A \code{LDA} object.
#' @importFrom stats na.pass
#' @export
DiscriminantVariables <- function(x)
{
    ldaExtractVariables(x, "x", x$observed.prior, newdata = x$model, na.action = na.pass)
}

#' @importFrom stats predict
ldaExtractVariables <- function(object, type, prior, newdata = object$model, na.action = na.pass, ...)
{
    newdata[, object$outcome.name] <- NULL
    set.seed(12321) # avoid random tie-breaking from max.col within predict()
    suppressWarnings(predict(object$original, prior = prior , newdata = newdata, na.action = na.action)[[type]])
}

#' \code{predict.RandomForest}
#'
#' Predicts values for numeric outcomes and group membership for categories based on \code{newdata}
#' and a fitted RandomForest \code{object}.  A value (which may be NA) is returned for every instance
#' including those with missing data and for the fitted \code{data} before filtering in the case
#' that \code{newdata} is not specified.  NA is returned for cases with unfitted factor levels.
#' @param object A \code{RandomForest} object.
#' @param newdata Optionally, a data frame including the variables used to fit the model.
#' If omitted, the \code{data} supplied to \code{RandomForest()} is used before any filtering.
#' @param na.action Function determining what should be done with missing values in \code{newdata}.
#' The default is to predict \code{NA}.
#' @param ... Additional arguments to pass to predict.RandomForest.
#' @importFrom stats na.pass
#' @importFrom flipData CheckPredictionVariables
#' @export
predict.RandomForest <- function(object, newdata = NULL, na.action = na.pass, ...)
{
    # CheckPredictionVariables is still required without newdata because empty training levels are removed
    newdata <- if (is.null(newdata))
        # no warnings from CheckPredictionVariables if predicting training data
        suppressWarnings(CheckPredictionVariables(object, object$model))
    else
        CheckPredictionVariables(object, newdata)

    randomForestExtractVariables(object, "response", newdata = newdata, na.action = na.action)
}

#' \code{Probabilities.RandomForest}
#'
#' Estimates probabilities of group membership for the entire sample passed into the original analysis (including missing and filtered values).
#' @param x A \code{RandomForest} object.
#' @importFrom stats na.pass
#' @export
Probabilities.RandomForest <- function(x)
{
    if(x$numeric.outcome)
        stop("Probabilities are only applicable to models with categorical outcome variables.")
    randomForestExtractVariables(x, "prob", newdata = x$model)
}

#' @import randomForest
randomForestExtractVariables <- function(object, type, newdata = object$model, na.action = na.pass)
{
    pred <- predict(object$original, type, newdata = newdata, na.action = na.action)
    names(pred) <- NULL
    pred
}


#' \code{predict.SupportVectorMachine}
#'
#' Predicts values for numeric outcomes and group membership for categories based on \code{newdata}
#' and a fitted \code{SupportVectorMachine} object.  A value (which may be NA) is returned for every instance
#' including those with missing data. If \code{newdata} is not specified the fitted \code{data} before filtering
#' are used.  NA is returned for cases with unfitted factor levels or any missing data.
#' @param object A \code{SupportVectorMachine} object.
#' @param newdata Optionally, a data frame including the variables used to fit the model.
#' If omitted, the \code{data} supplied to \code{SupportVectorMachine()} is used before any filtering.
#' @param ... Additional arguments to pass to predict.svm.
#' @importFrom stats complete.cases
#' @importFrom flipData CheckPredictionVariables
#' @export
predict.SupportVectorMachine <- function(object, newdata = NULL, ...)
{
    # CheckPredictionVariables is still required without newdata because empty training levels are removed
    newdata <- if (is.null(newdata))
        # no warnings from CheckPredictionVariables if predicting training data
        suppressWarnings(CheckPredictionVariables(object, object$model))
    else
        CheckPredictionVariables(object, newdata)

    # Since e1071 svm predictions cannot return NA for missing data, we predict only for complete.cases
    # (without NA or new levels). Default to NA for other instances.
    newdata[complete.cases(newdata), "prediction"] <-
        predict(object$original, newdata = newdata[complete.cases(newdata), , drop = FALSE], ...)
    return(newdata$prediction)
}

#' \code{Probabilities.SupportVectorMachine}
#'
#' Estimates probabilities of group membership for the entire sample passed into the original analysis (including missing and filtered values).
#' @param x A \code{SupportVectorMachine} object.
#' @export
Probabilities.SupportVectorMachine <- function(x)
{
    if(x$numeric.outcome)
        stop("Probabilities are only applicable to models with categorical outcome variables.")

    predictions <- predict(x$original, newdata = x$model, probability = TRUE)
    prob.excluding.na <- attr(predictions, "probabilities")
    # add NA probability for instances with missing prediction variables
    all.probs <- data.frame(matrix(NA, ncol = ncol(prob.excluding.na), nrow = nrow(x$model)))
    all.probs[row.names(prob.excluding.na), ] <- prob.excluding.na
    colnames(all.probs) <- colnames(prob.excluding.na)
    return(as.matrix(all.probs[, order(names(all.probs))]))
}

#' \code{predict.GradientBoost}
#'
#' Predicts values for numeric outcomes and group membership for categories based on \code{newdata}
#' and a fitted \code{GradientBoost} object.  A value (which may be NA) is returned for every instance
#' including those with missing data. If \code{newdata} is not specified the fitted \code{data} before filtering
#' are used.  NA is returned for cases with unfitted factor levels or any missing data.
#' @param object A \code{GradientBoost} object.
#' @param newdata Optionally, a data frame including the variables used to fit the model.
#' If omitted, the \code{data} supplied to \code{GradientBoost()} is used before any filtering.
#' @param ... Additional arguments to pass to predict.xgb.Booster.
#' @importFrom stats complete.cases
#' @importFrom flipData CheckPredictionVariables
#' @export
predict.GradientBoost <- function(object, newdata = NULL, ...)
{
    # CheckPredictionVariables is still required without newdata because empty training levels are removed
    newdata <- if (is.null(newdata))
        # no warnings from CheckPredictionVariables if predicting training data
        suppressWarnings(CheckPredictionVariables(object, object$model))
    else
        CheckPredictionVariables(object, newdata)
    newdata <- OneHot(newdata, object$outcome.name)$X
    object$original$feature_names <- colnames(newdata) # avoids error if newdata uses names and model uses labels

    prediction <- predict(object$original, newdata = newdata, reshape = TRUE, ...)

    if (object$original$params$objective == "binary:logistic")
    {
        prediction <- ceiling(2 * prediction)
        prediction <- object$outcome.levels[prediction]
        prediction <- factor(prediction, levels = object$outcome.levels)
    }
    if (object$original$params$objective == "multi:softprob")
    {
        prediction <- factor(apply(prediction, 1, which.max), levels = as.character(1:length(object$outcome.levels)))
        levels(prediction) <- object$outcome.levels
    }

    # Since xgboost predicts regardless of missing data, overwrite with NA if not complete.cases
    prediction[!complete.cases(newdata)] <- NA
    return(prediction)
}

#' \code{Probabilities.GradientBoost}
#'
#' Estimates probabilities of group membership for the entire sample passed into the original analysis (including
#' missing and filtered values).
#' @param object A \code{GradientBoost} object.
#' @export
Probabilities.GradientBoost <- function(object)
{
    if(object$numeric.outcome)
        stop("Probabilities are only applicable to models with categorical outcome variables.")

    data <- CheckPredictionVariables(object, object$model)
    data <- OneHot(data, object$outcome.name)$X
    probabilities <- data.frame(predict(object$original, newdata = data, reshape = TRUE))
    if (object$original$params$objective == "binary:logistic")
        probabilities <- cbind(1 - probabilities, probabilities)

    # add NA probability for instances with missing prediction variables
    colnames(probabilities) <- object$outcome.levels
    probabilities[!complete.cases(data), ] <- NA
    return(as.matrix(probabilities))
}


