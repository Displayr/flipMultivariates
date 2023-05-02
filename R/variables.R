
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
#' @importFrom flipData CheckPredictionVariables ValidateNewData
#' @export
predict.LDA <- function(object, newdata = NULL, na.action = na.pass, ...)
{
    no.newdata.provided <- is.null(newdata)
    if (no.newdata.provided) {
        predictions <- ldaExtractVariables(object, "class", object$prior, newdata = newdata, na.action, ...)
        return(predictions)
    }
    stopifnot("newdata must be a data.frame" = is.data.frame(newdata))

    predictor.cols <- match(colnames(newdata), names(object[["factor.levels"]]))
    # Remove unnecessary variables to avoid pointless computation later in AsNumeric
    if (anyNA(predictor.cols)) {
        unused.variables <- is.na(predictor.cols)
        newdata[unused.variables] <- NULL
        predictor.cols <- predictor.cols[!unused.variables]
        newdata[object[["outcome.name"]]] <- NULL
    }
    # Validate categorical variables and convert to factors if necessary
    factor.levels <- Filter(length, object[["factor.levels"]][predictor.cols])
    for (variable in names(factor.levels))
    { # If variable already a factor, realign the levels if required
        if (is.factor(newdata[[variable]])) {
            if (identical(levels(newdata[[variable]]), factor.levels[[variable]])) next
            newdata[[variable]] <- factor(newdata[[variable]], levels = factor.levels[[variable]])
            next
        }
        # If it is numeric or character, convert to factor with correct levels
        new.variable <- newdata[[variable]]
        if (is.numeric(new.variable))
            new.variable <- as.character(new.variable)
        newdata[[variable]] <- factor(new.variable, levels = factor.levels[[variable]])
    }
    # Expand the factors into dummy encoding
    if (length(factor.levels))
        newdata <- AsNumeric(newdata, binary = TRUE, remove.first = TRUE)
    ErrorIfInfinity(newdata)
    CheckPredictionVariables(object, newdata)
    ldaExtractVariables(object, "class", object$prior, newdata = newdata, na.action, ...)
}

#' @importFrom flipData Probabilities
#' @export
flipData::Probabilities

#' @importFrom flipData ValidateNewData
#' @export
Probabilities.LDA <- function(object, newdata = NULL, ...)
{
    newdata <- ValidateNewData(object, newdata)
    ldaExtractVariables(object, "posterior", object$prior, newdata = newdata, na.action = na.pass, ...)
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
    suppressWarnings(predict(object$original, prior = prior, newdata = newdata, na.action = na.action, ...)[[type]])
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
#' @param ... Additional arguments to pass to \code{predict}
#' @importFrom stats na.pass
#' @importFrom flipData CheckPredictionVariables ValidateNewData
#' @export
predict.RandomForest <- function(object, newdata = NULL, na.action = na.pass, ...)
{
    newdata <- ValidateNewData(object, newdata)
    randomForestExtractVariables(object, "response", newdata = newdata, na.action = na.action)
}

#' @importFrom flipData ValidateNewData
#' @export
Probabilities.RandomForest <- function(object, newdata = NULL, ...)
{
    requireCategoricalOutcome(object)
    newdata <- ValidateNewData(object, newdata)
    randomForestExtractVariables(object, "prob", newdata, ...)
}

#' @importFrom stats predict
randomForestExtractVariables <- function(object, type, newdata = object$model, na.action = na.pass, ...)
{
    pred <- predict(object$original, type, newdata = newdata, na.action = na.action, ...)
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
#' @importFrom flipData CheckPredictionVariables ValidateNewData
#' @export
predict.SupportVectorMachine <- function(object, newdata = NULL, ...)
{
    newdata <- ValidateNewData(object, newdata)
    # Since e1071 svm predictions cannot return NA for missing data, we predict only for complete.cases
    # (without NA or new levels). Default to NA for other instances.
    complete.observations <- complete.cases(newdata)
    newdata[complete.observations, "prediction"] <-
        predict(object$original, newdata = newdata[complete.observations, , drop = FALSE], ...)
    newdata$prediction
}

#' @importFrom flipData ValidateNewData
#' @export
Probabilities.SupportVectorMachine <- function(object, newdata = NULL, ...)
{
    requireCategoricalOutcome(object)
    newdata <- ValidateNewData(object, newdata)
    predictions <- predict(object$original, newdata = newdata, probability = TRUE)
    prob.excluding.na <- attr(predictions, "probabilities")
    # add NA probability for instances with missing prediction variables
    all.probs <- data.frame(matrix(NA, ncol = ncol(prob.excluding.na), nrow = nrow(object$model)))
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
#' @param keep.soft.probs A logical to control whether output returns soft probabilities
#' (i.e. probabilities for each categorical level) are kept or output is coerced to a factor
#' via the largest probability.
#' @param ... Additional arguments to pass to predict.xgb.Booster.
#' @importFrom stats complete.cases
#' @importFrom flipData CheckPredictionVariables ValidateNewData
#' @importFrom xgboost xgb.load.raw
#' @export
predict.GradientBoost <- function(object, newdata = NULL, keep.soft.probs = FALSE, ...)
{
    newdata <- ValidateNewData(object, newdata)
    newdata <- OneHot(newdata, object$outcome.name)$X
    object$original$feature_names <- colnames(newdata) # avoids error if newdata uses names and model uses labels
    prediction <- try(predict(object$original, newdata = newdata, reshape = TRUE, ...),
                      TRUE)

    if (inherits(prediction, "try-error"))
    { # If an old output, try and recover it, otherwise suggest user recompute it.
        if (grepl("^Error in predict.xgb.Booster", prediction))
        {
            object$original$handle <- xgb.load.raw(object$original$raw)
            prediction <- try(predict(object$original, newdata = newdata,
                                      reshape = TRUE, ...), TRUE)
        }
        # If still throwing an error after attempted salvaging old output attempt, suggest user recompute
        if (inherits(prediction, "try-error"))
            stop("Unable to predict values on this gradient boosting output. If it is an old ",
                 "output, please re-compute it since older gradient boosting outputs are ",
                 "not compatible with newer versions of the gradient boosting prediction method. ",
                 "If errors persist after recomputing, please contact support for further help")
    }

    if (object$original$params$objective == "binary:logistic" && !keep.soft.probs)
    {
        prediction <- ceiling(2 * prediction)
        prediction <- object$outcome.levels[prediction]
        prediction <- factor(prediction, levels = object$outcome.levels)
    }
    if (object$original$params$objective == "multi:softprob" && !keep.soft.probs)
        prediction <- factor(object$outcome.levels[apply(prediction, 1, which.max)])

    # Since xgboost predicts regardless of missing data, overwrite with NA if not complete.cases
    prediction[!complete.cases(newdata)] <- NA
    return(prediction)
}

#' @importFrom flipData ValidateNewData
#' @export
Probabilities.GradientBoost <- function(object, newdata = NULL, ...)
{
    requireCategoricalOutcome(object)
    newdata <- ValidateNewData(object, newdata)
    probabilities <- data.frame(predict(object, newdata = newdata, keep.soft.probs = TRUE))
    if (object$original$params$objective == "binary:logistic")
        probabilities <- cbind(1 - probabilities, probabilities)

    # add NA probability for instances with missing prediction variables
    colnames(probabilities) <- object$outcome.levels
    probabilities[!complete.cases(object$model), ] <- NA
    return(as.matrix(probabilities))
}

#' @title Compute the Propensity Weight scores for a binary classification model
#' @description Computes the propensity weights from a binary classification model
#'   using the inverse predicted probabilities of each observation being in the
#'   positive class.
#' @param object A \code{Regression} or \code{MachineLearning} output.
#' @return A vector of numeric propensity weights for each case in the model.
#' @importFrom flipU OutcomeVariable
#' @export
PropensityWeights <- function(object)
{
    is.binary.ml.model <- inherits(object, "MachineLearning") && !object$numeric.outcome
    is.binary.regression <- inherits(object, paste0(c("Binary", "Multinomial"), "LogitRegression"))
    binary.outcome.msg <- paste0("Propensity weights can only be saved for binary classification models; ",
                                 "e.g., Binary Logit Regression or Machine Learning classification model with an ",
                                 "outcome variable that is ordinal or nominal variable with two categories.")
    if (!(is.binary.ml.model || is.binary.regression))
        stop(binary.outcome.msg)
    probabilities <- Probabilities(object)
    n.classes <- NCOL(probabilities)
    if (n.classes > 2L)
    {
        msg <- paste0("The supplied model is a multiclass classification model with ",
                      n.classes, " outcome categories/class labels. ", binary.outcome.msg,
                      " Consider merging categories in the outcome variable to produce ",
                      "a binary classification before computing propensity weights.")
        stop(msg)
    }
    outcome.variable <- OutcomeVariable(object[["formula"]], object[["model"]])
    if (nlevels(outcome.variable) != 2L)
        stop(binary.outcome.msg)
    n.obs <- NROW(probabilities)
    positive.class <- levels(outcome.variable)[2L]
    # Return the inverse of the probability depending on the outcome variable
    # If positive class, return the second column, otherwise first, use single indexing for speed.
    1 / probabilities[1:n.obs + n.obs * (outcome.variable == positive.class)]
}

requireCategoricalOutcome <- function(object)
{
    if (object[["numeric.outcome"]])
        stop("Probabilities are only applicable to models with categorical outcome variables.")
}
