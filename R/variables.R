#' \code{predict.LDA}
#'
#' Predicts group membership  for the entire sample passed into the original analysis (including missing and filtered values).
#' @param object A \code{LDA} object.
#' @param newdata Optionally, a data frame in which to look for variables with which to predict.
#' If omitted, the data used to fit the model is used.
#' @param na.action Function determining what should be done with missing values in \code{newdata}.
#' The default is to predict \code{NA}.
#' @param ... Additional arguments to pass to predict.lda.
#' The default is to predict \code{NA}.
#' @importFrom stats na.pass
#' @export
predict.LDA <- function(object, newdata = NULL, na.action = na.pass, ...)
{
    aligned <- AlignPredictionVariables(object, newdata)
    newdata <- aligned$newdata
    new.level.flags <- aligned$new.level.flags
    # Predict only for instances with no new factors, default to NA otherwise.
    # droplevels is applied after filtering for the cases that can be predicted.
    newdata[!new.level.flags, "prediction"] <- ldaExtractVariables(object, "class", object$prior,
                                                                   newdata = droplevels(newdata[!new.level.flags, , drop = FALSE]), na.action, ...)
    return(newdata$prediction)
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
#' Computes discriminant varibales for the entire sample passed into the original analysis (including missing and filtered values).
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
    suppressWarnings(predict(object$original, prior = prior , newdata = newdata, na.action = na.action)[[type]])
}

#' \code{predict.RandomForest}
#'
#' Predicts values for numeric outcomes and group membership for categories, for the entire sample passed into the original analysis (including missing and filtered values).
#' @param object A \code{RandomForest} object.
#' @param newdata Optionally, a data frame in which to look for variables with which to predict.
#' If omitted, the data used to fit the model is used.
#' @param na.action Function determining what should be done with missing values in \code{newdata}.
#' The default is to predict \code{NA}.
#' @param ... Additional arguments to pass to predict.randomForest.
#' @importFrom stats na.pass
#' @export
predict.RandomForest <- function(object, newdata = NULL, na.action = na.pass, ...)
{
    aligned <- AlignPredictionVariables(object, newdata)
    newdata <- aligned$newdata
    new.level.flags <- aligned$new.level.flags
    # Predict only for instances with no new factors, default to NA otherwise.
    # droplevels is applied after filtering for the cases that can be predicted.
    newdata[!new.level.flags, "prediction"] <- randomForestExtractVariables(object, "response",
                                            newdata = droplevels(newdata[!new.level.flags, , drop = FALSE]),
                                            na.action = na.action)
    return(newdata$prediction)
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
    predict(object$original, type, newdata = newdata, na.action = na.action)
}


#' \code{predict.SupportVectorMachine}
#'
#' Predicts values for numeric outcomes and group membership for categories based on new data or the data
#' used to fit the model.  A value is returned for every instance including missing and filtered cases,
#' producing NA where no prediction is possible (eg missing data or unfitted factor levels).
#' @param object A \code{SupportVectorMachine} object.
#' @param newdata Optionally, a data frame including the variables used to fit the model.
#' If omitted, the actual data used to fit the model is used (before any filtering).
#' @param na.action Function determining what should be done with missing values in \code{newdata}.
#' The default is to predict \code{NA}.
#' @param ... Additional arguments to pass to predict.SupportVectorMachine.
#' @importFrom stats na.omit complete.cases
#' @export
predict.SupportVectorMachine <- function(object, newdata = NULL, na.action = na.omit, ...)
{
    # AlignPredictionVariables is still required without newdata because predictions in object$fitted may be
    # a subset of object$model.
    aligned <- AlignPredictionVariables(object, newdata)
    newdata <- aligned$newdata
    new.level.flags <- aligned$new.level.flags
    # Since e1071 svm predictions cannot return NA for missing data, we predict only for complete.cases and
    # no new factors.  Default to NA for other instances. droplevels is applied after removing cases that cannot be predicted.
    newdata[complete.cases(newdata) & !new.level.flags, "prediction"] <-
        predict(object$original, newdata = droplevels(newdata[complete.cases(newdata) & !new.level.flags, , drop = FALSE]))
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
    return(all.probs)
}


#' \code{AlignPredictionVariables}
#'
#' Verifies that newdata is consistent with data used to used to fit a model.  newdata must contain a
#' superset of the variables used to fit the model or an error results. If a factor variable of
#' newdata contains fewer levels than the factor used for fitting, the levels are expanded.  If a factor
#' variable contains more levels than the factor used for fitting, a warning is given.  Returns newdata
#' with potentially expanded factor levels and 'new.level.flags' indicating which instances have new levels.
#' @param object A \code{SupportVectorMachine} object.
#' @param newdata Optionally, a data frame including the variables used to fit the model.
#' If omitted, the actual data used to fit the model is used (before any filtering).
#' @export
AlignPredictionVariables <- function(object, newdata)
{
    if (is.null(newdata))
    {
        newdata <- object$model
        predicting.training <- TRUE
    }
    else
        predicting.training <- FALSE

    # EstimationData removes unused levels from training data (after filter and removing NA). svm.predict() binary
    # encodes factor variables according to the number of their levels and will fail if newdata contains
    # a different number of levels from training data (even if no instances have the unused levels).
    # Hence we filter out instances of newdata with new levels (predicting NA), and add back to newdata any
    # levels not present but were in training data.  Thus droplevels(newdata) is aligned with fitted levels.

    training <- object$model[object$subset, names(object$model) != object$outcome.name, drop = FALSE]
    train.levels <- sapply(droplevels(training), levels)

    if (!identical(setdiff(names(training), names(newdata)), character(0)))
        stop("Attempting to predict based on fewer variables than those used to train the model.")
    newdata <- newdata[, names(training)]
    prediction.levels <- sapply(newdata, levels)

    new.level.flags <- rep(FALSE, nrow(newdata))
    nb.flags <- 0

    for (i in 1:length(train.levels))
    {
        if (!is.null(train.levels[[i]]))    # factor variables only
        {
            # if there are any prediction levels that have not been used to train
            new.levels <- setdiff(prediction.levels[[i]], train.levels[[i]])
            if (!identical(new.levels, character(0)))
            {
                # set flags to TRUE for any newdata row with a new factor level
                new.level.flags[newdata[, i] %in% new.levels] <- TRUE
                updated.nb.flags <- sum(new.level.flags[new.level.flags == TRUE])
                if ((updated.nb.flags - nb.flags) > 0 & !predicting.training)
                    warning(sprintf("Prediction variable %s contains categories (%s) that were not used for training. %d instances are affected.",
                                    names(training[i]), new.levels, updated.nb.flags - nb.flags))
                nb.flags <- updated.nb.flags
            }
            # if train has any levels not in prediction, then add those levels to prediction
            if (!identical(setdiff(train.levels[[i]], prediction.levels[[i]]), character(0)))
                levels(newdata[, i]) <- train.levels[[i]]
        }
    }
    return(list(newdata = newdata, new.level.flags = new.level.flags))
}
