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
#' @importFrom flipData CheckPredictionVariables
#' @export
predict.LDA <- function(object, newdata = NULL, na.action = na.pass, ...)
{
    aligned <- CheckPredictionVariables(object, newdata)
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
#' @importFrom flipData CheckPredictionVariables
#' @export
predict.RandomForest <- function(object, newdata = NULL, na.action = na.pass, ...)
{
    aligned <- CheckPredictionVariables(object, newdata)
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
#' @importFrom flipData CheckPredictionVariables
#' @export
predict.SupportVectorMachine <- function(object, newdata = NULL, na.action = na.omit, ...)
{
    # CheckPredictionVariables is still required without newdata because predictions in object$fitted may be
    # a subset of object$model.
    aligned <- CheckPredictionVariables(object, newdata)
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

