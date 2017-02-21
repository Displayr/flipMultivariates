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
predict.LDA <- function(object, newdata = object$model, na.action = na.pass, ...)
{
    ldaExtractVariables(object, "class", object$prior, newdata, na.action, ...)
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
    outcome.i <- match(object$outcome.name, names(newdata))
    newdata <- newdata[, -outcome.i]
    suppressWarnings(predict(object$original,prior = prior , newdata = newdata, na.action = na.action)[[type]])
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
predict.RandomForest <- function(object, newdata = object$model, na.action = na.pass, ...)
{
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
    #outcome.i <- match(object$outcome.name, names(newdata))

    #newdata <- newdata[, -outcome.i]
    predict(object$original, type, newdata = newdata, na.action = na.action)
}

#' \code{predict.DeepLearning}
#'
#' Predicts values for numeric outcomes and group membership for categories, for the entire sample passed into the original analysis (including missing and filtered values).
#' @param object A \code{DeepLearning} object.
#' @param newdata Optionally, a data frame in which to look for variables with which to predict.
#' If omitted, the data used to fit the model is used.
#' @param na.action Function determining what should be done with missing values in \code{newdata}.
#' The default is to predict \code{NA}.
#' @param ... Additional arguments to pass to predict.DArch
#' @importFrom stats na.pass
#' @export
predict.DeepLearning <- function(object, newdata = object$model, na.action = na.pass, ...)
{
    type <- "class"
    if (object$numeric.outcome)
        type <- "raw"
    deepLearningExtractVariables(object, type = type, newdata = newdata, na.action = na.action)
}


#' \code{Probabilities.DeepLearning}
#'
#' Estimates probabilities of group membership for the entire sample passed into the original analysis (including missing and filtered values).
#' @param x A \code{DeepLearning} object.
#' @importFrom stats na.pass
#' @export
Probabilities.DeepLearning <- function(x)
{
    if(x$numeric.outcome)
        stop("Probabilities are only applicable to models with categorical outcome variables.")
    deepLearningExtractVariables(x, type="raw", newdata = x$model, na.action = na.pass)
}


#' @importFrom flipTransformations RemoveMissingLevelsFromFactors
#' @import darch
deepLearningExtractVariables <- function(object, type, newdata = object$model, na.action = na.pass)
{
    # predict.DArch will stop if any previously unobserved data is seen
    # we do some rough data cleaning to avoid stopping if possible
    # note that filtered and missing data (or imputed values) are included
    any.missing <- apply(newdata, 1, function(x){any(is.na(x))})
    if (sum(any.missing) > 0)
        newdata[any.missing,] <- NA
    newdata <- RemoveMissingLevelsFromFactors(newdata)
    predict(object$original, type=type, newdata = newdata, na.action = na.action)
}


#' \code{predict.SupportVectorMachine}
#'
#' Predicts values for numeric outcomes and group membership for categories, for the entire sample passed
#' into the original analysis (including missing and filtered values).
#' @param object A \code{SupportVectorMachine} object.
#' @param newdata Optionally, a data frame in which to look for variables with which to predict.
#' If omitted, the data used to fit the model is used.
#' @param na.action Function determining what should be done with missing values in \code{newdata}.
#' The default is to predict \code{NA}.
#' @param ... Additional arguments to pass to predict.SupportVectorMachine.
#' @importFrom stats na.pass
#' @export
predict.SupportVectorMachine <- function(object, newdata = NULL, na.action = na.omit, ...)
{
    if (is.null(newdata))
    {
        # Must droplevels since EstimationData removes unused levels from training data.  svm.predict() binary
        # encodes prediction variables according to the number of levels and will fail if newdata contains
        # a different number of levels from training data (even if no instances have the unused levels).
        newdata <- droplevels(object$model)
        exclusions <- rep(FALSE, nrow(newdata))
    }
    else
    {
        training <- object$model[, names(object$model) != object$outcome.name]
        train.levels <- sapply(droplevels(training), levels)

        # droplevels to be consistent with treatment of training data
        newdata <- droplevels(newdata[, names(newdata) != object$outcome.name])
        prediction.levels <- sapply(newdata, levels)
        exclusions <- rep(FALSE, nrow(newdata))

        if (!identical(names(training), names(newdata)))
            stop("Attempting to predict based on a different set of variables than that used to
                 train the model.")

        for (i in 1:length(train.levels))
        {
            if (!is.null(train.levels[[i]]))    # factor variables only
            {
                # if prediction has any levels not in train
                new.levels <- setdiff(prediction.levels[[i]], train.levels[[i]])
                if (!identical(new.levels, character(0)))
                    # set exclusions to TRUE for any newdata row with a new factor level
                    nb.excluded <- sum(exclusions == TRUE)
                    exclusions[newdata[, i] %in% new.levels] <- TRUE
                    newly.excluded <- sum(exclusions == TRUE) - nb.excluded
                    warning(sprintf("Prediction variable %s contains categories (%s) that were not used for training. %d instances are affected and will be predicted NA.",
                                    names(training[i]), new.levels, newly.excluded))

                # if train has any levels not in prediction, then add those levels to prediction
                if (!identical(setdiff(train.levels[[i]], prediction.levels[[i]]), character(0)))
                    levels(newdata[, i]) <- train.levels[[i]]
            }

        }

    }
    # Since e1071 svm predictions cannot return NA for missing data, predict only for complete.cases
    # and no new factor levels, default to NA for other instances.
    newdata[complete.cases(newdata) & !exclusions, "prediction"] <-
        predict(object$original, newdata = droplevels(newdata[complete.cases(newdata) & !exclusions, ]))
    return(newdata$prediction)
}

#' \code{Probabilities.SupportVectorMachine}
#'
#' Estimates probabilities of group membership for the entire sample passed into the original analysis (including missing and filtered values).
#' @param x A \code{SupportVectorMachine} object.
#' @importFrom stats na.pass
#' @export
Probabilities.SupportVectorMachine <- function(x)
{
    if(x$numeric.outcome)
        stop("Probabilities are only applicable to models with categorical outcome variables.")

    predict(x$original, decision.values = TRUE, probability = TRUE, newdata = x$model, na.action = na.pass)
}

