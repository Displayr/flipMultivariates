#' @export
#' @importFrom stats na.pass
predict.LDA <- function(object, newdata = object$model, na.action = na.pass, ...)
{
    ldaExtractVariables(object, "class", object$prior, newdata, na.action, ...)
}

#' @importFrom stats na.pass
#' @export
Probabilities.LDA <- function(x)
{
    ldaExtractVariables(x, "posterior", x$prior, newdata = x$model, na.action = na.pass)
}

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

#' @export
#' @importFrom stats na.pass
predict.RandomForest <- function(object, newdata = object$model, na.action = na.pass, ...)
{
    randomForestExtractVariables(object, "response", newdata = object$model)
}

#' @importFrom stats na.pass
#' @export
Probabilities.RandomForest <- function(x)
{
    randomForestExtractVariables(x, "prob", newdata = x$model)
}

#' @importFrom stats predict
randomForestExtractVariables <- function(object, type, newdata = object$model)
{
    outcome.i <- match(object$outcome.name, names(newdata))
    newdata <- newdata[, -outcome.i]
    predict(object$original, type, newdata = newdata)
}

