#' @export
#' @importFrom stats predict na.pass
predict.LDA <- function(object, newdata = object$model, na.action = na.pass, ...)
{
    ldaExtractVariables(object, "class", newdata, na.action, ...)
}


#' @export
Probabilities.LDA <- function(x)
{
    ldaExtractVariables(x, "posterior", newdata = x$model, na.action = na.pass)
}


#' @export
DiscriminantVariables <- function(x)
{
    ldaExtractVariables(x, "x", newdata = x$model, na.action = na.pass)
}

ldaExtractVariables <- function(object, type, newdata = object$model, na.action = na.pass, ...)
{
    outcome.i <- match(object$outcome.name, names(newdata))
    newdata <- newdata[, -outcome.i]
    suppressWarnings(predict(object$original, newdata = newdata, na.action = na.action)[[type]])
}
