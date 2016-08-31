#' @export
#' @importFrom stats predict.glm
predict.LDA <- function(object, newdata = object$model, na.action = na.pass, ...)
{
    outcome.i <- match(object$outcome.name, names(newdata))
    newdata <- newdata[, -outcome.i]
    suppressWarnings(predict(object$original, newdata = newdata, na.action = na.action)$class)
}


