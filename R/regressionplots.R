#' \code{Equation} Formats a model as an equation (string), for
#' description purposes.
#' @param Regression.object The model.
#' @export
setGeneric("PredictionPlot", function(Regression.object)
{
    equation <- Equation(Regression.object)
    y.name = names(Regression.object$model)[1]
    x.name <- names(Regression.object$model)[2]
    x <- Regression.object$model[[2]]#model.matrix(Regression.object)[, 2]
    y <- Regression.object$model[[1]]
    if(length(Regression.object$model) != 2)
        warning("Only the first predictor variable is shown on this plot")
    plt <- plot(x,  y, xlab = x.name, ylab = y.name, main = equation)
    plt <- abline(Regression.object)
    print(plot)
})
