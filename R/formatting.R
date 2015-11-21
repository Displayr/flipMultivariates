#' \code{FormatAsPercent}
#' \Description  Converts a proportion to a percent and
#' formats it nicely and with a percentage sign.
#' @param x The number(s)
#' @param digits Number of significant digits
#' @param format See \code{\link{formatC}}.
#' @param suffix The text to appear after a percent. Defaults to %.
#' @param ... Additional arguments that are passed to  \code{\link{formatC}}.

#' @export

FormatAsPercent <- function(x, digits = 2, format = "fg", suffix = "%",  ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), suffix)
}

#' \code{FormatAsReal} Formats real numbers nicely, as if they are to presented as a an average.
#' @param x The number(s)
#' @param digits Number of significant digits
#' @param format See \code{\link{formatC}}.
#' @param ... Additional arguments that are passed to  \code{\link{formatC}}.

#' @export

FormatAsReal <- function(x, digits = 2, format = "fg", ...) {
    formatC(x, digits = digits, format = format ,...)
}


#' \code{Equation} Formats a model as an equation (string), for
#' description purposes.
#' @param object The model.
#' @export
setGeneric("Equation",  function(object)
{
    variable.names <- names(object$model)
    predictor.names <- c("", nms[-1])
    equation <- paste0(variable.names[1], " = ",
                       paste0(coefs(object), variable.names, sep = " + "))
    strwrap(equation)
})


