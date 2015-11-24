#' Converts a proportion to a percent and
#' formats it nicely and with a percentage sign.
#'
#' @param x The number(s)
#' @param digits Number of significant digits
#' @param format See \code{\link{formatC}}.
#' @param suffix The text to appear after a percent. Defaults to \%.
#' @param ... Additional arguments that are passed to \code{\link{formatC}}.

#' @export

FormatAsPercent <- function(x, digits = 2, format = "fg", suffix = "%",  ...)
{
    paste0(formatC(100 * x, format = format, digits = digits, ...), suffix)
}

#' Formats real numbers nicely, as if they are to be presented as an average.
#'
#' @param x The number(s)
#' @param digits Number of significant digits
#' @param format See \code{\link{formatC}}.
#' @param ... Additional arguments that are passed to \code{\link{formatC}}.

#' @export

FormatAsReal <- function(x, digits = 2, format = "fg", ...)
{
    formatC(x, digits = digits, format = format ,...)
}


#' Formats a model as an equation (string), for
#' description purposes.
#'
#' @param object The model.
#' @export
setGeneric("Equation", function(object)
{
    coefs <- coef(object)
    parameter.names <- names(coefs)
    dependent.name <- parameter.names[1]
    parameter.names[1] <- "" #Intercept.
    signs <- sign(coefs)
    operator <- rep(" + ", length(signs))
    if (sum(signs == -1) > 0)
        operator[signs == -1] <- " - "
    operator[1] <- ifelse(signs[1] == 1, "", " -")
    coefs <- FormatAsReal(abs(coefs))
    equation <- paste0(dependent.name, " = ",
                       paste0(operator, coefs, parameter.names, collapse = ""))
    strwrap(equation)
})
