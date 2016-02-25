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
#' \code{FormatAsPValue}
#' Formats p relatively nicely, ensuring that values greater than 0.05
#' are not rounded to 0.05, and numbers greater than 0 are not rounded to 0,
#' unless is really close to 0.
#'
#' @param p The number(s)
#' @param p.cutoff TODO
#' @param ... Additional arguments that are passed to \code{\link{formatC}}.

#' @export
FormatAsPValue <- function(p, p.cutoff = 0.05, ...)
{
    n.digits <- 2
    if (p < 0)
        return("0")
    p.formatted <- formatC(p, digits = n.digits, format = "f" ,...)
    # Making sure values greater than 0.05 are not shown as 0.05 due to rounding.
    while(as.numeric(p.formatted) == p.cutoff)
    {
        n.digits <- n.digits + 1
        p.formatted <- formatC(p, digits = n.digits, format = "f" ,...)
    }
    # Making sure values greater than 0.05 are not shown as 0.05 due to rounding.
    while(as.numeric(p.formatted) == 0 & n.digits < 12)
    {
        n.digits <- n.digits + 1
        p.formatted <- formatC(p, digits = n.digits, format = "f" ,...)
    }
    p.formatted
}

#' Formats a model as an equation (string), for
#' description purposes. It shows the linear expectation of the mean, and assumes you have not manipulated
#' default assumptions (e.g., via offsets and link functions).
#'
#' @param object The model.
#' @export
setGeneric("Equation", function(object)
{
    coefs <- coef(object)
    parameter.names <- names(coefs)
    dependent.name <- outcomeName(formula(object$call))
    parameter.names[1] <- "" #Intercept.
    signs <- sign(coefs)
    operator <- rep(" + ", length(signs))
    if (sum(signs == -1) > 0)
        operator[signs == -1] <- " - "
    operator[1] <- ifelse(signs[1] == 1, "", " -")
    coefs <- FormatAsReal(abs(coefs))
    equation <- paste0(dependent.name, " = ",
                       paste0(operator, coefs, parameter.names, collapse = ""))
    if (object$type == "Poisson" || object$type == "Quasi-Poisson" )
        equation <- paste0("exp(", equation, ")")
    strwrap(equation)
})
