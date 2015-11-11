asPercent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

asMean <- function(x, digits = 2, format = NULL ,...) {
    formatC(x, digits = digits, format = format ,...)
}
