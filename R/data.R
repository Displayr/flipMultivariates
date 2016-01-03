# Properties of data.

printDetails <- function(x)
{
    cat(paste0(deparse(substitute(x)), " n:", length(x), " valid:", sum(!is.na(x)), " missing:",sum(is.na(x)), "\n"))
    print(summary(x))
    cat("\n")

}


allIntegers <- function(x)
{
    all(x%%1==0)
}


anyNegative <- function(x)
{
    min(c(x, NA), na.rm = TRUE) < 0
}


isCount = function(x) {
    if(!is.numeric(x))
        return(FALSE)
    u = unique(x)
    if (min(u, na.rm = TRUE) < 0)
        return(FALSE)
    sum(as.integer(u) != u, na.rm = TRUE) == 0}

outcomeName <- function(formula.object)
{
    all.vars(formula.object)[1]
}


outcomeVariable <- function(formula.object, data)
{
    data[[outcomeName(formula.object)]]
}

outcomeVariableFromModel <- function(Regression.object)
{
#     print(Regression.object)
#     print(Regression.object$call)
    formula <- as.list(Regression.object$call)$formula
    Regression.object$model[, outcomeName(formula)]
}

hasSubset <- function(subset) {!is.null(subset) & length(subset) != 1}


