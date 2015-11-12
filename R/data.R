# Properties of data.

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
    if (min(u) < 0)
        return(FALSE)
    sum(as.integer(u) != u) == 0}

dependentName <- function(formula.object)
{
    all.vars(formula.object)[1]
}


dependentVariable <- function(formula.object, data)
{
    data[[dependentName(formula.object)]]
}

