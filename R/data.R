# Properties of data.

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

