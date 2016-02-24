# Properties of data.
outcomeName <- function(formula.object)
{

    if (hasOutcome(formula.object))
        return(all.vars(formula.object)[1])
    return(NULL)
}


hasOutcome <- function(formula.object)
{
    attr(terms(formula.object), "response") != 0
}

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

unclassIfNecessary <- function(x)
{
    if(is.factor(x))
        return(unclass(x));
    return(x);
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


outcomeVariable <- function(formula.object, data)
{
    data[[outcomeName(formula.object)]]
}

outcomeVariableFromModel <- function(Regression.object)
{
    form <- formula(Regression.object)
    Regression.object$model[, outcomeName(form)]
}

hasSubset <- function(subset) {!is.null(subset) & length(subset) != 1}


