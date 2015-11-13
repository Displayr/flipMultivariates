#' \code{DichotomizeFactor} Converts a list of variable or data frames into a data.frame.
#'
#' @param list.of.variables A variable in a DataSet or data.frame.
#' @param coerce.to.numeric Makes factors and ordered factors numeric.
#' @export
DichotomizeFactor <- function(variable, cutoff = 0.5, warning = FALSE, variable.name = deparse(substitute(x))) {
    if (!is.factor(variable))
        variable = factor(variable)
    if (nlevels(variable) == 1)
        stop(paste(deparse(substitute(variable)), "cannot be dichotimized as it only contains one level."))
    else if (nlevels(variable) == 2)
        return(variable)
    cumulative.probs = cumsum(prop.table(table(variable)))
    cut.point = match(TRUE, cumulative.probs >= cutoff)
    if (cut.point == 1)
        stop(paste(variable.name, "cannot be dichotimized (e.g., perhaps only has 1 value)."))
    new.factor = factor(unclass(variable) >= cut.point)
    levels(new.factor) = paste0(c("<=",">="), levels(variable)[c(cut.point - 1, cut.point)])
    if (warning)
        warning(paste(variable.name, "has been dichotimized into", paste(levels(new.factor), collapse = " & ")))
    new.factor
}

#' @export
CreatingBinaryDependentVariableIfNecessary <- function(formula, data)
{
    CreatingBinaryVariableIfNecessary(data, dependentName(data, variable.name))
}

#' @export
CreatingBinaryVariableIfNecessary <- function(data, variable.name)
{
    variable <- data[[variable.name]]
    n.unique <- length(unique(variable))
    if (n.unique < 2)
        stopTooFewForBinary()
    else
    {
        if (n.unique > 2) {
            if(!is.factor(variable))
                variable <- factor(variable)
            if (nlevels(variable) > 2)
                variable <- DichotomizeFactor(variable, warning = TRUE, variable.name = variable.name)
            data[[variable.name]] <- variable
        }
    }
    data
}

#' \code{FactorToIndicators} Converts a factor into an indicator matrix.
#'
#' @param variable A variable in a DataSet or data.frame.
#' @param variable.name The name of the variable.
#' @export
FactorToIndicators <- function(variable, variable.name = deparse(substitute(variable)))
{
    indicators <- model.matrix( ~ variable - 1)
    if (nrow(indicators) == length(variable)) {
        colnames(indicators) = paste0(variable.name, ".", levels(variable))
        return(indicators)
    }
    new.indicators <- matrix(NA, length(variable), ncol(indicators))
    row.names <- as.numeric(dimnames(indicators)[[1]])
    colnames(new.indicators) <- colnames(indicators)
    new.indicators[row.names, ] <- indicators
    return(new.indicators);
}

#' \code{OrderedToNumeric} Converts an ordered factor into a numeric variable
#'
#' @param variable A variable in a DataSet or data.frame.
#' @param variable.name The name of the variable.
#' @export
OrderedToNumeric <- function(ordered.factor)
{
    return(unclass(ordered.factor))
}

#' \code{FactorToNumeric} Converts a an ordered factor into a numeric variable
#'
#' @param variable A variable in a DataSet or data.frame.
#' @param variable.name The name of the variable.
#' @export
FactorToNumeric <- function(variable, variable.name = deparse(substitute(variable)))
{
    if (is.ordered(variable))
        return(OrderedToNumeric(variable))
    return(FactorToIndicators(variable, variable.name))
}


dataFrameToVariableIfAppropriate <- function(data.frame.object) {
    if(is.data.frame(data.frame.object))
        return(data.frame.object[,1])
data.frame.object}


#' \code{ListToDataFrame} Converts a list of variable or data frames into a data.frame.
#'
#' @param list.of.variables A variable in a DataSet or data.frame.
#' @param coerce.to.numeric Makes factors and ordered factors numeric.
#' @export
ListToDataFrame <- function(list.of.variables, coerce.to.numeric = FALSE) {
    result <- NULL
    nms = names(list.of.variables)
    counter <- 0
    for (variable in list.of.variables) {
        counter <- counter + 1
        variable.name = nms[counter]
        if(is.character(variable))
            stop(paste0("Variable ", counter, " is a Text variable. It needs to be converted to numeric data if to be used in cluster analysis."))
        if (is.data.frame(variable)) {
            transformed.variable = ListToDataFrame(variable, coerce.to.numeric)
            colnames(transformed.variable ) = paste0(variable.name, " ", colnames(transformed.variable ))
        } else {
            if (coerce.to.numeric & is.factor(variable)) {
                transformed.variable = FactorToNumeric(variable, variable.name)
            } else {
                    transformed.variable = variable
            }
        }
        result <- cbind(result, transformed.variable)
        if(is.null(ncol(transformed.variable)))
            colnames(result)[ncol(result)] <- variable.name
       }
result}

