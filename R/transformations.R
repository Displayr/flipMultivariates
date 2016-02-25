#' \code{DichotomizeFactor} Converts a list of variable or data frames into a
#' data.frame.
#'
#' @param variable A variable in a DataSet or data.frame.
#' @param cutoff The cutoff point to split the variable into.
#' @param warning If TRUE, raise a warning showing the new levels.
#' @param variable.name An alternate name to show instead of the deparsed
#'   variable name.
#' @export
DichotomizeFactor <- function(variable, cutoff = 0.5, warning = FALSE, variable.name = deparse(substitute(variable))) {
    if (!is.factor(variable))
        variable <- factor(variable)
    if (nlevels(variable) == 1)
        stop(paste(deparse(substitute(variable)), "cannot be dichotimized as it only contains one level."))
    else if (nlevels(variable) == 2)
        return(variable)
    cumulative.probs <- cumsum(prop.table(table(variable)))
    cut.point <- match(TRUE, cumulative.probs >= cutoff)
    if (cut.point == 1)
        stop(paste(variable.name, "cannot be dichotimized (e.g., perhaps only has 1 value)."))
    new.factor <- factor(unclass(variable) > cut.point)
    levels(new.factor) <- paste0(c("<=", ">="), levels(variable)[c(cut.point - 1, cut.point)])
    if (warning)
        warning(paste(variable.name, "has been dichotimized into", paste(levels(new.factor), collapse = " & ")))
    new.factor
}

#' @export
CreatingBinaryDependentVariableIfNecessary <- function(formula, data)
{
    outcome.name <- outcomeName(formula)
    data[, outcome.name] <- CreatingBinaryVariableIfNecessary(data, outcome.name)
    data
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
        }
    }
    variable
}

#' Converts a factor into an indicator matrix.
#'
#' @param variable A variable in a \code{DataSet} or \code{data.frame}.
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
    return(new.indicators)
}

#' Converts an ordered factor into a numeric variable
#'
#' @param ordered.factor A variable in a \code{DataSet} or \code{data.frame}.
#' @export
OrderedToNumeric <- function(ordered.factor)
{
    return(unclass(ordered.factor))
}

#' Converts a an ordered factor into a numeric variable
#'
#' @param variable A variable in a \code{DataSet} or \code{data.frame}.
#' @param variable.name The name of the variable.
#' @export
FactorToNumeric <- function(variable, variable.name = deparse(substitute(variable)))
{
    if (is.ordered(variable))
        return(OrderedToNumeric(variable))
    return(FactorToIndicators(variable, variable.name))
}


dataFrameToVariableIfAppropriate <- function(data.frame.object)
{
    if(is.data.frame(data.frame.object))
        return(data.frame.object[, 1])
    data.frame.object
}
