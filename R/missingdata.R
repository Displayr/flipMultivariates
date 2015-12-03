#' \code{SingleImputation}
#' @description This is a wrapper for \code{\link{mice}}.
#' @param data A \code{\link{data.frame}}.
#' @param dependent.name A If the name of the dependent (outcome)
#' variable is provided, any cases with missing values on this variable
#' are excluded from the final data file.}.
#' @param ... Optional arguments for \code{\link{mice}}.
#'# @examples
#'# df <- data.frame(x = c(NA, 1), y = 1:2)
#'# ExcludeCasesWithAnyMissingData(df)
#' @export
SingleImputaton <- function(data, dependent.name = NULL, ...)
{
    library(mice)
    set.seed(12321)
    imputed.data<- complete(mice(data, m = 1), 1)
    if (!is.null(dependent.name))
        imputed.data[!is.na(data[[dependent.name]]),] # Excluding observations with missing values.
}

ommitedRowNames <- function(data, row.names)
{
    omitted <- as.numeric(row.names[-match(rownames(data), row.names)])
    class(omitted) = "omit"
}


#' \code{ExcludeCasesWithAnyMissingData}
#' @description This is a wrapper for \code{\link{na.omit}}
#' @param data A \code{\link{data.frame}}.
#'# @examples
#'# df <- data.frame(x = c(NA, 1), y = 1:2)
#'# ExcludeCasesWithAnyMissingData(df)
#' @export
ExcludeCasesWithAnyMissingData <- function(data)
{
    result = na.omit(data)
    ifelse(nrow(result) == 0, noData(), result)
}

#' \code{ErrorIfMissingDataFound}
#' @description This is a wrapper for \code{\link{na.fail}}
#' @param data A \code{\link{data.frame}}.
#'# @examples
#'# df = data.frame(x = c(NA, 1), y = 1:2)
#'# ErrorIfMissingDataFound(df)
#' @export
ErrorIfMissingDataFound <- function(data)
{
    tt <- try(na.fail(df))
    ifelse(is(tt,"try-error"), missingDataFail(), data)
}


notAvailable <- function(unavailable.function.name)
{
    stop(paste(unavailable.function.name,"is not available for this analysis. Please contact support if you believe this option should be available."))
}

noData <- function()
{
    stop("All observations contains some missing data, so an analysis is not possible. Check to see if there are any missing.data options that can be used.")
}

missingDataFail <- function()
{
    stop("The data contains missing values. Change the missing.data settings to run the analysis.")
}

