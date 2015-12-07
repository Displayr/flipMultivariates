#' \code{SingleImputation}
#' @description This is a wrapper for \code{\link{mice}}.
#' @param data A \code{\link{data.frame}}.
#' @param outcome.name A If the name of the outcome (outcome)
#' variable is provided, any cases with missing values on this variable
#' are excluded from the final data file.
#' @param ... Optional arguments for \code{\link{mice}}.
#'# @examples
#'# df <- data.frame(x = c(NA, 1), y = 1:2)
#'# ExcludeCasesWithAnyMissingData(df)
#' @export
SingleImputation <- function(formula, data, outcome.name = NULL, ...)
{
    if(!any(is.na(data)))
        warning("Imputation has been selected, but the data has no missing values, so nothing has been imputed.")
    requireNamespace("mice")
    set.seed(12321) # Ensures that users do not have diferent outcomes each time.
    imputed.data<- complete(mice(data, seed = 12321, m = 1, printFlag = FALSE), 1)
    if (!is.null(outcome.name))
    {
        valid.dependent <- !is.na(data[, outcome.name])
        return(imputed.data[valid.dependent, ]) # Excluding observations with missing values.
    }
    return(imputed.data)
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
    result <- na.omit(data)
    if(nrow(result) == 0)
        noData()
    result
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
    #tt <- try(suppressWarnings(na.fail(data)))
    ifThen(any(is.na(data)), missingDataFail(), data)
}


notAvailable <- function(unavailable.function.name)
{
    stop(paste(unavailable.function.name,"is not available for this analysis. Please contact support if you believe this option should be available."))
}

noData <- function()
{
    stop("All observations contains some missing data, so an analysis is not possible. Check to see if there are any 'missing' options that can be used.")
}

missingDataFail <- function()
{
    stop("The data contains missing values. Change the 'missing' option to run the analysis.")
}

