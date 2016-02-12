#' \code{EstimationData}
#' Selects the data from a data frame for estimation. Conducts imputation if necessary.
#' @param formula An object of class \code{\link{formula}} (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of type specification are given under ‘Details’.
#' @param data A \code{\link{data.frame}}.
#' @param subset An optional vector specifying a subset of observations to be used in the fitting process, or,
#' the name of a variable in \code{data}. It may not be an expression.
#' \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or,
#' the name of a variable in \code{data}. It may not be an expression.
#' @param missing How missing data is to be treated in the regression. Options are:
#' \code{"Error if missing data"}, \code{"Exclude cases with missing data"},
#' \code{"Use partial data (pairwise correlations)"},and \code{"Imputation (replace missing values with estimates)"}.
#' @export
EstimationData <- function(formula, data, subset = NULL,
                             weights = NULL,
                             missing = "Exclude cases with missing data")
{
    # Cleaning weightes and subsets.
    if (!is.null(subset))
        subset[is.na(subset)] <- FALSE
    if (!is.null(weights))
        weights[is.na(weights)] <- 0
    unfiltered.weights <- weights
    # Selecting the relevant variables from the data frame (unless imputation is being used).
    variable.names <- all.vars(formula)
    if (missing != "Imputation (replace missing values with estimates)")
        data.for.model <- data[ ,variable.names]
    # Addressing missing values.
    data.post.missing.value.treatment <- switch(missing, "Error if missing data" = ErrorIfMissingDataFound(data.for.model),
                   "Exclude cases with missing data" = ExcludeCasesWithAnyMissingData(data.for.model),
                   "Use partial data (pairwise correlations)" = data.for.model[complete.cases(data.for.model),],
                   "Imputation (replace missing values with estimates)" = SingleImputation(formula, data))
    post.missing.data.estimation.sample <- row.names(data) %in% rownames(data.post.missing.value.treatment)
    data[post.missing.data.estimation.sample, ] <- data.post.missing.value.treatment
    estimation.subset <- flipU::IfThen(hasSubset(subset),
        subset[post.missing.data.estimation.sample],
        rep(TRUE, nrow(data.post.missing.value.treatment)))
    if (!is.null(weights))
    {
        weights <- weights[post.missing.data.estimation.sample]
        estimation.subset <- estimation.subset & weights > 0
        weights <- weights[estimation.subset]
    }
    estimation.data <- data.post.missing.value.treatment[estimation.subset, variable.names] #Removing variables not used in the formula.
    missing.data.proportion <- 1 - nrow(estimation.data)/ ifelse(hasSubset(subset), sum(subset), nrow(data))
    if (missing.data.proportion > 0.20)
        warning(paste(FormatAsPercent(missing.data.proportion), "of the data is missing and has been excluded from the analysis.",
                      "Consider either filters to ensure that the data that is missing is in-line with your expectations,",
                      "or, set 'Missing Data' to another option."))
    if (missing != "Imputation (replace missing values with estimates)")
        estimation.data <- estimation.data[ ,variable.names]

    list(estimation.data = estimation.data,
         weights = weights,
         unfiltered.weights = unfiltered.weights,
         missing.data.proportion = missing.data.proportion,
         post.missing.data.estimation.sample = post.missing.data.estimation.sample,
         estimation.subset = estimation.subset, data = data, subset = subset)
}


#' \code{SingleImputation}
#' @description This is a wrapper for \code{\link{mice}}.
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}}.
#' @param ... Optional arguments for \code{\link{mice}}.
#'# @examples
#'# df <- data.frame(x = c(NA, 1), y = 1:2)
#'# ExcludeCasesWithAnyMissingData(df)
#' @export
SingleImputation <- function(formula, data, ...)
{
    outcome.name <- outcomeName(formula)
    if(!any(is.na(data)))
        warning("Imputation has been selected, but the data has no missing values, so nothing has been imputed.")
    requireNamespace("mice")
    library(mice)
    set.seed(12321) # Ensures that users do not have diferent outcomes each time.
    imputed.data<- mice::complete(mice::mice(data, seed = 12321, m = 1, printFlag = FALSE), 1)
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
#' @examples
#' df <- data.frame(x = c(NA, 1), y = 1:2)
#' ExcludeCasesWithAnyMissingData(df)
#' @export
ExcludeCasesWithAnyMissingData <- function(data)
{
    result <- na.omit(data)
    if(nrow(result) == 0)
        noData()
    result
}

#' \code{ExcludeCasesWithCompletelyMissingData}
#' @description Create a copy of a data frame where any cases which have only missing values
#' are removed.
#' @param data A \code{\link{data.frame}}.
#' @examples
#' my.df <- data.frame("A" = c(1, 2, 3, 4, NA), "B" = c(NA, 1, 2, 3, NA), "C" = c(NA, NA, 1, 2, NA))
#' ExcludeCasesWithCompletelyMissingData(my.df)
#' @export
ExcludeCasesWithCompletelyMissingData <- function(data)
{
    result <- data[rowSums(is.na(data)) < ncol(data), ]
    if (nrow(result) == 0)
    {
        noData()
    }
    return(result)
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
    flipU::IfThen(any(is.na(data)), missingDataFail(), data)
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


#' \code{MissingValuesByVariable}
#' @description Computes the number of missing values by variable.
#' @param data A \code{\link{data.frame}}.
#' @export
MissingValuesByVariable <- function(data)
{
    n <- nrow(data)
    missing <- apply(is.na(data), 2, sum)
    result <- data.frame("Missing" = missing,
        proportion = FormatAsPercent(missing / n))
    names(result)[2] <- paste0("Percent (of ", n, ")")
    result}

