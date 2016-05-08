#' \code{CleanSubset}
#' Takes a QSubset variable and turns it into a logical vector with no missing values
#' @param subset A QSubset variable from Displayr or Q.
#' @param n.total The total number of observations.
#' @export

CleanSubset <- function(subset, n.total)
{
    subset.label <- attr(subset, "label")
    if (is.null(subset))
    {
        n.subset <- n.total
        subset <- rep(TRUE, n.total)
    }
    else
    {
        subset.length <- length(subset)
        if(subset.length == 1)
        {
            n.subset <- n.total
            subset <- rep(subset, n.total)
        }
        else
        {
            if (subset.length != n.total)
                stop("subset.length != n.total")
            subset[is.na(subset)] <- FALSE
            subset[is.na(subset)] <- FALSE
            n.subset <- sum(subset)

        }
    }
    attr(subset, "n.subset") = n.subset
    attr(subset, "label") = subset.label
    subset
}

#' \code{CleanWeights}
#' Takes a QSubset variable and turns it into a logical vector with no missing values
#' @param weights A weights variable from Displayr or Q (i.e., \code{QcalibratedWeight}
#' or \code{QPopulationWeight}).
#' @export

CleanWeights <- function(weights)
{
    if (is.null(weights))
        return(weights)
    weights[is.na(weights) | weights < 0] <- 0
    weights
}

#' \code{EstimationData} Selects the data from a data frame for estimation.
#' Conducts imputation if necessary.
#' @param formula An object of class \code{\link{formula}} (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#'   The details of type specification are given under \sQuote{Details}.
#' @param data A \code{\link{data.frame}}.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param missing How missing data is to be treated in the regression. Options
#'   are: \code{"Error if missing data"}, \code{"Exclude cases with missing
#'   data"}, \code{"Use partial data"}, \code{"Use partial data (pairwise
#'   correlations)"}, and \code{"Imputation (replace missing values with
#'   estimates)"}.
#' @export
EstimationData <- function(formula, data, subset = NULL,
                             weights = NULL,
                             missing = "Exclude cases with missing data")
{
    # Cleaning weightes and subsets.
    n.total <- nrow(data)
    subset <- CleanSubset(subset, n.total)
    n.subset <- attr(subset, "n.subset")
    if (weighted <- !is.null(weights))
    {
        weights <- CleanWeights(weights)
        weight.label <- attr(weights, "label")
    }
    unfiltered.weights <- weights
    # Filtering the data
    filter.ewerrfdfdsrew045 <- if (weighted) subset & weights > 0 else subset #Name to avoid bug in subset.data.frame
    data.subset <- subset(data, filter.ewerrfdfdsrew045)
    # Selecting the relevant variables from the data frame (unless imputation is being used).
    variable.names <- all.vars(formula)
    if (missing == "Imputation (replace missing values with estimates)")
    {
        # Imputation is performed only using the subset, as otherwise probelms can occurif the subset
        # is based on a range of values of a variable, and the imputation causes values outside this
        # range to be imputed.
        data.for.estimation = SingleImputation(data.subset, formula)
        imputation.label <- attr(data.for.estimation, "imputation.method")
        #Filtering for the whole data set (as if using only the non-filter,the sample may be too small)
        data$filter.ewerrfdfdsrew045 <- as.integer(filter.ewerrfdfdsrew045) # Adding the filter as a variable to assist the imputation (name is to avoid duplicates).
        data = SingleImputation(data)
        # Inserting the values imputed for estimation sample.
        estimation.sample <- row.names(data) %in% rownames(data.for.estimation)
        data[estimation.sample, ] = data.for.estimation
        data.for.estimation <- data.for.estimation[, variable.names]
    }
    else
    {
        data.subset <- data.subset[ ,variable.names]
        data.for.estimation <- switch(missing, "Error if missing data" = ErrorIfMissingDataFound(data.subset),
                   "Exclude cases with missing data" = removeCasesWithAnyNA(data.subset),
                   "Use partial data" = removeCasesWithAllNA(data.subset),
                   "Use partial data (pairwise correlations)" = removeCasesWithAllNA(data.subset))
        estimation.sample <- row.names(data) %in% rownames(data.for.estimation)
    }
    if (weighted)
        weights <- weights[estimation.sample]
    # Reporting.
    n.estimation <- sum(estimation.sample)
    if (n.estimation < length(variable.names))
        stop("Sample size is too small ")
    description <- SampleDescription(n.total, n.subset, n.estimation,
        attr(subset, "label"), weighted, weight.label, missing, imputation.label)
    list(estimation.data = data.for.estimation,
         weights = weights,
         unfiltered.weights = unfiltered.weights,
         post.missing.data.estimation.sample = estimation.sample,
         #estimation.subset = estimation.subset,
         data = data,
         subset = filter,
         description = description)
}


removeCasesWithAnyNA <- function(x)
{
    x[apply(is.na(x), 1, sum) == 0, ]
}

removeCasesWithAllNA <- function(x)
{
    x[apply(is.na(x), 1, sum) < ncol(x), ]
}


#' \code{SampleDescription}
#'
#' @description Describes the sample, for use as footers in multivariate
#'   analyses.
#' @param n.total  Total number of observations in the database.
#' @param n.subset Total number of observations in the subset (less than or
#'   equal to \code{n.total}).
#' @param n.estimation The total number of observations used in estimation (less
#'   than or equal to \code{subset}).
#' @param subset.label Total number of observations in the database.
#' @param weighted Total number of observations in the database.
#' @param weight.label Total number of observations in the database.
#' @param missing How missing data is to be treated in the analysis. Options
#'   are: \code{"Error if missing data"}, \code{"Exclude cases with missing
#'   data"}, ,and \code{"Imputation (replace missing values with estimates)"}.
#' @param imputation.label TODO
#'
#' @export
SampleDescription <- function(n.total, n.subset, n.estimation, subset.label, weighted = TRUE, weight.label = "", missing, imputation.label = NULL)
{
    # Warning if there is less than 50% data.
    missing.data.proportion <- 1 - n.estimation / n.subset
    if (missing.data.proportion > 0.50)
        warning(paste(FormatAsPercent(missing.data.proportion), "of the data is missing and has been excluded from the analysis.",
                      "Consider either filters to ensure that the data that is missing is in-line with your expectations,",
                      "or, set 'Missing Data' to another option."))
    # Creating description.
    missing.data <- n.estimation < n.subset
    description <- baseDescription(paste0("n = ", n.estimation," cases used in estimation"),
        n.total, n.subset, n.estimation, subset.label, weighted, weight.label)
    description <- paste(description, if(missing.data | missing == "Imputation (replace missing values with estimates)")
        switch(missing, "Error if missing data" = "",
                   "Exclude cases with missing data" = "Cases containing missing values have been excluded;",
                   "Imputation (replace missing values with estimates)" =
                       paste0("Missing values of predictor variables have been imputed using ", imputation.label, ";"))
        else "")
    description
}


baseDescription <- function(description.of.n, n.total, n.subset, n.estimation, subset.label, weighted = TRUE, weight.label = "")
{
    base <- paste0(
                ifelse(n.estimation < n.subset,
                    paste0(" of a total sample size of ", n.subset,
                        ifelse(n.subset < n.total,
                            paste0(" (", subset.label, ")"),
                            "")), ""),
                ";")

    paste0(description.of.n,base,
        ifelse(weighted,
               paste0(" Data has been weighted (", weight.label, ");"),
               ""))
}



#' \code{SingleImputation}
#' @description This is a wrapper for \code{\link{mice}}.
#' @param data A \code{\link{data.frame}}.
#' @param formula A \code{\link{formula}}. Where the formula contains a dependent variable,
#' observations with missing values on this variable are deleted after the imputation (von Hippel 2007).
#' @param method "mice" applies multivariate imputation by chained equations
#' (predictive mean matching) with the \code{\link{mice}} package. "hot deck" applies the \code{\link{hot.deck}} method.
#' The default setting is "try mice", which first applies the  \code{\link{mice}} method and,if an error
#' occurs, falls back to \code{\link{hot.deck}}.
#' @references von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#' Improved Strategy for Analyzing Multiply Imputed Data." Sociological Methodology 37:83-117.
#' Skyler J. Cranmer and Jeff Gill (2013). We Have to Be Discrete About This: A Non-Parametric Imputation Technique for Missing Categorical Data. British Journal of Political Science, 43, pp 425-449.

#' @export
SingleImputation <- function(data, formula = NULL, method = "try mice")
{
    .errorInImputation <- function(imputed.data, formula)
    {
        class(imputed.data) == "try-error" || AnyNA(imputed.data, formula)
    }
    if(!any(is.na(data)))
    {
        warning("Imputation has been selected, but the data has no missing values, so nothing has been imputed.")
        return(data)
    }

    if(method != "Hot deck")
    {
        imputed.data <- try({
            require("mice")
            mice::complete(mice::mice(data, seed = 12321, m = 1, printFlag = FALSE), 1)}, silent = TRUE)
        attr(imputed.data, "imputation.method") <- "chained equations (predictive mean matching)"
        if (method == "mice" && .errorInImputation(imputed.data, formula))
            stop("Mice imputation failed.")
    }
    if(method != "mice" && (method == "hot deck" || .errorInImputation(imputed.data, formula)))
    {
        set.seed(12321)
        imputed.data <- suppressWarnings(hot.deck::hot.deck(data, m = 1)$data[[1]])
        attr(imputed.data, "imputation.method") <- "hot decking"
    }
    if (.errorInImputation(imputed.data, formula))
        stop("Imputation has failed.")
    if (!is.null(formula))
    {
        outcome.name <- outcomeName(formula)
        if (!is.null(outcome.name))
        {
            valid.dependent <- !is.na(data[, outcome.name])
            imputed.data <- imputed.data[valid.dependent, ] # Excluding observations with missing values.
        }
    }
    return(imputed.data)
}



#' \code{AnyNA}
#'
#' Checks to see if the are any missing valus
#' @param data A \code{\link{data.frame}}.
#' @param formula A no optional \code{\link{formula}}. Variables not listed in a formula are excluded from the evaluation.
#'

#' @export
AnyNA <- function(data, formula = NULL)
{
    if (!is.null(formula))
    {
        data <- data[, all.vars(formula)]
    }
    any(is.na(data))
}

# k <- 1:ncol(dat)
# characters <- k[unlist(lapply(dat, is.character))]
# ordered <- unlist(lapply(dat, is.ordered))
# factors <- k[unlist(lapply(dat, is.factor)) & !ordered]
# ordered <- k[ordered]
# amelia(dat, noms = factors, ords = ordered, idvars = characters)
#
# mdf <- missing_data.frame(dat)
# mi(mdf)
#     a     b     c     d     e
# FALSE FALSE FALSE FALSE  TRUE
# > unlist(lapply(dat, is.ordered))
#     a     b     c     d     e
# FALSE FALSE FALSE  TRUE FALSE
# > amelia(dat)
# Amelia Error Code:  37
#  The following variable(s) are 'factors':
# c, d
# You may have wanted to set this as a ID variable to remove it
# from the imputation model or as an ordinal or nominal
# variable to be imputed.  Please set it as either and
# try again.



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
    if (any(is.na(data)))
        missingDataFail()
    data
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

