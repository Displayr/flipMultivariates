# Exceptions and warnings

stopIfNotCount = function(formula, data)
{
    dependent.name <- outcomeName(formula)
    dependent.variable <- data[[dependent.name]]
    if (!flipU::AllIntegers(dependent.variable))
        stop(paste("This analysis assumes that the Outcome variable contains only integers (i.e., whole numbers).
             However,", dependent.name, "contains non-integer values (i.e., numbers with decimal places)."))
    if(anyNegative(dependent.variable))
        stop(paste("This analysis assumes that the Outcome variable contains only values greater than or equal
            to zero. However", dependent.name, "contains negative values."))
}

warningSampleSizeTooSmall <- function() {"The sample size is too small for it to be possible to conduct the analysis."}

warningPredictorVariableDoesNotExist <- function() {"The specified predictor variable does not exist."}

warningRobustInappropriate <- function() {"Robust standard errors has been selected. This option has been ignored,
    as due to the use of weights, a survey-weighted model has been estimated, and this method contains a similar
    type of adjustment already"}

stopNotCount <- function() {stop("This analysis assumes that your Outcome variable is a count variable (e.g., number of products purchased per week). A count variable cannot contained either decimals nor negative variables. Your data is not consistent with this assumption.")}

WarningFactorToNumeric <- function() {warning("Outcome variable is a factor; it has been made numeric.
                                              Consider using another type of regression (e.g., Ordered Logit or Binary Logit).")}

WarningMakingNumericBinary <- function() {warning("Outcome variable is numeric and contains more than two unique
                                                  values. It has been xxxis a factor; it has been made numeric.")}

stopTooFewForBinary <- function() {warning("The Outcome variable needs to contain two or more categories. It does not.")}

warningNotOrdered <- function() {warning("Outcome variable is a not an Ordered Factor; it has been converted into an Ordered Factor.")}
