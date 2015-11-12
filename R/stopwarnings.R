# Exceptions and warnings

stopIfNotCount = function(formula, data)
{
    dependent.name <- dependentName(formula)
    dependent.variable <- data[[dependent.name]]
    if (!allIntegers(dependent.variable))
        stop(paste("This analysis assumes that the Outcome variable contains only integers (i.e., whole numbers).
             However,", dependent.name, "contains non-integer values (i.e., numbers with decimal places)."))
    if(anyNegative(dependent.variable))
        stop(paste("This analysis assumes that the Outcome variable contains only values greater than or equal
            to zero. However", dependent.name, "contains negative values."))
}

warningNotCountAsNegatives <- function() {warning("This analysis assumes that your Outcome variable is a count variable
                                                  (e.g., number of products purchased per week. Your data is not ")}

WarningFactorToNumeric <- function() {warning("Outcome variable is a factor; it has been made numeric.
                                              Consider using another type of regression (e.g., Ordered Logit or Binary Logit).")}

WarningMakingNumericBinary <- function() {warning("Outcome variable is numeric and contains more than two unique
                                                  values. It has been xxxis a factor; it has been made numeric.")}

stopTooFewForBinary <- function() {warning("The Outcome variable needs to contain two or more categories. It does not.")}

warningNotOrdered <- function() {warning("Outcome variable is a not an Ordered Factor; it has been converted into an Ordered Factor.")}

warningWeightsBootstrapped <- function()
{
    warning("Weights have been applied, but the algorithm you have selected
             is only able to use integer valued weights.\n",
      	   "A bootstrapped version of the dataset was constructed using
            the weights as sample probabilities.\n\n")
}
