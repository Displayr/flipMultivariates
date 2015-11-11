# Exceptions and warnings

StopIfNotCount = function(variable, variable.name) {
    if(!isCount(variable))
        stop(paste(variable.name,
                         "is not a count (i.e., it has negative or non-integer values."))
    }

WarningFactorToNumeric <- function() {warning("Outcome variable is a factor; it has been made numeric.")}

WarningMakingNumericBinary <- function() {warning("Outcome variable is numeric and contains more than two unique
                                                  values. It has been xxxis a factor; it has been made numeric.")}
