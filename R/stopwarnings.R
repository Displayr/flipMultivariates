# Exceptions and warnings

StopIfNotCount = function(variable, variable.name) {
    if(!isCount(variable))
        stop(paste(variable.name,
                         "is not a count (i.e., it has negative or non-integer values."))
    }

WarningFactorToNumeric <- function() {warning("Dependent variable is a factor; it has been made numeric.")}
