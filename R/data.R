#
# # Extracts data from the environment of the formula, if it has not
# # been provided as an argument.
# getData <- function(formula, data)
# {
#     if (is.null(data)) # Extracting the data from the environment
#     {
#         variable.names <- all.vars(formula)
#         data <- environment(formula)
#         data <- as.data.frame(lapply(variable.names, function(x) {get(x, data)}))
#         names(data) <- variable.names
#     }
#     else if (!is.data.frame(data))
#         stop("'data' must be a 'data.frame'.")
#     data
# }
#
# # Properties of data.
# outcomeName <- function(formula.object)
# {
#
#     if (hasOutcome(formula.object))
#         return(all.vars(formula.object)[1])
#     return(NULL)
# }
#
#
# hasOutcome <- function(formula.object)
# {
#     attr(terms(formula.object), "response") != 0
# }
#
# printDetails <- function(x)
# {
#     cat(paste0(deparse(substitute(x)), " n:", length(x), " valid:", sum(!is.na(x)), " missing:",sum(is.na(x)), "\n"))
#     print(summary(x))
#     cat("\n")
# }
#
# unclassIfNecessary <- function(x)
# {
#     if(is.factor(x))
#         return(unclass(x));
#     return(x);
# }
#
# anyNegative <- function(x)
# {
#     min(c(x, NA), na.rm = TRUE) < 0
# }
#
#
# isCount = function(x) {
#     if(is.factor(x))
#         return(FALSE)
#     if(!is.numeric(x)) {
#         if (!is.character(x))
#             x <- x$type
#         return(x == "Poisson" | x == "Quasi-Poisson" | x == "NBD")
#     }
#     x <- x[!is.na(x)]
#     if (length(x) == 0)
#         stop("No data.")
#     u = unique(x)
#     if (min(u, na.rm = TRUE) < 0)
#         return(FALSE)
#     sum(as.integer(u) != u, na.rm = TRUE) == 0}
#
#
# outcomeVariable <- function(formula.object, data)
# {
#     data[[outcomeName(formula.object)]]
# }
#
# outcomeVariableFromModel <- function(Regression.object)
# {
#     form <- formula(Regression.object)
#     Regression.object$model[, outcomeName(form)]
# }
#
# hasSubset <- function(subset) {!is.null(subset) & length(subset) != 1}
#
# #
