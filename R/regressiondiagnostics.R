

#'  \code{BreuschPagan}Breusch-Pagan test for non-constant variance.
#'
#' @param Regression.object An object of class \code{\link{Regression}}.
#' @details Weights are ignored when conducting the test.
#' @details show.warnings Returns a warning if the sample size is less than 4.
#' @export
BreuschPagan <- function(Regression.object, show.warnings = TRUE)#, var.formula)
{
    if (class(Regression.object)[1] != "Regression")
        stop("Breusch-Pagan Test has only been written for Regression objects.")
    #Modified from car::ncvTest, to addess different size of input data.
    subset <- Regression.object$flip.subset
    if(sum(subset) < 4)
    {
        if (show.warnings)
            warning("Sample size too small for Breusch-Pagan test.")
        return(list(ChiSquare = NA, Df = NA, p = 1,
            test = "Breusch-Pagan test of Non-constant Variance"))
    }
    residuals <- residuals(Regression.object)[subset]
    squared.residuals <- residuals^2
    U <- squared.residuals / mean(squared.residuals)#mean.squared.error#sum(squared.residuals)
    fitted.values <- fitted.values(Regression.object)[subset]
    mod <- lm(U ~ fitted.values)#, subset = Regression.object$subset)
    SS <- anova(mod)$"Sum Sq"
    RegSS <- sum(SS) - SS[length(SS)]
    Chisq <- RegSS/2
    result <- list(#formula = var.formula, formula.name = "Variance",
        ChiSquare = Chisq, Df = 1, p = pchisq(Chisq, 1, lower.tail = FALSE),
        test = "Breusch-Pagan test of Non-constant Variance")
    class(result) <- "chisqTest"
    result
}
#
# breusch.pagan <- function(Regression.object)
# {   #Modified from car::ncvTest, to addess different size of input data.
# #    data <- getCall(Regression.object)$data
# #    if (!is.null(data)) {
#     # print("cat")
#     Regression.summary = summary.lm(Regression.object)
#         data <- Regression.object$model
#         if (hasSubset(Regression.object$subset))
#             data <- subset(data, Regression.object$subset)
#         #update(Regression.object, formula(Regression.object))#, na.action = "na.exclude",            data = data)
#     #}
# #    else update(Regression.object, formula(Regression.object))#, na.action = "na.exclude")
# ##    sumry <- summary(Regression.object)
#     residuals <- residuals(Regression.object, type = "pearson")
#     printDetails(residuals)
#     S.sq <- df.residual(Regression.object) * (Regression.summary$sigma)^2/sum(!is.na(residuals))
#     printDetails(S.sq)
#     U <- (residuals^2)/S.sq
# #     if (missing(var.formula)) {
#     fitted.values <- fitted.values(Regression.object)
#     #print(Regression.object)
#     #residuals <- residuals(Regression.object)#outcome <- outcomeVariableFromModel(Regression.object)
#
#     # stop("dog")
#         mod <- lm(U ~ fitted.values, subset = Regression.object$subset)
#         varnames <- "fitted.values"
#         var.formula <- ~fitted.values
#         df <- 1
# #     }
# #     else {
# #         form <- as.formula(paste(".U ~ ", as.character(var.formula)[[2]],
# #             sep = ""))
# #         mod <- if (!is.null(data)) {
# #             data$.U <- .U
# #             lm(form, data = data)
# #         }
# #         else lm(form)
# #         df <- sum(!is.na(coefficients(mod))) - 1
# #     }
#     SS <- anova(mod)$"Sum Sq"
#     RegSS <- sum(SS) - SS[length(SS)]
#     Chisq <- RegSS/2
#     result <- list(formula = var.formula, formula.name = "Variance",
#         ChiSquare = Chisq, Df = df, p = pchisq(Chisq, df, lower.tail = FALSE),
#         test = "Non-constant Variance Score Test")
#     class(result) <- "chisqTest"
#     result
# }
