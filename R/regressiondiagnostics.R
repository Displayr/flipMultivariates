

#'  \code{BreuschPagan}Breusch-Pagan test for non-constant variance.
#'
#' @param Regression.object An object of class \code{\link{Regression}}.
#' @details Weights are ignored when conducting the test.
#' @export
BreuschPagan <- function(Regression.object)#, var.formula)
{   #Modified from car::ncvTest, to addess different size of input data.
    subset <- Regression.object$flip.subset
    residuals <- residuals(Regression.object)[subset]
    printDetails(residuals)
    squared.residuals <- residuals^2
    U <- squared.residuals / mean(squared.residuals)#mean.squared.error#sum(squared.residuals)
    printDetails(U)
    fitted.values <- fitted.values(Regression.object)[subset]
    printDetails(fitted.values)
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
