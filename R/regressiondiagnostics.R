

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
