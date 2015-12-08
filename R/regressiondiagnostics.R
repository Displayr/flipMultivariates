breusch.pagan <- function(Regression.object, Regression.summary, var.formula)
{   #Modified from car::ncvTest, to addess different size of input data.
#    data <- getCall(Regression.object)$data
#    if (!is.null(data)) {
    # print("cat")
        data <- Regression.object$model
        if (hasSubset(Regression.object$subset))
            data <- subset(data, Regression.object$subset)
        #update(Regression.object, formula(Regression.object))#, na.action = "na.exclude",            data = data)
    #}
#    else update(Regression.object, formula(Regression.object))#, na.action = "na.exclude")
##    sumry <- summary(Regression.object)
    residuals <- residuals(Regression.object, type = "pearson")
    S.sq <- df.residual(Regression.object) * (Regression.summary$sigma)^2/sum(!is.na(residuals))
    U <- (residuals^2)/S.sq
#     if (missing(var.formula)) {
    fitted.values <- fitted.values(Regression.object)
    #print(Regression.object)
    #residuals <- residuals(Regression.object)#outcome <- outcomeVariableFromModel(Regression.object)

    # stop("dog")
        mod <- lm(U ~ fitted.values, subset = Regression.object$subset)
        varnames <- "fitted.values"
        var.formula <- ~fitted.values
        df <- 1
#     }
#     else {
#         form <- as.formula(paste(".U ~ ", as.character(var.formula)[[2]],
#             sep = ""))
#         mod <- if (!is.null(data)) {
#             data$.U <- .U
#             lm(form, data = data)
#         }
#         else lm(form)
#         df <- sum(!is.na(coefficients(mod))) - 1
#     }
    SS <- anova(mod)$"Sum Sq"
    RegSS <- sum(SS) - SS[length(SS)]
    Chisq <- RegSS/2
    result <- list(formula = var.formula, formula.name = "Variance",
        ChiSquare = Chisq, Df = df, p = pchisq(Chisq, df, lower.tail = FALSE),
        test = "Non-constant Variance Score Test")
    class(result) <- "chisqTest"
    result
}
