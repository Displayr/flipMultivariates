#'  \code{BreuschPagan}Breusch-Pagan test for non-constant variance.
#'
#' @param Regression.object An object of class \code{\link{Regression}}.
#' @details Weights are ignored when conducting the test.
#' @export
BreuschPagan <- function(Regression.object)#, var.formula)
{   #Modified from car::ncvTest, to addess different size of input data.
#    data <- getCall(Regression.object)$data
#    if (!is.null(data)) {
    # print("cat")
   # subset <-
 #       data <- subset(Regression.object$model, subset)
#""        if (hasSubset(Regression.object$subset))
#            data <- subset(data, Regression.object$subset)
        #update(Regression.object, formula(Regression.object))#, na.action = "na.exclude",            data = data)
    #}
#    else update(Regression.object, formula(Regression.object))#, na.action = "na.exclude")
##    sumry <- summary(Regression.object)
    squared.residuals <- Regression.object$residuals[Regression.object$subset]^2
    #sum.squared.residuals <- sum(residuals^2)
 #   dispersion <- sum.squared.residuals / df.residual(Regression.object)
#    sigma <- sqrt(dispersion)
#    print("sigma")
#    print(sigma)
#    print(Regression.summary$sigma)
#    S.sq <- df.r * sigma^2/sum(!is.na(residuals))
#    print("S.sq")
#    print(S.sq)
#        residuals <-
    #print(length(residuals(Regression.object, type = "pearson")))
#    pearson.residuals <- residuals(Regression.object, type = "pearson")
  #  print(summary(pearson.residuals))
  #  print(length(pearson.residuals))



   # pearson.residuals <- pearson.residuals[subset]
    #print(summary(pearson.residuals))
    #print(length(pearson.residuals))
#    print()
 #   print(sum(squared.residuals) / df.residual(Regression.object))
#    mean.squared.error <- mean(squared.residuals) #* df.residual(Regression.object) / length(squared.residuals)
 #   print(mean.squared.error)
  #  stop("dog")
#print(sum(squared.residuals))
#print(mean(squared.residuals))
#print(mean(squared.residuals) * df.residual(Regression.object) / length(squared.residuals) )

    U <- squared.residuals / mean(squared.residuals)#mean.squared.error#sum(squared.residuals)
#     if (missing(var.formula)) {
    fitted.values <- Regression.object$fitted.values
#    print("length(U)")
#    print(length(U))
#    print(length(fitted.values))
    #print(Regression.object)
    #residuals <- residuals(Regression.object)#outcome <- outcomeVariableFromModel(Regression.object)

    # stop("dog")
        mod <- lm(U ~ fitted.values)#, subset = Regression.object$subset)
#        varnames <- "fitted.values"
        #var.formula <- ~fitted.values
 #       df <- 1
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
    result <- list(#formula = var.formula, formula.name = "Variance",
        ChiSquare = Chisq, Df = 1, p = pchisq(Chisq, 1, lower.tail = FALSE),
        test = "Non-constant Variance Score Test")
    class(result) <- "chisqTest"
 #   print(result)
    result
}
