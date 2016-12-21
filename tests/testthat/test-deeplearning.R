context("Deep learning")
data("cola", package="flipExampleData")
data(BostonHousing, package="mlbench")

test_that("Categorical predictor", {
    mod1 <- DeepLearning(Q17~Q12+Q13+Q14+Q15, data=cola)
    pb1 <- Probabilities.DeepLearning(mod1)
    expect_equal(ncol(pb1), 2)
})

test_that("Missing values", {
    mod2 <- DeepLearning(Q17~Q7_1+Q7_2+Q7_3+Q7_4, data=cola)
    pred2 <- predict(mod2)
    expect_equal(class(pred2), "factor")
    expect_equal(sum(!is.na(pred2)), 13)
})

test_that("Regression", {
    mod3 <- DeepLearning(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat, data=BostonHousing)
    pred3 <- predict(mod3)
    expect_equal(class(pred3), "numeric")
})




