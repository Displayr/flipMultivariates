context("Deep learning")
data("cola", package="flipExampleData")
data(BostonHousing, package="mlbench")

test_that("Categorical predictor", {
    mod1 <- DeepLearning(Q17~Q12+Q13+Q14+Q15, data=cola)
    pred1 <- predict(mod1)
    expect_that(ncol(pred1), 2)
})

test_that("Missing values", {
    mod2 <- DeepLearning(Q17~Q7_1+Q7_2+Q7_3+Q7_4, data=cola)
    pred2 <- predict(mod2)
    expect_equal(nrow(pred2), nrow(cola))
    expect_equal(sum(!is.na(pred2[,1])), 13)
})

test_that("Regression", {
    mod3 <- DeepLearning(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat, data=BostonHousing)
    pred3 <- predict(mod3)
    expect_equal(ncol(pred3), 1)
})




