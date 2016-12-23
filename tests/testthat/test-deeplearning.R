context("Deep learning")
data("cola", package="flipExampleData")
data(BostonHousing, package="mlbench")
data(BreastCancer, package="mlbench")

test_that("Categorical predictor", {
    #mod1 <- DeepLearning(Q17~Q12+Q13+Q14+Q15, data=cola)
    mod1 <- DeepLearning(Class~Cl.thickness+Cell.size+Cell.shape+Marg.adhesion+Epith.c.size+Bare.nuclei+Bl.cromatin+Normal.nucleoli+Mitoses,
                         data=BreastCancer)
    pb1 <- flipData::Probabilities(mod1)
    spb1 <- rowSums(pb1)
    expect_equal(ncol(pb1), 2)
    expect_equal(all(abs(1 - spb1[!is.na(spb1)]) < 1e-6), TRUE)
})

test_that("Missing values", {
    mod2 <- DeepLearning(Q17~Q7_1+Q7_2+Q7_3+Q7_4, data=cola)
    pred2 <- predict(mod2)
    pb2 <- rowSums(flipData::Probabilities(mod2))
    expect_equal(class(pred2), "factor")
    expect_equal(sum(!is.na(pred2)), 13)
    expect_equal(all(abs(1 - pb2[!is.na(pb2)]) < 1e-6), TRUE)
})

test_that("Regression", {
    mod3 <- DeepLearning(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat, data=BostonHousing)
    pred3 <- predict(mod3)
    expect_equal(class(pred3), "numeric")
})

test_that("Filter", {
    ids <- as.numeric(BreastCancer$Id)
    filt = ids < 1238000
    none.missing <- apply(BreastCancer, 1, function(x){all(!is.na(x))})
    mod4 <- DeepLearning(Class~Cl.thickness+Cell.size+Cell.shape+Marg.adhesion+Epith.c.size+Bare.nuclei+Bl.cromatin+Normal.nucleoli+Mitoses,
                         data=BreastCancer, subset=filt)
    pred4 <- predict(mod4)
    expect_equal(length(pred4), nrow(BreastCancer))
    expect_equal(nrow(mod4$estimation.data), sum(filt & none.missing))
})




