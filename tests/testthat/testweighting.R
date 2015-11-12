library(testthat)
library(flipMultivariates)



dat = data.frame(x = 1:2, y = 2:1)
sm = sum(AdustDataToReflectWeights(dat, 2:1))
sm

test_that("Replicating data file with integer weights", {
    d = AdustDataToReflectWeights(dat, 2:1)
    expect_that(nrow(d), equals(3))
    expect_that(sum(d), equals(9))
})

test_that("Creating bootrapped sample with weights", {
    # Small sample.
    d = suppressWarnings(AdustDataToReflectWeights(dat, (2:1) / 10))
    expect_that(nrow(d), equals(1))
    expect_that(sum(d), equals(3))
    expect_warning(AdustDataToReflectWeights(dat, (2:1) / 10))

    # Moderate sample.
    d = suppressWarnings(AdustDataToReflectWeights(dat[rep(1:2,50),], runif(100)))
    expect_that(nrow(d), equals(50))
    expect_that(sum(d), equals(150))
    expect_warning(AdustDataToReflectWeights(dat[rep(1:2,50),], runif(100)))
})

