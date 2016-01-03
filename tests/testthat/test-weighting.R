context("Weighting")

dat <- data.frame(x = 1:2, y = 2:1)
sm <- sum(AdjustDataToReflectWeights(dat, 2:1))
# sm

test_that("Replicating data file with integer weights", {
    d = AdjustDataToReflectWeights(dat, 2:1)
    expect_that(nrow(d), equals(3))
    expect_that(sum(d), equals(9))
})

test_that("Creating bootrapped sample with weights", {
    # Small sample.
    d <- suppressWarnings(AdjustDataToReflectWeights(dat, (2:1) / 10))
    expect_that(nrow(d), equals(1))
    expect_that(sum(d), equals(3))
    expect_warning(AdjustDataToReflectWeights(dat, (2:1) / 10))

    # Moderate sample.
    d <- suppressWarnings(AdjustDataToReflectWeights(dat[rep(1:2, 50), ], runif(100)))
    expect_that(nrow(d), equals(50))
    expect_that(sum(d), equals(150))
    expect_warning(AdjustDataToReflectWeights(dat[rep(1:2, 50), ], runif(100)))
})
