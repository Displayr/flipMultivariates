context("Weighted computations")

test_that("WeightedSVD",
{
    set.seed(123)
    z <- matrix(runif(21), ncol = 3)
    zWeight <- c(4, rep(1, 6))
    z1 <- rbind(z[rep(1:7, zWeight), ])

    # rnd <- function(x) round(abs(x), 5)

#     expect_equal(svd(z1), WeightedSVD(z, zWeight))
#     expect_equal(svd(z1, nu = 0L), WeightedSVD(z, zWeight, nu = 0L))
})
