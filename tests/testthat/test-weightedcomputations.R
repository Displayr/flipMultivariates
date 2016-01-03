context("Weighted computations")

test_that("WeightedSVD",
{
    set.seed(123)
    z <- matrix(runif(21), ncol = 3)
    zWeight <- c(4, rep(1, 6))
    z1 <- rbind(z[rep(1:7, zWeight), ])
    .removeSigns <- function(x)
    {
        n <- length(x)
        for (i in 1:n)
            x[[i]] <- abs(x[[i]])
        x
    }

    # rnd <- function(x) round(abs(x), 5)
     a <- (.removeSigns(svd(z1)))
     b <- (.removeSigns(WeightedSVD(z, zWeight)))
     expect_equal(a$d[1], b$d[1], tolerance = 1.0e-8)
     expect_equal(a$v[1,2], b$v[1,2], tolerance = 1.0e-8)
     expect_equal(a$u[5,2], b$u[2,2], tolerance = 1.0e-8)
     a <- (.removeSigns(svd(z1, nu = 0L)))
     b <- (.removeSigns(WeightedSVD(z, zWeight, nu = 0L)))
     expect_equal(a$d[1], b$d[1], tolerance = 1.0e-8)
     expect_equal(a$v[1,2], b$v[1,2], tolerance = 1.0e-8)
     expect_equal(a$u[5,2], b$u[2,2], tolerance = 1.0e-8)
})

rm(list=ls())
