weightedSurveyDesign <- function(data, weights)
{
    require(survey)
    survey::svydesign(id=~1, weights = weights, data = data)
}

#' \code{AdustDataToReflectWeights} Adjust data to reflect weights.
#' Creates a new \code{\link{data.frame}} to reflect weights.

#' @param data A \code{\link{data.frame}}.
#' @param weights The sampling or replication weights.
#' @param see The seed used in random number generation.
#' @details
#' In situations where an algorithm does not accomodate weights, this
#' function modifies the \code{\link{data.frame}} by either:
#' (A) stretching it out, where the the weights are integers, or
#' (B) resampling to create a new bootstrapped  \code{\link{data.frame}},
#' where the  \code{weights} are proportional to the probability of
#' selection.
#' When creating the bootstrap sample, the sample size is whichever is greatest
#' of the  rounded sum and 1.
#'
#' @export
AdustDataToReflectWeights <- function(data, weights, seed = 123)
{   # Inspired by Zelig, 13-11-15.
    set.seed(seed)
    n <- nrow(data)
    if (allIntegers(weights))
    {   # Reproducing cases according to the values of the weights.
        n <- nrow(data)
        replicants <- rep(1:n, weights)
    }
    else
    {   # Creating bootstrapped data file by resampling.
        warningWeightsBootstrapped()
        sum.weights   <- max(round(sum(weights)), 1)
        replicants <- sample(1:n, size = sum.weights,
            replace = TRUE, prob = weights / sum.weights)
    }
    return(data[replicants,])
}


# library(testthat)
# library(flipMultivariates)
#
#
#
# dat = data.frame(x = 1:2, y = 2:1)
# sm = sum(AdustDataToReflectWeights(dat, 2:1))
# sm
#
# test_that("Replicating data file with integer weights", {
#     d = AdustDataToReflectWeights(dat, 2:1)
#     expect_that(nrow(d), equals(3))
#     expect_that(sum(d), equals(9))
# })
#
# test_that("Creating bootrapped sample with weights", {
#     # Small sample.
#     d = suppressWarnings(AdustDataToReflectWeights(dat, (2:1) / 10))
#     expect_that(nrow(d), equals(1))
#     expect_that(sum(d), equals(3))
#     expect_warning(AdustDataToReflectWeights(dat, (2:1) / 10))
#
#     # Moderate sample.
#     d = suppressWarnings(AdustDataToReflectWeights(dat[rep(1:2,50),], runif(100)))
#     expect_that(nrow(d), equals(50))
#     expect_that(sum(d), equals(150))
#     expect_warning(AdustDataToReflectWeights(dat[rep(1:2,50),], runif(100)))
# })
#

