weightedSurveyDesign <- function(data, weights)
{
    survey::svydesign(id = ~ 1, weights = weights, data = data)
}

#' Adjust data to reflect weights
#'
#' Creates a new \code{\link{data.frame}} to reflect weights.

#' @param data A \code{\link{data.frame}}.
#' @param weights The sampling or replication weights.
#' @param seed The seed used in random number generation.
#' @details
#' In situations where an algorithm does not accomodate weights, this
#' function modifies the \code{\link{data.frame}} by either:
#' (A) stretching it out, where the the weights are integers, or
#' (B) resampling to create a new bootstrapped \code{\link{data.frame}},
#' where the \code{weights} are proportional to the probability of
#' selection.
#' When creating the bootstrap sample, the sample size is whichever is greatest
#' of the rounded sum and 1.
#'
#' @export
AdjustDataToReflectWeights <- function(data, weights, seed = 123)
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
        sum.weights <- max(round(sum(weights)), 1)
        replicants <- sample(1:n, size = sum.weights,
            replace = TRUE, prob = weights / sum.weights)
    }

    return(data[replicants, ])
}
