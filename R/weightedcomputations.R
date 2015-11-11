#' Weighted Singular Value Decomposition
#' \code{WeightedSVD} Computes a SVD with frequency weights.
#' @param x A numeric or complex matrix whose SVD decomposition is to be computed. Logical matrices are coerced to numeric.
#' @param weights Frequency Weights.
#' @param nu the number of left singular vectors to be computed. This must between 0 and n = nrow(x).
#' @export
WeightedSVD <- function(x, weights = rep(1, n), nu = min(n, p)) {
  n <-nrow(x)
  p <- ncol(x)
  if(!is.matrix(x))
    x <- as.matrix(x)
  x.eigen <- eigen(t(x) %*% sweep(x, 1, weights, "*"))
  d <- sqrt(x.eigen$values)
  v <- x.eigen$vectors
  if (nu == 0L)
    return(list(d = d, v = v))
  u <- t(solve(sweep(v, 2, d, "*"), t(x)))
  list(d = d, u = u[,1:nu], v = v)
}



# #' \code{WeightedCounts} Computes a SVD with frequency weights.
# #' @param x A numeric vector.
# #' @param weights Frequency Weights.
# #' @example
# #' @export
# WeightedCounts = function(x, weights){
#   tapply(weights, x, FUN = "sum")
# }
#
# # animals = factor(c("Cat","Cat","Dog","Penguin"))
# # wgt = c(1, 1, 2, .5)
# # WeightedCounts(animals, wgt)
#
# #' \code{WeightedMeans} Computes a SVD with frequency weights.
# WeightedMeans = function(x, weights){
#   tapply(weights, x, FUN = "sum") / sum(weights)
# }
#
#
