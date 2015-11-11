#' LinearDiscriminantAnalysis
#' \code{LinearDiscriminantAnalysis} Fits linear discriminant analysis models.
#' @param x A matrix or data frame of explanatory variables.
#' @param y A variable containg the group memberships (i.e., to be predicted or explained by x).
#' @param prior The assumed probability of each value of y occurring in the population.  By default this is set to "observed" and
#' the value is computed based on the observed data.  If set to "constant" the prior will be set to be equal for each group (this is the
#' default in SPSS).  Alternatively, a vector of probabilities can be provided.
#' @param ... Additional arguments.
#' @details
#' This is a wrapper for MASS::lda and MASS::qda.
#'
#' #### Linear discriminant analysis #####
# http://www.ats.ucla.edu/stat/spss/output/SPSS_discrim.htm
#' @export
LinearDiscriminantAnalysis = function (x, grouping, prior = proportions, tol = 1e-04,
                                       method = c("moment", "mle"),
                                       weights = rep(1, n.cases),
                                       CV = FALSE, nu = 5, ...){
  # Generalization of MASS::lda
  if (is.null(dim(x)))
    stop("'x' is not a matrix")
  x <- as.matrix(x)
  if (any(!is.finite(x)))
    stop("infinite, NA or NaN values in 'x'")
  n.cases <- nrow(x)
  p <- ncol(x)
  if (n.cases != length(grouping))
    stop("nrow(x) and length(grouping) are different")
  g <- as.factor(grouping)
  lev <- lev1 <- levels(g)
  counts <- as.vector(WeightedCounts(g, weights))
  if (!missing(prior)) {
    if (any(prior < 0) || round(sum(prior), 5) != 1)
      stop("invalid 'prior'")
    if (length(prior) != nlevels(g))
      stop("'prior' is of incorrect length")
    prior <- prior[counts > 0L]
  }
  if (any(counts == 0L)) {
    empty <- lev[counts == 0L]
    warning(sprintf(ngettext(length(empty), "group %s is empty",
                             "groups %s are empty"), paste(empty, collapse = " ")),
            domain = NA)
    lev1 <- lev[counts > 0L]
    g <- factor(g, levels = lev1)
    counts <- as.vector(WeightedCounts(g, weights))
  }
  proportions <- prop.table(counts)
  ng <- length(proportions)
  names(prior) <- names(counts) <- lev1
  method <- match.arg(method)
  x.by.weights <- sweep(x, 1, weights, "*")
  group.sums <- tapply(x.by.weights, list(rep(g, p), col(x)), sum)
  group.means <- sweep(group.sums, 1, counts, "/")
  var.weighted = cov.wt(x - group.means[g, ], weights, method == "mle")$cov
  print(var.weighted)
  stop("dog")
  f1 <- sqrt(diag(var.weighted))
  if (any(f1 < tol)) {
    const <- format((1L:p)[f1 < tol])
    stop(sprintf(ngettext(length(const), "variable %s appears to be constant within groups",
                          "variables %s appear to be constant within groups"),
                 paste(const, collapse = " ")), domain = NA)
  }
  weights.sum = sum(weights)
  scaling <- diag(1/f1, , p)
  fac <- if (method == "moment")
    1 / (weights.sum - ng)
  else 1 / weights.sum

  if (method == "moment")
    var.weighted = var.weighted * weights.sum / (weights.sum - ng)
  X <- sqrt(fac) * (x - group.means[g, ]) %*% scaling
  return(cbind(X,g, weights))
  X.s <- WeightedSVD(X, weights, nu = 0L)
  print(X.s)
  rank <- sum(X.s$d > tol)
  if (rank == 0L)
    stop("rank = 0: variables are numerically constant")
  if (rank < p)
    warning("variables are collinear")
  scaling <- scaling %*% X.s$v[, 1L:rank] %*% diag(1/X.s$d[1L:rank],, rank)
  if (CV) {
    x <- x %*% scaling
    dm <- group.means %*% scaling
    K <- if (method == "moment")
      ng
    else 0L
    dist <- matrix(0, n, ng)
    for (i in 1L:ng) {
      dev <- x - matrix(dm[i, ], n, rank, byrow = TRUE)
      dist[, i] <- rowSums(dev^2)
    }
    ind <- cbind(1L:n, g)
    nc <- counts[g]
    cc <- nc/((nc - 1) * (n - K))
    dist2 <- dist
    for (i in 1L:ng) {
      dev <- x - matrix(dm[i, ], n, rank, byrow = TRUE)
      dev2 <- x - dm[g, ]
      tmp <- rowSums(dev * dev2)
      dist[, i] <- (n - 1L - K)/(n - K) * (dist2[, i] +
                                             cc * tmp^2/(1 - cc * dist2[ind]))
    }
    dist[ind] <- dist2[ind] * (n - 1L - K)/(n - K) * (nc/(nc -
                                                            1))^2/(1 - cc * dist2[ind])
    dist <- 0.5 * dist - matrix(log(prior), n, ng, byrow = TRUE)
    dist <- exp(-(dist - min(dist, na.rm = TRUE)))
    cl <- factor(lev1[max.col(dist)], levels = lev)
    posterior <- dist/drop(dist %*% rep(1, length(prior)))
    dimnames(posterior) <- list(rownames(x), lev1)
    return(list(class = cl, posterior = posterior))
  }
  xbar <- colSums(prior %*% group.means)
  fac <- if (method == "mle")
    1/ng
  else 1/(ng - 1)
  X <- sqrt((n * prior) * fac) * scale(group.means, center = xbar,
                                       scale = FALSE) %*% scaling
  X.s <- svd(X, nu = 0L)
  rank <- sum(X.s$d > tol * X.s$d[1L])
  if (rank == 0L)
    stop("group means are numerically identical")
  scaling <- scaling %*% X.s$v[, 1L:rank]
  if (is.null(dimnames(x)))
    dimnames(scaling) <- list(NULL, paste("LD", 1L:rank,
                                          sep = ""))
  else {
    dimnames(scaling) <- list(colnames(x), paste("LD", 1L:rank,
                                                 sep = ""))
    dimnames(group.means)[[2L]] <- colnames(x)
  }
  cl <- match.call()
  cl[[1L]] <- as.name("lda")
  structure(list(prior = prior, counts = counts, means = group.means,
                 scaling = scaling, lev = lev, svd = X.s$d[1L:rank], N = n,
                 call = cl), class = "lda")
}

# library(foreign)
# dat = read.spss("http://www.ats.ucla.edu/stat/spss/output/discrim.sav", to.data.frame = TRUE)
# dat = dat[sample(1:nrow(dat),91),]
# wgt = c(10,rep(1,90))
# dat1 = dat[c(rep(1,10),2:91),]
#
# MASS::lda(dat1[,1:3], dat1$job , prior = rep(1/3,3))
# z1 = LinearDiscriminantAnalysis(dat1[,1:3], dat1$job , prior = rep(1/3,3), method = "mle")
# z2 = LinearDiscriminantAnalysis(dat[,1:3], dat$job , prior = rep(1/3,3), weights = wgt, method = "mle")
#
#
# WeightedSVD(z1[,1:3], nu = 0)
# WeightedSVD(z2[,1:3],z2[,5], nu = 0)
# z1a = z2[c(rep(1,10),2:91),1:3]
# WeightedSVD(z1a, nu = 0)
#
# etAnywhere("print.lda")
# A single object matching ‘print.lda’ was found
# It was found in the following places
# registered S3 method for print from namespace MASS
# namespace:MASS
# with value
#
# function (x, ...)
# {
#   if (!is.null(cl <- x$call)) {
#     names(cl)[2L] <- ""
#     cat("Call:\n")
#     dput(cl, control = NULL)
#   }
#   cat("\nPrior probabilities of groups:\n")
#   print(x$prior, ...)
#   cat("\nGroup means:\n")
#   print(x$means, ...)
#   cat("\nCoefficients of linear discriminants:\n")
#   print(x$scaling, ...)
#   svd <- x$svd
#   names(svd) <- dimnames(x$scaling)[[2L]]
#   if (length(svd) > 1L) {
#     cat("\nProportion of trace:\n")
#     print(round(svd^2/sum(svd^2), 4L), ...)
#   }
#   invisible(x)
# }
# <bytecode: 0x000000001a54f8a8>
#   <environment: namespace:MASS>
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# library(foreign)
# dat = read.spss("http://www.ats.ucla.edu/stat/spss/output/discrim.sav", to.data.frame = TRUE)
# library(MASS)
# lda(job ~ outdoor + social + conservative, data = dat)
#
#
# LDA0 <- lda(job ~ outdoor + social + conservative, data = dat)
# LDA1 <- lda(job ~ outdoor + social + conservative, prior = rep(1/3,3),  data = dat)
#
# xtabs(~predict(LDA1)$class + dat$job)
# LDA1 <- lda(dat[,1:3], dat$job , prior = rep(1/3,3),  data = dat)
# LDA1
# LDA2 <- lda(dat[,1:2], dat$job , prior = rep(1/3,3),  data = dat)
# LDA2
#
#
#
# lda(dat[,1:2], dat$job , prior = rep(1/3,3),  data = dat, weight = rep(1,lengthdat$job)))
#
#
# summary(LDA1)
#
#
# require(caret)
# require(e1071)
# z = train(dat[,1:3], dat$job,  method = "stepLDA",  prior = rep(1/3,3), trControl = trainControl(method = "cv"))
#
# z$finalModel$fit
#
#
# # prir
#
# function <- DiscriminantAnalysis(x, y, prior = "observed") {
# # setting the prior
# k <- ncol(x)
# if (prior == "constant")
#   prior z- rep(1 / k, k)
# else if (prior == "observed")
#   prior <- NULL
#
#
#
# 1 / rep(ncol(x)
