#' Packages up the object goodness of fit,
#' \code{GoodnessOfFitInternal} puts together the various bits of \code{GoodnessOfFit}.  These should usually not
#' be used directly unless by experienced users.
#' @param value The computed goodness-of-fit.
#' @param description Text elements used to construct the print statment.
#' @param call The original call used to create the object for which goodness-of-fit is being computed.
#' @param ... Additional arguments affecting the goodness-of-fit displayed.
#' @return A \code{\link{list}} with components
#' @return value The computed goodness-of-fit.
#' @return description Text elements used to construct the print statment.
#' @return call The original call used to create the object for which goodness-of-fit is being computed.
GoodnessOfFitInternal <- function(value, description, call) {
  result <- list(call = call, value = value, description = description)
  class(result) <- "flipGOF"
  result
}

#' Extracts fitted and observed.
#' \code{FittedAndObserved} extracts vectors of fitted (or predicted) and observed values from an object for use in computing
#' \code{GoodnessOfFit}.
#' @param object An object for which a summary is desired..
#' @return fitted Fitted values from the object.
#' @return observed Observed values from the object.
FittedAndObserved = function(object) {
  fitted = fitted(object)
  if(is.null(fitted))
    fitted = predict(object)
  observed = fitted + resid(object)
  list(fitted = fitted, observed = observed)
}


print.flipGOF = function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  cat(paste0(c(x$description,"\n", collapse = "")))
  invisible(x)
}

#' Object Goodness-of-Fit
#' \code{GoodnessOfFit} summary is a generic function used to produce result summaries of the
#' results of the model object. The function invokes particular \code{\link{methods}}
#' which depend on the \code{\link{class}} of the first argument.
#'
#' reports the goodness-of-fit of an object.
#' @param object An object for which a summary is desired..
#' @param digits Minimal number of significant digits, see \code{\link{print.default}}.
#' @param ... Additional arguments affecting the goodness-of-fit displayed.
#' #@examples
#'
#' # linear regression
#' x <- rnorm(10)
#' y <- rnorm(10) + x
#' mod <- lm(y ~ x)
#' GoodnessOfFit(mod)
#'
#' # MDS - square
#' library(smacof)
#' data(breakfastDissimilarities)
#' mdsInterval <- smacofSym(breakfastDissimilarities[[4]],
#'     type = "interval", eps = 1e-12, itmax = 100000)
#' GoodnessOfFit(mdsInterval)
#' # MDS - rectangular
#' data(breakfast)
#' GoodnessOfFit(smacofRect(breakfast))
#' @export
GoodnessOfFit <- function(object, digits = max(3L, getOption("digits") - 3L), ...) {
  UseMethod("GoodnessOfFit")
}

#' @describeIn GoodnessOfFit  Default goodness-of-fit \eqn{R^2}.
#' @export
GoodnessOfFit.default = function(object, digits = max(3L, getOption("digits") - 3L), ...) {
    obs.fit = FittedAndObserved(object)
    r2 = cor(obs.fit$fitted, obs.fit$observed, use = "complete.obs")^2
    names(r2) <- "R-squared"
    description <- list("Variance explained: ",
                      formatC(100 * r2, digits = digits),
                      "%\n(R-squared * 100)")
    GoodnessOfFitInternal(r2, description, object$call)
}


#' @describeIn GoodnessOfFit  Goodness-of-fit for a Regression object. Computed as the \eqn{R^2} statistic.
#' With factors, the index value of the factor is used. With unordered factors, this will often be
#' grieviously-downward biased.
#' @export
GoodnessOfFit.Regression = function(object, digits = max(3L, getOption("digits") - 3L), ...) {
    if (object$missing == "Use partial data (pairwise correlations)")
        r2 <- object$original$lm.cov$R2
    else if (object$type == "Linear" & is.null(object$weights))
        r2 <- object$summary$r.square
    else
    {
        predicted <- unclassIfNecessary(predict(object)[object$subset])
        observed <- unclassIfNecessary(Observed(object)[object$subset])
        if (is.null(object$weights))
            r2 <- cor(observed, predicted, use = "complete.obs") ^ 2
        else
            r2 <- summary(lm(predicted ~ observed, weights = object$weights[object$subset]))$r.square
    }
    names(r2) <- "R-squared"
    description <- list("Variance explained: ",
                        formatC(100 * r2, digits = digits),
                        "%\n(R-squared * 100)")
    GoodnessOfFitInternal(r2, description, object$call)
}



#' @describeIn GoodnessOfFit  Goodness-of-fit for a smacof object with square data (i.e., dissimilarities/distance)
#' @export
GoodnessOfFit.smacof = function(object, digits = max(3L, getOption("digits") - 3L), ...) {
  stress =  object$stress
  names(stress) = "STRESS1"
  description = list("Variance Explained: ",
                     formatC(100 *  (1 - stress), digits = digits),
                     "%\n(100 - STRESS1 * 100)")
  GoodnessOfFitInternal(stress, description, object$call)
}

#' @describeIn GoodnessOfFit  Goodness-of-fit for a smacofR object with rectangular data (e.g., preferences)
#' @export
GoodnessOfFit.smacofR = function(object, digits = max(3L, getOption("digits") - 3L), ...) {
  stress =  object$stress
  names(stress) = "STRESS2"
  description = list("Variance Explained: ",
                     formatC(100 - stress * 100, digits = digits),
                     "%\n(100 - STRESS2 * 100)")
  GoodnessOfFitInternal(stress, description, object$call)
}


nullDeviance <- function(x)
{
    null.d <- x$original$null.deviance
    if (!is.null(null.d))
        return(null.d)
    observed <- if (x$type %in% c("Ordered Logit", "Multinomial Logit") & !is.null(x$weights))
        as.vector(by(x$weights[x$subset], Observed(x)[x$subset], sum))
    else
        table(Observed(x)[x$subset])
    observed <- observed[observed > 0 & !is.na(observed)]
    ll <- sum(observed * log(prop.table(observed)))
    -2 * ll
}

#' \code{McFaddensRhoSquared}
#'
#' @param model A 'Regression' model.
#' @param n.permutations Number of permutations used in computing the p-value.
#' @details 1 - the deviance divided by the null deviance.
#'  McFadden, D. (1974) “Conditional logit analysis of qualitative choice behavior.” Pp. 105-142 in P. Zarembka (ed.), Frontiers in Econometrics
#' @export
McFaddensRhoSquared <- function(x)
{
    if (x$type == "Linear")
        stop("McFadden's rho-squared statistic is not computed for models of type 'Linear'.")
    1 - deviance(x$original) / nullDeviance(x)
}

#' @export
logLik.Regression <- function(x)
{
    logLik(x$original)
}

#' @export
AIC.Regression <- function(x)
{
    if (is.null(x$original$aic))
        return(AIC(x$original))
    x$original$aic
}

