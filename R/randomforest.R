#' \code{RandomForest}
#'
#' RandomForest
#' @param formula A formula of the form \code{groups ~ x1 + x2 + ...}
#' That is, the response is the grouping factor and the right hand side
#' specifies the (non-factor) discriminators, and any transformations, interactions,
#' or other non-additive operators will be ignored.
#' transformations nor
#' @param data A \code{\link{data.frame}} from which variables specified
#' in formula are preferentially to be taken.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression. \code{subset} may not
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @param output One of \code{"Importance"}, or \code{"Detail"}.
#' @param missing How missing data is to be treated in the regression. Options:
#'   \code{"Error if missing data"},
#'   \code{"Exclude cases with missing data"},
#' @param seed The random number seed used in imputation.
#' @param statistical.assumptions A Statistical Assumptions object.
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param sort.by.importance Sort the last column of the importance table
#' in descending order.
#' @param ... Other arguments to be supplied to \code{\link{randomForest}}.
#' @importFrom flipData GetData CleanSubset CleanWeights EstimationData DataFormula
#' @importFrom stats pnorm
#' @importFrom flipFormat Labels
#' @importFrom flipU OutcomeName
#' @importFrom randomForest randomForest
#' @importFrom flipTransformations AdjustDataToReflectWeights
#' @export
RandomForest <- function(formula,
                data = NULL,
                subset = NULL,
                weights = NULL,
                output = "Importance",
                missing  = "Exclude cases with missing data",
                seed = 12321,
                statistical.assumptions,
                show.labels = FALSE,
                sort.by.importance = TRUE,
                ...)
{
    ####################################################################
    ##### Reading in the data and doing some basic tidying        ######
    ####################################################################
    cl <- match.call()
    if(!missing(statistical.assumptions))
        stop("'statistical.assumptions' objects are not yet supported.")
    input.formula <- formula # To work past scoping issues in car package: https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf.
    subset.description <- try(deparse(substitute(subset)), silent = TRUE) #We don't know whether subset is a variable in the environment or in data.
    subset <- eval(substitute(subset), data, parent.frame())
    if (!is.null(subset))
    {
        if (is.null(subset.description) | (class(subset.description) == "try-error") | !is.null(attr(subset, "name")))
            subset.description <- Labels(subset)
        if (is.null(attr(subset, "name")))
            attr(subset, "name") <- subset.description
    }
    if(!is.null(weights))
        if (is.null(attr(weights, "name")))
            attr(weights, "name") <- deparse(substitute(weights))
    weights <- eval(substitute(weights), data, parent.frame())
    data <- GetData(input.formula, data, auxiliary.data = NULL)
    row.names <- rownames(data)
    outcome.name <- OutcomeName(input.formula)
    outcome.i <- match(outcome.name, names(data))
    outcome.variable <- data[, outcome.i]
    numeric.outcome <- !is.factor(outcome.variable)
    variable.labels <- Labels(data)
    outcome.label <- variable.labels[outcome.i]
    if (outcome.label == "data[, outcome.name]")
        outcome.label <- outcome.name
    if (!is.null(weights) & length(weights) != nrow(data))
        stop("'weights' and 'data' are required to have the same number of observations. They do not.")
    if (!is.null(subset) & length(subset) > 1 & length(subset) != nrow(data))
        stop("'subset' and 'data' are required to have the same number of observations. They do not.")
    # Treatment of missing values.
    processed.data <- EstimationData(input.formula, data, subset, weights, missing, seed = seed,
                                     error.if.insufficient.obs = FALSE)
    unfiltered.weights <- processed.data$unfiltered.weights
    .estimation.data <- processed.data$estimation.data
    n.predictors <- ncol(.estimation.data)
    n <- nrow(.estimation.data)
    if (n < ncol(.estimation.data) + 1)
        stop("The sample size is too small for it to be possible to conduct the analysis.")
    post.missing.data.estimation.sample <- processed.data$post.missing.data.estimation.sample
    .weights <- processed.data$weights
    .formula <- DataFormula(input.formula)
    # Resampling to generate a weighted sample, if necessary.
    .estimation.data.1 <- if (is.null(weights))
        .estimation.data
    else
        AdjustDataToReflectWeights(.estimation.data, .weights)
    ####################################################################
    ##### Fitting the model. Ideally, this should be a call to     #####
    ##### another function, with the output of that function       #####
    ##### called 'original'.                                       #####
    ####################################################################
    set.seed(seed)
    result <- list(original = randomForest(.formula,
        importance = TRUE, data = .estimation.data.1))
    result$original$call <- cl
    ####################################################################
    ##### Saving results, parameters, and tidying up               #####
    ####################################################################
    # 1. Saving data.
    result$subset <- subset <- row.names %in% rownames(.estimation.data)
    result$weights <- unfiltered.weights
    result$model <- data
    # 2. Saving descriptive information.
    class(result) <- "RandomForest"
    result$outcome.name <- outcome.name
    result$sample.description <- processed.data$description
    result$n.observations <- n
    result$estimation.data <- .estimation.data
    result$numeric.outcome <- numeric.outcome
    result$confusion <- ConfusionMatrix(result, subset, unfiltered.weights)
    # 3. Replacing names with labels
    if (result$show.labels <- show.labels)
    {
        result$outcome.label <- outcome.label
        # Removing the outcome variable
        result$variable.labels <- variable.labels <- variable.labels[-outcome.i]
        if (numeric.outcome)
            names(result$original$importanceSD) <- variable.labels
        else
            rownames(result$original$importanceSD) <- variable.labels
        # As predict.lda uses the variable labels as an input, the final swap to labels for the importance tables appears in print.RandomForest
    }
    else
        result$outcome.label <- outcome.name
    # 4.Saving parameters
    result$formula <- input.formula
    result$output <- output
    result$missing <- missing
    result$sort.by.importance <- sort.by.importance
    # 5. Statistics
    result$z.statistics <- result$original$importance[, 1:(ncol(result$original$importance) - 1)] / result$original$importanceSD
    result$p.values <- 2 * (1 - pnorm(abs(result$z.statistics)))
    result
}

#' @import randomForest
#' @importFrom flipFormat RandomForestTable FormatWithDecimals RandomForestTable
#' @export
print.RandomForest <- function(x, ...)
{
    if (x$show.labels)
        rownames(x$original$importance) <- x$variable.labels
    if (x$output == "Importance")
    {
        title <- paste0("Random Forest: ", x$outcome.label)
        subtitle <- if (x$numeric.outcome)
            paste("R-squared:", FormatWithDecimals(x$original$rsq[length(x$original$rsq)], 3))
        else
        {
            err <- x$original$err.rate
            accuracies <- 1 - err[nrow(err), ]
            k <- length(accuracies)
            correctPredictionsText(accuracies[1], colnames(err)[2:k], accuracies[2:k])
        }
        tbl <- RandomForestTable(x$original$importance,
                                 x$z.statistics,
                                 x$p.values,
                                 x$sort.by.importance,
                                 title = title,
                                 subtitle = subtitle,
                                 footer = x$sample.description)
        print(tbl)
    }
    else
    {
        print(x$original)
        invisible(x)
    }
}


#' \code{ConfusionMatrix}
#'
#' @param obj A model with an outcome variable.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression.
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @details The proportion of observed values that take the same values as the predicted values.
#' Where the outcome
#' variable in the model is not a factor and not a count, predicted values are assigned to the closest observed
#' value.
#' @importFrom stats predict
#' @importFrom methods is
#' @importFrom flipData Observed
#' @importFrom flipRegression ConfusionMatrixFromVariables ConfusionMatrixFromVariables
#' @export
ConfusionMatrix.RandomForest <- function(obj, subset = NULL, weights = NULL)
{
    observed <- Observed(obj)
    predicted <- predict(obj)
    if(obj$numeric.outcome)
        return(ConfusionMatrixFromVariables(observed, predicted, subset, weights))
    return(ConfusionMatrixFromVariables(observed, predicted, subset, weights))
}



#
#
# Accuracy <- function(observed, predicted) {
#   if(!is.factor(observed) | !is.factor(predicted))
#     stop("observed and predicted must be factors.")
#   n.levels <- nlevels(observed)
#   labs <- levels(observed)
#   counts <- tabulate(unclass(observed) + n.levels * (unclass(predicted) - 1))
#   counts <- c(counts, rep(0, n.levels ^ 2 - length(counts))) #filling in any empty cells
#   counts <- matrix(counts, n.levels, dimnames = list(Observed = labs, Predicted = labs))
#   n <- length(observed)
#   proportions <- prop.table(counts)
#   accuracy <- sum(diag(proportions))
#   accuracy.by.class <- diag(prop.table(counts,1))
#   names(accuracy.by.class) <- labs
#   worst.accuracy <- min(accuracy.by.class)
#   list(observed = prop.table(table(observed)), predicted = prop.table(table(predicted)),
#        counts = counts, proportions = proportions, accuracy.by.class = accuracy.by.class,
#        worst.accuracy = worst.accuracy,
#        accuracy = accuracy)
# }
#
#
# RemoveZeroVariance <- function(x) {
#   warning("Need to deal with factors")
#   warning("Test for positive number of variables")
#   sd <- apply(x, 2, sd)
#   to.remove <- sd < 0.000000001
#   if(sum(to.remove) == 0)
#     cat("No All variables have positive variances.\n")
#   else
#   cat("Removed", paste(names(x)[to.remove], collapse = ","),"\n")
#   x[,!to.remove]
# }
#
#
#
# # ExportExcelWorkbook <- function(object, file = "") {#PredictSegments object
# #   require(xlsx)
# #   write.xlsx(object$fit$frame, file, append = FALSE, sheetName = "frame")
# #   write.xlsx(data.frame(y = object$fit$y, Predictions = object$predictions, "Spreadsheet Predictions" = NA, object$fit$x), file, append = TRUE, sheetName = "data")
# # }
# #
# # ExportExcelWorkbook(seg1, 'C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\z.xlsx')
# #
# #   help(WriteXLS))
#
# library(foreign)
# spss1 <- read.spss('C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\jwtFirstSegmentation.sav')
# attach(spss1)
# d1 <- cbind(q25r1, q25r2, q25r3, q25r4, q25r5, q25r17, q25r6, q25r7, q25r8, q25r9, q25r10, q25r11,q25r12, q25r13, q25r14, q25r15, q25r16,
#             q31r1, q31r2, q31r3, q31r4, q31r5, q31r6, q31r7, q31r8, q31r9, q31r10, q38r1_3, q38r2_3, q38r3_3, q38r4_3, q38r5_3,
#             q38r6_3,q38r7_3, q38r8_3, q38r9_3, q38r10_3, q38r11_3, q38r12_3, q38r13_3, q41r1_3, q41r2_3, q41r3_3, q41r4_3)
# x1 <- as.data.frame(RemoveZeroVariance(d1) - 1)
# y1 <- Segments_Unweighted2
# detach(spss1)
#
# seg1 <- PredictSegments(x1, y1, max.n.variables = 10, min.size = 10,  method = "tree")
# seg1$accuracy
# seg1$names
#
# # ExportExcelWorkbook(seg1, 'C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\z.xlsx')
# # write.csv(seg1$fit$frame, "C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\seg1frame.csv")
#
# spss2 <- read.spss('C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\jwtSecondSegmentation.sav')
# attach(spss2)
# d2 <- cbind(q21r1_6,q21r2_6,q21r3_6,q21r4_6,q21r5_6,q21r6_6,q21r7_6,q21r8_6,q21r9_6,q21r10_6, q21r11_6,
#             #q21r12_6,  contains missing values
#             q21r13_6, q21r14_6, q21r15_6, q21r16_6, q21r17_6,  q21r18_6,q21r19_6, q21r20_6,  q21r21_6,q21r22_6,
#             q32r1_7,q32r2_7,q32r3_7,q32r4_7,q32r5_7,q32r6_7,q32r7_7,q32r8_7,q32r9_7,q32r10_7, q32r11_7,q32r12_7,
#             q32r13_7, q32r14_7, q32r15_7, q32r16_7, q32r17_7, q32r23_7,  q32r24_7,q32r18_7, q32r19_7,  q32r20_7,q32r21_7, q32r22_7)
# x2 <- as.data.frame(RemoveZeroVariance(d2))
# y2 <- Segments_V7
# x2 <- x2[!is.na(y2),]
# y2 <- y2[!is.na(y2)]
# detach(spss2)
#
# seg2 <- PredictSegments(x2, y2, max.n.variables = 9, min.size = 5,  method = "tree")
# # forcing to be consistent with email sent to Heather
# seg2 <- PredictSegments(x2[,c("q32r16_7", "q21r7_6", "q32r23_7", "q32r4_7", "q32r21_7", "q21r5_6", "q21r15_6", "q32r24_7", "q21r4_6")], y2, max.n.variables = 9, min.size = 5,  method = "tree")
# seg2$accuracy
# seg2$names
#
# #ExportExcelWorkbook(seg2, 'C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\z.xlsx')
# write.csv(seg2$fit$frame, "C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\seg2frame.csv")
# write.csv(data.frame(y = seg2$fit$y, Predictions = seg2$predictions, "Spreadsheet Predictions" = NA, Test = NaN, seg2$fit$x), "C:\\Users\\Tim\\Dropbox (Numbers)\\Q Sales and Marketing\\Clients\\JWT\\seg2Data.csv")
#
# write.xlsx(, file, append = TRUE, sheetName = "data")
# }
#
#
#
