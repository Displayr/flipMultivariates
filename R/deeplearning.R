#' \code{DeepLearning}
#'
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
#' @param show.labels Shows the variable labels, as opposed to the labels, in the outputs, where a
#' variables label is an attribute (e.g., attr(foo, "label")).
#' @param hidden A vector of integers giving the number of units in each layer.
#' @param unit.function Type of function to use. By default it is \code{"rectifiedLinearUnit"}.
#' @param learning.rate,learning.decay,dropout.input,dropout.hidden,dither,bootstrap,pretrain.epoch Parameter passed to DArch.
#' @param epochs Duration of training period.
#' @param batch.size Size of training set
#' @param ... Other arguments to be supplied to \code{\link{darch}}.
#' @importFrom flipData GetData CleanSubset CleanWeights EstimationData DataFormula
#' @importFrom flipFormat Labels
#' @importFrom flipU OutcomeName
#' @importFrom flipTransformations AdjustDataToReflectWeights
#' @import darch
#' @export
DeepLearning <- function(formula,
                hidden = c(50, 50),
                unit.function = "rectifiedLinearUnit",
                learning.rate = 0.01,
                learning.decay = 0,
                dropout.input = 0,
                dropout.hidden = 0.5,
                dither = F,
                epochs = 100,
                batch.size = 1,
                bootstrap = T,
                pretrain.epoch = 0,
                data = NULL,
                subset = NULL,
                weights = NULL,
                output = "Summary",
                missing  = "Exclude cases with missing data",
                seed = 12321,
                show.labels = FALSE,
                ...)
{
    if (is.character(hidden))
    {
        h.str <- unlist(strsplit(split=",", hidden))
        hidden <- gsub(" ", "", h.str)
    }
    hidden <- suppressWarnings(as.numeric(hidden))
    if (any(is.na(hidden)))
        stop("'hidden' should be a comma-separated list specifying the number of units in each layer\n")
    if (any(hidden != round(hidden)))
        stop("'hidden' should be a list of integers\n")

    ####################################################################
    ##### Reading in the data and doing some basic tidying        ######
    ####################################################################
    cl <- match.call()
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
    processed.data <- EstimationData(input.formula, data, subset, weights, missing,seed = seed)
    unfiltered.weights <- processed.data$unfiltered.weights
    .estimation.data <- processed.data$estimation.data
    n.predictors <- ncol(.estimation.data)
    n <- nrow(.estimation.data)
    #if (n < ncol(.estimation.data) + 1)
    #    stop("The sample size is too small for it to be possible to conduct the analysis.")
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
    cnames <- if (show.labels) Labels(data)
              else colnames(data)
    vnames <- c()
    for (i in 2:ncol(data))
    {
        tmp <- c()
        if (is.ordered(data[,i]))
        {
            tmp <- paste0(":", levels(.estimation.data[,i])[-1])
        } else if (is.factor(data[,i]))
        {
            tmp <- paste0(":", levels(.estimation.data[,i]))
        }
        vnames <- c(vnames, paste0(cnames[i], tmp))
    }

    result <- list()
    nlev <- nlevels(.estimation.data.1[,1])
    if (is.null(nlev) || nlev == 0)
        nlev <- 1
    obj <- darch(formula, data=.estimation.data.1,
                     layers=c(length(vnames),hidden,nlev),
                     preProc.params = if(numeric.outcome) list(method=c("center","scale")) else NULL,
                     preProc.targets = numeric.outcome,
                     preProc.fullRank = F,
                     normalizeWeights=T,
                     bootstrap = bootstrap,
                     darch.isClass = !numeric.outcome,
                     darch.unitFunction = c(rep(unit.function, length(hidden)),
                                            ifelse(numeric.outcome, "linearUnit", "softmaxUnit")),
                     darch.dither = dither,
                     darch.dropout = c(dropout.input, rep(dropout.hidden, length(hidden))),
                     bp.learnRate = learning.rate,
                     bp.learnRateScale = 1 - learning.decay,
                     darch.numEpochs = epochs,
                     darch.batchSize = batch.size,
                     rbm.numEpochs = pretrain.epoch,
                     rbm.lastLayer = -1,
                     darch.returnBestModel = T,
                     logLevel = "WARN",
                     seed = seed,
                     ...)
    result <- list()
    result$original <- obj
    result$call <- cl


    ####################################################################
    ##### Saving results, parameters, and tidying up               #####
    ####################################################################
    result$subset <- subset <- row.names %in% rownames(.estimation.data)
    result$weights <- unfiltered.weights
    result$model <- data
    class(result) <- "DeepLearning"
    result$outcome.name <- outcome.name
    result$sample.description <- processed.data$description
    result$n.observations <- n
    result$estimation.data <- .estimation.data
    result$numeric.outcome <- numeric.outcome
    result$confusion <- ConfusionMatrix(result, subset, unfiltered.weights)
    result$variablenames <- vnames

    if (missing == "Imputation (replace missing values with estimates)")
        data <- processed.data$data
    result$model <- data
    result$formula <- input.formula
    result$output <- output
    result$missing <- missing
    # 5. Statistics
    #result$z.statistics <- result$original$importance[, 1:(ncol(result$original$importance) - 1)] / result$original$importanceSD
    #result$p.values <- 2 * (1 - pnorm(abs(result$z.statistics)))
    result
}

#' \code{VariableImportance}
#' @description Computes relative importance of input variable to the neural network
#' as the sum of the product of raw input-hidden, hidden-output connection weights
#' as proposed by Olden et al. 2004.
#' @param object Object of class \code{"DeepLearning"}
#' @importFrom NeuralNetTools olden
#' @export
VariableImportance <- function(object)
{
    if (class(object) != "DeepLearning")
        stop("Object should be of class \"DeepLearning\"\n")

    xx <- object$original
    mod_in <- c()
    struct <- c()
    for (i in 1:length(xx@layers))
    {
        mod_in <- c(mod_in, xx@layers[[i]][["weights"]])
        struct <- c(struct, nrow(xx@layers[[i]][["weights"]]) - 1)
        cat("i:", dim(xx@layers[[i]][["weights"]]), "\n")
    }
    struct <- c(struct, ncol(xx@layers[[length(xx@layers)]][["weights"]]))
    varImp <- suppressWarnings(olden(mod_in, struct=struct, bar_plot=FALSE))
    rownames(varImp) <- object$variablenames
    return(varImp)
}

#' \code{ConfusionMatrix.DeepLearning}
#' @param obj A model with an categorical outcome variable.
#' @param subset An optional vector specifying a subset of observations to be
#'   used in the fitting process, or, the name of a variable in \code{data}. It
#'   may not be an expression.
#' @param weights An optional vector of sampling weights, or, the name or, the
#'   name of a variable in \code{data}. It may not be an expression.
#' @details The proportion of observed values that take the same values as the predicted values.
#' Where the outcome
#' variable in the model is not a factor and not a count, predicted values are assigned to the closest observed
#' value.
#' @importFrom methods is
#' @importFrom flipRegression ConfusionMatrixFromVariables
#' @export
ConfusionMatrix.DeepLearning <- function(obj, subset = NULL, weights = NULL)
{
    observed <- obj$model[,1]
    predicted <- predict(obj)
    #if(obj$numeric.outcome)
    #    stop("ConfusionMatrix only defined for categorical outcome variables\n")
    return(ConfusionMatrixFromVariables(observed, predicted, subset, weights))
}

#' \code{print.DeepLearning}
#' @param x DeepLearning object to print.
#' @param ... Not used.
#' @importFrom flipFormat DeepLearningTable FormatWithDecimals ExtractCommonPrefix
#' @importFrom graphics plot
#' @export
print.DeepLearning <- function(x, ...)
{
    # Check about using label/names
    # Also, perhaps stats should be computed by predicting on the estimation data


    if (x$output == "Training error")
    {
        print(plot(x$original))
    } else if (x$output == "Summary")
    {
        title <- paste0("Deep Learning: ", x$outcome.name)  # doesn't handle show labels
        imp <- VariableImportance(x)
        extracted <- ExtractCommonPrefix(rownames(imp))
        if (!is.na(extracted$common.prefix))
        {
            title <- paste0(title, " by ", extracted$common.prefix)
            rownames(imp) <- extracted$shortened.labels
        }
        subtitle <- ""
        if (!x$numeric.outcome)
        {
            confM <- ConfusionMatrix(x)
            tot.cor <- sum(diag(confM))/sum(confM)
            class.cor <- unlist(lapply(1:nrow(confM), function(i) {confM[i,i]/sum(confM[i,])}))
            tmp.text <- paste(paste0(rownames(confM), ":"),
                              paste0(FormatWithDecimals(class.cor*100, 2), "%"),
                              collapse=", ")
            subtitle <- sprintf("Correct predictions: %.2f%% (%s)",
                                tot.cor*100, tmp.text)
        } else
        {
            pred <- predict(x)
            rmse <- sqrt(mean((pred - x$model[,1])^2))
            rsq <- (cor(pred, x$model[,1]))^2
            subtitle <- sprintf("R-sq: %.2f, RMSE: %.2f", rsq, rmse)
        }
        tbl <- DeepLearningTable(imp,
                                 order.values = TRUE,
                                 title = title,
                                 subtitle = subtitle,
                                 footer = x$sample.description)
        print(tbl)
    }
    else
    {
        print(x$call)
        cat(x$sample.description, "\n")

        if (!x$numeric.outcome)
        {
            cat("\nConfusion matrix:\n")
            print(ConfusionMatrix.DeepLearning(x))
            cat("\n")
        }

        vImp <- VariableImportance(x)
        cat("\nVariable importance (Olden)\n")
        print(vImp[order(vImp[,1], decreasing=T),,drop=FALSE])
    }
}


