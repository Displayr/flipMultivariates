#' Train a machine learning model.
#'
#' @param algorithm The name of algorithm to perform the training. One of
#' \code{"CART"}, \code{"Regression"}, \code{"Random Forest"}, \code{"Deep Learning"},
#' \code{"Support Vector Machine"}, \code{"Linear Discriminant Analysis"}, or
#' \code{"Gradient Boosting"}.
#' @param ... Arguments to the function \code{algorithm}; see See Also
#' @param warn.if.no.match Logical; If TRUE, a warning is shown if any arguments are not matched.
#' @importFrom methods formalArgs
#' @return A machine learning object.
#' @export
#' @seealso \code{\link{SupportVectorMachine}}, \code{\link[flipTrees]{CART}},
#' \code{\link{DeepLearning}}, \code{\link{GradientBoosting}},
#' \code{\link{LinearDiscriminantAnalysis}}, \code{\link{RandomForest}},
#' \code{\link[flipRegression]{Regression}}
MachineLearning <- function(algorithm, ..., warn.if.no.match = FALSE) {
    user.args <- list(...)

    # Identify function name
    machine.learning.function <- gsub(" ", "", algorithm)

    fun.and.pars <- getFunctionAndParameters(machine.learning.function)
    arguments <- substituteArgumentNames(fun.and.pars$parameters, user.args, warn.if.no.match)

    return(do.call(fun.and.pars$machine.learning.function, arguments))
}


#' getFunctionNameAndParameters
#'
#' Gets the function, loading parameters if necessary, and the parameters of the function.
#' @param function.name The name of the function used to perform the machine learning.
#' @return A list with the following elements:
#' \item{\code{machine.learning.function}}{The function}.
#' \item{\code{parameters}}{The parameters in \code{machine.learning.function}}.
#' @noRd
getFunctionAndParameters <- function(function.name) {
    if (!is.character(function.name)) {
        stop("'function.name' must be of type 'character'.")
    }

    machine.learning.function <- gsub('"', "", function.name, fixed = TRUE) # fixing mess created when 'type' is already a character

    # Getting the function
    if (machine.learning.function == "CART") {
        machine.learning.function <- flipTrees::CART
    } else if (machine.learning.function == "Regression") {
        machine.learning.function <- flipRegression::Regression
    } else {
        machine.learning.function <- get0(machine.learning.function, mode = "function")
    }

    if (!is.function(machine.learning.function)) {
        stop(paste0("Cannot find ", machine.learning.function, "."))
    }
    parameters <- formalArgs(machine.learning.function)

    list(machine.learning.function = machine.learning.function, parameters = parameters)
}

#' substituteArgumentNames
#'
#' @param parameter.names The names of parameters.
#' @param arguments The arguments to match to the parameters.
#' @param warn.if.no.match If TRUE, a warning is shown if any arguments are not matched.
#' @noRd
substituteArgumentNames <- function(parameter.names, arguments, warn.if.no.match = TRUE) {
    a.names <- names(arguments)
    p.names <- parameter.names
    a.unmatched <- !a.names %in% p.names
    p.unmatched <- !p.names %in% a.names
    if (any(a.unmatched)) # Some argument names do not match parameter names
        {
            # Perform matches and update a.names
            .replaceMatches <- function(aa, pp) {
                for (a in aa[a.unmatched]) {
                    for (p in pp[p.unmatched]) {
                        if (parametersEqual(p, a)) {
                            a.names[match(a, aa)] <- p.names[match(p, pp)]
                        }
                    }
                }
                a.names <<- a.names
            }
            ## Permuting order and ignoring case
            a.regularized <- tolower(a.names)
            p.regularized <- tolower(p.names)
            .replaceMatches(a.regularized, p.regularized)

            # Substituting synonyms
            a.unmatched <- !a.names %in% p.names
            p.unmatched <- !p.names %in% a.names
            if (any(a.unmatched)) {
                .replaceSynonyms <- function(names) {
                    for (i in seq_along(synonyms))
                    {
                        syns <- synonyms[[i]]
                        init.syn <- syns[1]
                        for (s in syns[-1]) {
                            names <- gsub(s, init.syn, names, fixed = TRUE)
                        }
                    }
                    names
                }
                a.regularized[a.unmatched] <- .replaceSynonyms(tolower(a.names[a.unmatched]))
                p.regularized[p.unmatched] <- .replaceSynonyms(tolower(p.names[p.unmatched]))
                .replaceMatches(a.regularized, p.regularized)
            }
        }
    a.unmatched <- !a.names %in% p.names
    if (any(a.unmatched) && warn.if.no.match) {
        warning("The following arguments have been ignored: ", paste(a.names[a.unmatched], collapse = ", "))
    }
    names(arguments) <- a.names
    arguments[!a.unmatched]
}


#' Make sure that the first synonym is the briefiest, and from there they are ordered from
#' most more verbose to any roots.
#' These are swapped out in order. E.g., after the first line, all "colours" will be changed to 'col'.
#' @noRd
synonyms <- list(
    c("col", "colours", "colour", "colors", "color"),
    c("size", "sizes"),
    c("label", "labels"),
    c("categories", "x", "categories", "x.axis", "xaxis"),
    c("values", "y", "values", "y.axis", "yaxis"),
    c("xtitle", "x.title", "xlab"),
    c("ytitle", "y.title", "ylab"),
    c("title", "main"),
    c("label.show", "data.label.show"),
    c("fontsize", "font.size")
)

#' parametersEqual
#'
#' Checks if parameters are equal
#' @param recipient The name of the parameter in the function.
#' @param donor The provided name of the parameter.
#' @noRd
parametersEqual <- function(recipient, donor) {
    # Exact match
    if (recipient == donor) {
        return(TRUE)
    }
    # Matching after re-ordering full stops
    recipient.split <- sort(strsplit(recipient, ".", fixed = TRUE)[[1]])
    donor.split <- sort(strsplit(donor, ".", fixed = TRUE)[[1]])
    if (length(recipient.split) == length(donor.split)) {
        return((all(recipient.split == donor.split)))
    }
    return(FALSE)
}

#' OrganizeCategoricalPredictors
#'
#' Organize information about categorical predictors in the model.
#' @param input.model The machine learning model.
#' @param all.combo.boxes The selection of predictor values.
#' @export
OrganizeCategoricalPredictors <- function(input.model, all.combo.boxes) {
    estimation.data.template <- input.model$estimation.data.template
    all.variable.names <- names(estimation.data.template)
    outcome.name <- attr(estimation.data.template, "outcome.name")
    all.predictor.names <- setdiff(all.variable.names, outcome.name)
    predictor.templates <- estimation.data.template[all.predictor.names]
    all.predictor.labels <- vapply(predictor.templates, "[[", character(1L), "label")
    xlevels <- lapply(predictor.templates, FUN = function(this.var) {
        if (!is.null(this.var[["observed.short.levels"]])) {
            return(this.var[["observed.short.levels"]])
        }
        if (inherits(input.model, "LDA")) {
            return(this.var[["levels"]])
        }
        this.var[["observed.levels"]]
    })
    names(xlevels) <- all.predictor.names
    xlevels <- Filter(length, xlevels)
    all.predictor.names <- names(xlevels)
    levels.not.in.data <- mapply(
        function(var.name, combo.name) !combo.name %in% xlevels[[var.name]],
        all.predictor.names, all.combo.boxes
    )
    if (any(levels.not.in.data)) {
        relevant.combos <- all.combo.boxes[levels.not.in.data]
        relevant.vars <- all.predictor.names[levels.not.in.data]
        matches <- mapply(function(var.name, combo.name) vapply(xlevels[[var.name]], function(x) sub(combo.name, "", x), character(1L)),
            relevant.vars, relevant.combos,
            SIMPLIFY = FALSE
        )
        white.space.diffs <- lapply(matches, function(x) grepl("^\\s+$", x))
        if (any(unlist(white.space.diffs))) {
            var.to.use <- which.max(vapply(white.space.diffs, any, logical(1L)))
            StopForUserError(
                "This feature is not compatible with category labels that have surrounding white space. ",
                "To use this feature the category ", dQuote(xlevels[[names(var.to.use)]][white.space.diffs[[var.to.use]]]),
                " needs to be renamed to ", dQuote(relevant.combos[var.to.use]), " in the predictor variable ",
                sQuote(all.predictor.labels[levels.not.in.data][var.to.use])
            )
        }
        first.bad.level <- which.max(levels.not.in.data)
        StopForUserError(
            "The selected category ", dQuote(all.combo.boxes[first.bad.level]), " in the predictor variable ",
            sQuote(all.predictor.labels[first.bad.level]), " is not observed in the dataset used to construct the model ",
            "and consequently the model cannot predict the outcome. Please select a different category for this ",
            "predictor variable."
        )
    }
    return(xlevels)
}

#' PredictOutcome
#'
#' Predict the outcome for a simulator for a machine learning model.
#' @param input.model The machine learning model.
#' @param DF The data frame containing the predictor values.
#' @param is.numeric A logical value indicating if the outcome is numeric.
#' @importFrom utils getS3method
#' @export
PredictOutcome <- function(input.model, DF, is.numeric) {
    vector.or.class <- if (is.numeric) "vector" else "class"
    is.cart <- inherits(input.model, "CART")
    is.lda <- inherits(input.model, "LDA")
    prediction.function <- predict
    if (is.lda) {
        DF <- flipTransformations::AsDataFrame(DF,
            use.names = TRUE,
            ignore.columns = "",
            categorical.as.binary = TRUE,
            remove.first = TRUE
        )
    }
    arguments <- list(input.model, newdata = DF)
    if (is.cart) {
        prediction.function <- getS3method("predict", "rpart")
        type <- vector.or.class
        arguments <- c(arguments, type = type)
    }
    if (is.lda) {
        prediction.function <- ldaExtractVariables
        arguments$type <- "class"
        arguments$prior <- input.model$prior
        arguments$na.action <- na.pass
    }
    as.vector(do.call(prediction.function, arguments))
}

#' PredictProbabilities
#'
#' Predict the probabilities for a simulator for a machine learning model.
#' @param input.model The machine learning model.
#' @param DF The data frame containing the predictor values.
#' @importFrom utils getS3method
#' @export
PredictProbabilities <- function(input.model, DF) {
    model.classes <- class(input.model)
    if ("SupportVectorMachine" %in% model.classes) {
        svm.probs <- predict(input.model$original, newdata = DF, probability = TRUE)
        new.probs <- attr(svm.probs, "probabilities")
    } else if ("RandomForest" %in% model.classes) {
        new.probs <- randomForestExtractVariables(input.model, "prob", newdata = DF)
    } else if ("DeepLearning" %in% model.classes) {
        if (reticulate::py_is_null_xptr(input.model$original)) {
            input.model$original <- keras::unserialize_model(input.model$original.serial)
        }
        X <- as.matrix(flipTransformations::AsNumeric(DF))
        constants <- input.model$training.stdevs == 0
        if (input.model$normalize) {
            X[, !constants] <- scale(X[, !constants, drop = FALSE],
                center = input.model$training.means[!constants],
                scale = input.model$training.stdevs[!constants]
            )
        }
        new.probs <- predict(input.model$original, X)
        if (length(input.model$outcome.levels) == 2) {
            new.probs <- cbind(1 - new.probs, new.probs)
        }
        colnames(new.probs) <- input.model$outcome.levels
    } else if ("CART" %in% model.classes) {
        new.probs <- tryCatch(getS3method("predict", "rpart")(input.model, newdata = DF, type = "prob"),
            error = function(e) {
                if (grepl("new level", e$message)) {
                    StopForUserError("Cannot match categories. Please set Inputs > Predictor category labels to 'Full labels' in the CART model.")
                } else {
                    e
                }
            }
        )
    } else if ("GradientBoost" %in% model.classes) {
        new.probs <- predict.GradientBoost(input.model, newdata = DF, keep.soft.probs = TRUE)
        if (length(new.probs) == 1L) {
            new.probs <- cbind(1 - new.probs, new.probs)
        }
        colnames(new.probs) <- input.model$outcome.levels
    } else if ("LDA" %in% model.classes) {
        DF <- flipTransformations::AsDataFrame(DF,
            use.names = TRUE,
            ignore.columns = "",
            categorical.as.binary = TRUE,
            remove.first = TRUE
        )
        new.probs <- ldaExtractVariables(input.model, "posterior", input.model$prior, newdata = DF, na.action = na.pass)
    } else if ("BinaryLogitRegression" %in% model.classes) {
        new.probs <- Probabilities(input.model, newdata = DF)
    } else if ("MultinomialLogitRegression" %in% model.classes) {
        new.probs <- getS3method("Probabilities", "Regression")(input.model, newdata = DF)
        if (nrow(new.probs) == 1L) {
            colnames(new.probs) <- input.model$original$lev
        } else {
            new.probs <- new.probs[, 2, drop = FALSE]
        }
    } else if ("OrderedLogitRegression" %in% model.classes) {
        new.probs <- getS3method("Probabilities", "Regression")(input.model, newdata = rbind(DF, DF)) # Doesn't like new data with a single row
        new.probs <- as.matrix(new.probs[1, , drop = FALSE])
    }

    if (ncol(new.probs) > 1) {
        new.probs <- t(new.probs)
    }
    colnames(new.probs) <- "Probability (%)"
    new.probs <- new.probs * 100
    return(new.probs)
}
