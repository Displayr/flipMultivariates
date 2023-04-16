context("Machine Learning")

data(adult.2000, package = "flipExampleData")
# create some missing data
adult.2000$relationship[1:200] <- NA
set.seed(1234)
adult.2000$wgt <- runif(2000) * 10
adult.2000$subset <- rep(c(TRUE, TRUE, TRUE, FALSE), 500)

algorithms <- c("Support Vector Machine", "Random Forest", "Deep Learning",
                "Gradient Boosting", "Linear Discriminant Analysis", "CART", "Regression")

ml.args <- list(
    formula = sex ~ education_num + marital + workclass,
    data = adult.2000
)
newdata.test <- data.frame(
    education_num = 9,
    marital = " Married-civ-spouse",
    workclass = " Private"
)

for (alg in algorithms)
{
    test_that(paste0("Machine Learning: ", alg), {
        test.args <- c(ml.args, list(algorithm = alg))
        if (alg == "Deep Learning") # Avoid warning about convergence
            test.args[["max.epochs"]] <- 120L
        if (alg == "Regression") # Avoid warnings about binary class in Linear Reg
            test.args[["type"]] <- "Binary Logit"
        # Avoid warnings about outlier removal in printing
        expected.warn <- if (alg == "Regression") "Unusual observations detected" else NA
        expect_error(ml <- do.call(MachineLearning, test.args), NA)
        expect_warning(print(ml), expected.warn)
        first.pred <- predict(ml)[1]
        pred.from.chars <- predict(ml, newdata.test)
        expect_equal(first.pred, pred.from.chars)

        # NOTE: if the test below fails due to class names changing, ALL
        #       extension buttons in the wiki that refer to this class name should
        #       be updated with the new class name.
        expected.class <- if (alg == "Regression") "Regression" else "MachineLearning"
        expect_true(inherits(ml, expected.class))
    })
}

adult.2000$race[runif(2000) > 0.9] <- NA
adult.2000$age[runif(2000) > 0.9] <- -Inf
adult.2000$hrs_per_week[runif(2000) > 0.9] <- Inf
algorithms <- c("Support Vector Machine", "Random Forest", "Deep Learning",
                "Gradient Boosting", "Linear Discriminant Analysis")
ml.args[["formula"]] <- sex ~ education_num + marital + age + hrs_per_week
ml.args[["data"]] <- adult.2000
expected.error <- paste0("Variable(s) age, hrs_per_week contain infinite values. ",
                         "Either recode the infinities to finite values or set ",
                         "them as missing data.")

for (alg in algorithms)
{
    test_that(paste0("Machine Learning infinity: ", alg), {
        ml.args[["algorithm"]] <- alg
        expect_error(ml <- do.call(MachineLearning, ml.args),
                     expected.error, fixed = TRUE)
    })
}

test_that("DS-2304: effects plot with colinear variable",
{
    load("DS2304data.rda")
    dat <- data.frame(Overall, Fees, Interest, Phone, Branch, Online, ATM, Fees2)

    ## with data arg.
    model <- suppressWarnings(MachineLearning(
        formula = Overall ~ Fees + Interest + Phone + Branch + Online + ATM + Fees2,
        data = dat,
        algorithm = formAlgorithm,
        weights = QPopulationWeight, subset = QFilter,
        missing = formMissing, output = formOutput, show.labels = !formNames,
        seed = get0("formSeed"),
        cost = get0("formCost"),
        booster = get0("formBooster"),
        grid.search = get0("formSearch"),
        sort.by.importance = get0("formImportance"),
        hidden.nodes = get0("formHiddenLayers"),
        max.epochs = get0("formEpochs"),
        normalize = get0("formNormalize"),
        outcome.color = get0("formOutColor"),
        predictors.color = get0("formPredColor"),
        prior = get0("formPrior"),
        prune = get0("formPruning"),
        early.stopping = get0("formStopping"),
        predictor.level.treatment = get0("formPredictorCategoryLabels"),
        outcome.level.treatment = get0("formOutcomeCategoryLabels"),
        long.running.calculations = get0("formLongRunningCalculations"),
        type = get0("formRegressionType"),
        auxiliary.data = get0("formAuxiliaryVariables"),
        correction = get0("formCorrection"),
        robust.se = get0("formRobustSE", ifnotfound = FALSE),
        importance.absolute = get0("formAbsoluteImportance"),
        interaction = get0("formInteraction"),
        relative.importance = formOutput == "Relative Importance Analysis"))

    expect_error(suppressWarnings(print(model)), NA)

    ## without data arg
    model <- suppressWarnings(MachineLearning(
        formula = Overall ~ Fees + Interest + Phone + Branch + Online + ATM + Fees2,
        algorithm = formAlgorithm,
        weights = QPopulationWeight, subset = QFilter,
        missing = formMissing, output = formOutput, show.labels = !formNames,
        seed = get0("formSeed"),
        cost = get0("formCost"),
        booster = get0("formBooster"),
        grid.search = get0("formSearch"),
        sort.by.importance = get0("formImportance"),
        hidden.nodes = get0("formHiddenLayers"),
        max.epochs = get0("formEpochs"),
        normalize = get0("formNormalize"),
        outcome.color = get0("formOutColor"),
        predictors.color = get0("formPredColor"),
        prior = get0("formPrior"),
        prune = get0("formPruning"),
        early.stopping = get0("formStopping"),
        predictor.level.treatment = get0("formPredictorCategoryLabels"),
        outcome.level.treatment = get0("formOutcomeCategoryLabels"),
        long.running.calculations = get0("formLongRunningCalculations"),
        type = get0("formRegressionType"),
        auxiliary.data = get0("formAuxiliaryVariables"),
        correction = get0("formCorrection"),
        robust.se = get0("formRobustSE", ifnotfound = FALSE),
        importance.absolute = get0("formAbsoluteImportance"),
        interaction = get0("formInteraction"),
        relative.importance = formOutput == "Relative Importance Analysis"))

    expect_error(suppressWarnings(print(model)), NA)

})

test_that("DS-4360 Estimation Data template created correctly", {
    data(bank, package = "flipExampleData")
    bank.formula <- Overall ~ Fees + Interest
    all.variable.names <- flipU::AllVariablesNames(bank.formula, bank)
    sbank <- subset(bank, select = all.variable.names) |> na.omit()
    original.overall <- sbank[["Overall"]]
    # Recode to avoid warnings about small number of observations in category.
    original.overall[original.overall >= 6] <- 5
    original.overall[original.overall == 1] <- 4
    sbank[["Overall"]] <- original.overall
    sbank <- transform(sbank, Overall = factor(cut(Overall, breaks = c(-Inf, 3, Inf),
                                                   labels = c("<=3", ">=4"))))
    interest.levels <- c("Very dissatisfied", "unsatisfied", "a little unsatisfied",
                         "Neutral", "A little satisfied", "Satisfied", "Very satisfied")
    short.levels <- c("VeDi", "Uns", "ALiUn", "Neu", "ALiSa", "Sat", "VeSa")
    sbank <- transform(sbank, Interest = factor(Interest, labels = interest.levels))
    algorithm.types <- list("Regression", "CART", "Random Forest", #"Deep Learning",
                            "Support Vector Machine", "Gradient Boosting",
                            "Linear Discriminant Analysis")
    ml.args <- list(formula = bank.formula, data = sbank)
    # Helper function to test same data across all model types
    fitModelForTest <- function(algorithm, arguments = ml.args, use.binary.logit = TRUE) {
        arguments[["algorithm"]] <- algorithm
        is.regression <- algorithm == "Regression"
        if (is.regression) { # Avoid warning about two unique values to use Binary Logit instead of Linear
            arguments[["type"]] <- if (use.binary.logit) "Binary Logit" else "Linear"
        }
        expect_warning(model <- do.call(MachineLearning, arguments), NA)
        model
    }
    # Function to check each regression model
    checkEstimationDataTemplate <- function(ml.model, expected.template) {
        expect_true("estimation.data.template" %in% names(ml.model))
        expect_equal(ml.model[["estimation.data.template"]], expected.template)
    }
    # Basic tests for all algorithm types
    basic.expected.template <- structure(
        list(
            Overall = list(
                type = "factor",
                levels = c("<=3", ">=4"),
                observed.levels = c("<=3", ">=4"),
                has.unobserved.levels = FALSE,
                ordered = FALSE,
                default.value = "<=3"
            ),
            Fees = list(
                type = "numeric",
                default.value = 1
            ),
            Interest = list(
                type = "factor",
                levels = interest.levels,
                observed.levels = interest.levels,
                has.unobserved.levels = FALSE,
                ordered = FALSE,
                default.value = interest.levels[1]
            )
        ),
        outcome.name = "Overall"
    )
    for (algorithm in algorithm.types) {
        fit <- fitModelForTest(algorithm = algorithm)
        expected.template <- basic.expected.template
        if (algorithm == "CART") {
            expected.template[["Overall"]][["levels.shortened"]] <- FALSE
            expected.template[["Interest"]][["levels.shortened"]] <- TRUE
            expected.template[["Interest"]][["short.levels"]] <- short.levels
            expected.template[["Interest"]][["observed.short.levels"]] <- short.levels
        }
        checkEstimationDataTemplate(fit, expected.template)
    }
    # Check numeric outcome
    numeric.overall <- list(
        type = "numeric",
        default.value = 2
    )
    basic.expected.template[["Overall"]] <- numeric.overall
    numeric.out.args <- ml.args
    numeric.out.args[["data"]][["Overall"]] <- original.overall
    for (algorithm in algorithm.types) {
        fit <- fitModelForTest(algorithm = algorithm,
                               arguments = numeric.out.args,
                               use.binary.logit = FALSE)
        expected.template <- basic.expected.template
        if (algorithm == "CART") {
            expected.template[["Interest"]][["levels.shortened"]] <- TRUE
            expected.template[["Interest"]][["short.levels"]] <- short.levels
            expected.template[["Interest"]][["observed.short.levels"]] <- short.levels
        }
        checkEstimationDataTemplate(fit, expected.template)
    }

})
