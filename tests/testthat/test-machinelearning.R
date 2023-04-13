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
