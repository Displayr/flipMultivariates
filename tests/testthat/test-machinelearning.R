context("Machine Learning")

data(adult.2000, package = "flipExampleData")
# create some missing data
adult.2000$relationship[1:200] <- NA
set.seed(1234)
adult.2000$wgt <- runif(2000) * 10
adult.2000$subset <- rep(c(TRUE, TRUE, TRUE, FALSE), 500)

algorithms <- c("Support Vector Machine", "Random Forest", "Deep Learning",
               "Gradient Boosting", "Linear Discriminant Analysis", "CART", "Regression")

for (alg in algorithms)
{
    test_that(paste0("Machine Learning: ", alg), {

        expect_error(ml <- suppressWarnings(MachineLearning(alg,
                              formula = sex ~ education_num + marital + workclass,
                              data = adult.2000)), NA)
        suppressWarnings(print(ml))
        first.pred <- predict(ml)[1]
        pred.from.chars <- predict(ml, data.frame(education_num = 9,
                                                  marital = " Married-civ-spouse",
                                                  workclass = " Private"))
        expect_equal(first.pred, pred.from.chars)
    })
}

adult.2000$race[runif(2000) > 0.9] <- NA
adult.2000$age[runif(2000) > 0.9] <- -Inf
adult.2000$hrs_per_week[runif(2000) > 0.9] <- Inf
algorithms <- c("Support Vector Machine", "Random Forest", "Deep Learning",
                "Gradient Boosting", "Linear Discriminant Analysis")

for (alg in algorithms)
{
    test_that(paste0("Machine Learning infinity: ", alg), {

        expect_error(ml <- suppressWarnings(MachineLearning(alg,
                                                            formula = sex ~ education_num + marital + age + hrs_per_week,
                                                            data = adult.2000)),
                     "Variable(s) age, hrs_per_week contain infinite values. Either recode the infinities to finite values or set them as missing data.",
                     fixed = TRUE)
    })
}

test_that("DS-2304: effects plot with colinear variable",
{
    load("DS2304data.rda")
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
