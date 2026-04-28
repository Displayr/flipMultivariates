context("Machine Learning Simulator")

# Created using the following R code from Displayr, then downloaded from the Cloud Drive
# cell.phone.driver.analysis <- list(
#     Q13_2 = Q13_2,
#     Q16_1 = Q16_1,
#     Q16_2 = Q16_2,
#     Q16_3 = Q16_3,
#     Q13_2_2 = Q13_2_2,
#     Q13_2_3 = Q13_2_3
# )
# flipAPI::QSaveData(cell.phone.driver.analysis, "cell.phone.driver.analysis.rds")
cell.phone.driver.analysis <- readRDS("cell.phone.driver.analysis.rds")

test_that("Linear Regression", {
    Q13_2 <- cell.phone.driver.analysis$Q13_2
    Q16_1 <- cell.phone.driver.analysis$Q16_1
    Q16_2 <- cell.phone.driver.analysis$Q16_2
    Q16_3 <- cell.phone.driver.analysis$Q16_3
    input.model <- MachineLearning(
        formula = Q13_2 ~ Q16_1 + Q16_2 + Q16_3,
        algorithm = "Regression",
        weights = NULL,
        subset = TRUE,
        missing = "Exclude cases with missing data",
        output = "Summary",
        show.labels = TRUE,
        seed = 12321,
        cost = NULL,
        booster = NULL,
        grid.search = NULL,
        sort.by.importance = NULL,
        hidden.nodes = NULL,
        max.epochs = NULL,
        normalize = NULL,
        outcome.color = NULL,
        predictors.color = NULL,
        prior = NULL,
        prune = NULL,
        early.stopping = NULL,
        predictor.level.treatment = NULL,
        outcome.level.treatment = NULL,
        long.running.calculations = NULL,
        type = "Linear",
        auxiliary.data = NULL,
        correction = "None",
        robust.se = FALSE,
        importance.absolute = NULL,
        interaction = NULL,
        outlier.prop.to.remove = 0,
        stacked.data.check = FALSE,
        unstacked.data = NULL
    )
    expect_equal(class(input.model), c("Regression", "LinearRegression"))
    expect_equal(unname(input.model$r.squared), .4241956627477)

    # Test 1: Base case
    {
        combo.box.1 <- c("Very dissatisfied")
        combo.box.2 <- c("Very dissatisfied")
        combo.box.3 <- c("Very dissatisfied")
        all.combo.boxes <- c(combo.box.1, combo.box.2, combo.box.3)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q16_1" = factor(combo.box.1, levels = xlevels[["Q16_1"]], ordered = FALSE),
            "Q16_2" = factor(combo.box.2, levels = xlevels[["Q16_2"]], ordered = FALSE),
            "Q16_3" = factor(combo.box.3, levels = xlevels[["Q16_3"]], ordered = FALSE),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, -16.3405532431000)
    }

    # Test 2: Non-base case
    {
        combo.box.1 <- c("Very dissatisfied")
        combo.box.2 <- c("Very dissatisfied")
        combo.box.3 <- c("Very satisfied")
        all.combo.boxes <- c(combo.box.1, combo.box.2, combo.box.3)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q16_1" = factor(combo.box.1, levels = xlevels[["Q16_1"]], ordered = FALSE),
            "Q16_2" = factor(combo.box.2, levels = xlevels[["Q16_2"]], ordered = FALSE),
            "Q16_3" = factor(combo.box.3, levels = xlevels[["Q16_3"]], ordered = FALSE),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, 33.2749874513800)
    }
})
