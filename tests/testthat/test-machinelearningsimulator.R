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
test_that("Ordered Logit", {
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
        type = "Ordered Logit",
        auxiliary.data = NULL,
        correction = "None",
        robust.se = FALSE,
        importance.absolute = NULL,
        interaction = NULL,
        outlier.prop.to.remove = 0,
        stacked.data.check = FALSE,
        unstacked.data = NULL
    )
    expect_equal(class(input.model), c("Regression", "OrderedLogitRegression"))
    expect_equal(unname(input.model$r.squared), .3600247482983)

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
        expect_equal(outcome, "0")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("-100", "0", "100"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(32.3396052526200, 52.1032768699700, 15.5571178774200))
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
        expect_equal(outcome, "100")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("-100", "0", "100"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(8.5373174159710, 42.9193007783300, 48.5433818057000))
    }
})
test_that("Binary Logit", {
    Q13_2_2 <- cell.phone.driver.analysis$Q13_2_2
    Q16_1 <- cell.phone.driver.analysis$Q16_1
    Q16_2 <- cell.phone.driver.analysis$Q16_2
    Q16_3 <- cell.phone.driver.analysis$Q16_3
    input.model <- MachineLearning(
        formula = Q13_2_2 ~ Q16_1 + Q16_2 + Q16_3,
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
        type = "Binary Logit",
        auxiliary.data = NULL,
        correction = "None",
        robust.se = FALSE,
        importance.absolute = NULL,
        interaction = NULL,
        outlier.prop.to.remove = 0,
        stacked.data.check = FALSE,
        unstacked.data = NULL
    )
    expect_equal(class(input.model), c("Regression", "BinaryLogitRegression"))
    expect_equal(unname(input.model$r.squared), .2557121193025)

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
        expect_equal(outcome, "Not Promoter")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("Not Promoter", "Promoter"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(69.4728031533800, 30.5271968466200))
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
        expect_equal(outcome, "Not Promoter")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("Not Promoter", "Promoter"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(51.5989954910700, 48.4010045089300))
    }
})
test_that("Multinomial Logit", {
    Q13_2_3 <- cell.phone.driver.analysis$Q13_2_3
    Q16_1 <- cell.phone.driver.analysis$Q16_1
    Q16_2 <- cell.phone.driver.analysis$Q16_2
    Q16_3 <- cell.phone.driver.analysis$Q16_3
    input.model <- MachineLearning(
        formula = Q13_2_3 ~ Q16_1 + Q16_2 + Q16_3,
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
        type = "Multinomial Logit",
        auxiliary.data = NULL,
        correction = "None",
        robust.se = FALSE,
        importance.absolute = NULL,
        interaction = NULL,
        outlier.prop.to.remove = NULL,
        stacked.data.check = FALSE,
        unstacked.data = NULL
    )
    expect_equal(class(input.model), c("Regression", "MultinomialLogitRegression"))
    expect_equal(unname(input.model$r.squared), .3671676022462)

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
        expect_equal(outcome, "Detractor")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("Detractor", "Passive/Neutral", "Promoter"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(45.2439578781600, 23.6893962716900, 31.0666458501500))
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
        expect_equal(outcome, "Promoter")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("Detractor", "Passive/Neutral", "Promoter"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(7.6778488023430, 41.4464962371100, 50.8756549605400))
    }
})
test_that("CART Categories", {
    Q13_2_3 <- cell.phone.driver.analysis$Q13_2_3
    Q16_1 <- cell.phone.driver.analysis$Q16_1
    Q16_2 <- cell.phone.driver.analysis$Q16_2
    Q16_3 <- cell.phone.driver.analysis$Q16_3
    input.model <- MachineLearning(
        formula = Q13_2_3 ~ Q16_1 + Q16_2 + Q16_3,
        algorithm = "CART",
        weights = NULL,
        subset = TRUE,
        missing = "Exclude cases with missing data",
        output = "Sankey",
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
        prune = "Minimum error",
        early.stopping = FALSE,
        predictor.level.treatment = "Abbreviated labels",
        outcome.level.treatment = "Full labels",
        long.running.calculations = FALSE,
        type = NULL,
        auxiliary.data = NULL,
        correction = NULL,
        robust.se = FALSE,
        importance.absolute = NULL,
        interaction = NULL,
        outlier.prop.to.remove = numeric(0),
        stacked.data.check = NULL,
        unstacked.data = NULL
    )
    expect_equal(class(input.model), c("CART", "MachineLearning", "rpart"))
    expect_equal(attr(input.model$confusion, "accuracy"), .6452282157676)

    # Test 1: Base case
    {
        combo.box.1 <- c("VeDi")
        combo.box.2 <- c("VeDi")
        combo.box.3 <- c("VeDi")
        all.combo.boxes <- c(combo.box.1, combo.box.2, combo.box.3)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q16_1" = factor(combo.box.1, levels = xlevels[["Q16_1"]], ordered = FALSE),
            "Q16_2" = factor(combo.box.2, levels = xlevels[["Q16_2"]], ordered = FALSE),
            "Q16_3" = factor(combo.box.3, levels = xlevels[["Q16_3"]], ordered = FALSE),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "Detractor")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("Detractor", "Passive/Neutral", "Promoter"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(58.2417582417600, 30.7692307692300, 10.9890109890100))
    }

    # Test 2: Non-base case
    {
        combo.box.1 <- c("VeDi")
        combo.box.2 <- c("VeDi")
        combo.box.3 <- c("VeSa")
        all.combo.boxes <- c(combo.box.1, combo.box.2, combo.box.3)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q16_1" = factor(combo.box.1, levels = xlevels[["Q16_1"]], ordered = FALSE),
            "Q16_2" = factor(combo.box.2, levels = xlevels[["Q16_2"]], ordered = FALSE),
            "Q16_3" = factor(combo.box.3, levels = xlevels[["Q16_3"]], ordered = FALSE),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "Promoter")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("Detractor", "Passive/Neutral", "Promoter"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(3.0303030303030, 24.2424242424200, 72.7272727272700))
    }
})
test_that("CART Numeric", {
    Q13_2 <- cell.phone.driver.analysis$Q13_2
    Q16_1 <- cell.phone.driver.analysis$Q16_1
    Q16_2 <- cell.phone.driver.analysis$Q16_2
    Q16_3 <- cell.phone.driver.analysis$Q16_3
    input.model <- MachineLearning(
        formula = Q13_2 ~ Q16_1 + Q16_2 + Q16_3,
        algorithm = "CART",
        weights = NULL,
        subset = TRUE,
        missing = "Exclude cases with missing data",
        output = "Sankey",
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
        prune = "Minimum error",
        early.stopping = FALSE,
        predictor.level.treatment = "Full labels",
        outcome.level.treatment = "Full labels",
        long.running.calculations = FALSE,
        type = NULL,
        auxiliary.data = NULL,
        correction = NULL,
        robust.se = FALSE,
        importance.absolute = NULL,
        interaction = NULL,
        outlier.prop.to.remove = numeric(0),
        stacked.data.check = NULL,
        unstacked.data = NULL
    )
    expect_equal(class(input.model), c("CART", "MachineLearning", "rpart"))
    expect_equal(attr(input.model$confusion, "accuracy"), .0269709543569)

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
        outcome <- predictOutcome(input.model, DF, is.numeric = TRUE)
        expect_equal(outcome, -32.7868852459000)
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
        outcome <- predictOutcome(input.model, DF, is.numeric = TRUE)
        expect_equal(outcome, -12.5)
    }
})
test_that("Deep Learning Categories", {
    Q13_2_3 <- cell.phone.driver.analysis$Q13_2_3
    Q16_1 <- cell.phone.driver.analysis$Q16_1
    Q16_2 <- cell.phone.driver.analysis$Q16_2
    Q16_3 <- cell.phone.driver.analysis$Q16_3
    input.model <- MachineLearning(
        formula = Q13_2_3 ~ Q16_1 + Q16_2 + Q16_3,
        algorithm = "Deep Learning",
        weights = NULL,
        subset = TRUE,
        missing = "Exclude cases with missing data",
        output = "Accuracy",
        show.labels = TRUE,
        seed = 12321,
        cost = NULL,
        booster = NULL,
        grid.search = NULL,
        sort.by.importance = NULL,
        hidden.nodes = "3,2",
        max.epochs = 10,
        normalize = TRUE,
        outcome.color = NULL,
        predictors.color = NULL,
        prior = NULL,
        prune = NULL,
        early.stopping = NULL,
        predictor.level.treatment = NULL,
        outcome.level.treatment = NULL,
        long.running.calculations = NULL,
        type = NULL,
        auxiliary.data = NULL,
        correction = NULL,
        robust.se = FALSE,
        importance.absolute = NULL,
        interaction = NULL,
        outlier.prop.to.remove = numeric(0),
        stacked.data.check = NULL,
        unstacked.data = NULL
    )
    expect_equal(class(input.model), c("MachineLearning", "DeepLearning", "list"))
    expect_equal(attr(input.model$confusion, "accuracy"), .5041493775934)

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
        expect_equal(outcome, "Passive/Neutral")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("Detractor", "Passive/Neutral", "Promoter"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(29.2548567056700, 46.7373937368400, 24.0077510476100))
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
        expect_equal(outcome, "Passive/Neutral")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("Detractor", "Passive/Neutral", "Promoter"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(29.9336612224600, 48.9298105239900, 21.1365327239000))
    }
})
test_that("Deep Learning Numeric", {
    Q13_2 <- cell.phone.driver.analysis$Q13_2
    Q16_1 <- cell.phone.driver.analysis$Q16_1
    Q16_2 <- cell.phone.driver.analysis$Q16_2
    Q16_3 <- cell.phone.driver.analysis$Q16_3
    input.model <- MachineLearning(
        formula = Q13_2 ~ Q16_1 + Q16_2 + Q16_3,
        algorithm = "Deep Learning",
        weights = NULL,
        subset = TRUE,
        missing = "Exclude cases with missing data",
        output = "Accuracy",
        show.labels = TRUE,
        seed = 12321,
        cost = NULL,
        booster = NULL,
        grid.search = NULL,
        sort.by.importance = NULL,
        hidden.nodes = "3,2",
        max.epochs = 10,
        normalize = TRUE,
        outcome.color = NULL,
        predictors.color = NULL,
        prior = NULL,
        prune = NULL,
        early.stopping = NULL,
        predictor.level.treatment = NULL,
        outcome.level.treatment = NULL,
        long.running.calculations = NULL,
        type = NULL,
        auxiliary.data = NULL,
        correction = NULL,
        robust.se = FALSE,
        importance.absolute = NULL,
        interaction = NULL,
        outlier.prop.to.remove = numeric(0),
        stacked.data.check = NULL,
        unstacked.data = NULL
    )
    expect_equal(class(input.model), c("MachineLearning", "DeepLearning", "list"))
    expect_equal(attr(input.model, "ChartData")[["Root Mean Squared Error"]], 81.1513222523800)

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
        outcome <- predictOutcome(input.model, DF, is.numeric = TRUE)
        expect_equal(outcome, 1.1728953123090)
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
        outcome <- predictOutcome(input.model, DF, is.numeric = TRUE)
        expect_equal(outcome, 1.3864848613740)
    }
})
test_that("SVM Categories", {
    Q13_2_3 <- cell.phone.driver.analysis$Q13_2_3
    Q16_1 <- cell.phone.driver.analysis$Q16_1
    Q16_2 <- cell.phone.driver.analysis$Q16_2
    Q16_3 <- cell.phone.driver.analysis$Q16_3
    input.model <- MachineLearning(
        formula = Q13_2_3 ~ Q16_1 + Q16_2 + Q16_3,
        algorithm = "Support Vector Machine",
        weights = NULL,
        subset = TRUE,
        missing = "Exclude cases with missing data",
        output = "Accuracy",
        show.labels = TRUE,
        seed = 12321,
        cost = "1",
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
        type = NULL,
        auxiliary.data = NULL,
        correction = NULL,
        robust.se = FALSE,
        importance.absolute = NULL,
        interaction = NULL,
        outlier.prop.to.remove = numeric(0),
        stacked.data.check = NULL,
        unstacked.data = NULL
    )
    expect_equal(class(input.model), c("MachineLearning", "SupportVectorMachine", "list"))
    expect_equal(attr(input.model$confusion, "accuracy"), .6431535269710)

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
        expect_equal(outcome, "Detractor")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("Promoter", "Detractor", "Passive/Neutral"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(14.1110897286000, 51.3360502440900, 34.5528600273100))
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
        expect_equal(outcome, "Promoter")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("Promoter", "Detractor", "Passive/Neutral"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(40.5388607042300, 24.6634939674600, 34.7976453283100))
    }
})
test_that("SVM Numeric", {
    Q13_2 <- cell.phone.driver.analysis$Q13_2
    Q16_1 <- cell.phone.driver.analysis$Q16_1
    Q16_2 <- cell.phone.driver.analysis$Q16_2
    Q16_3 <- cell.phone.driver.analysis$Q16_3
    input.model <- MachineLearning(
        formula = Q13_2 ~ Q16_1 + Q16_2 + Q16_3,
        algorithm = "Support Vector Machine",
        weights = NULL,
        subset = TRUE,
        missing = "Exclude cases with missing data",
        output = "Accuracy",
        show.labels = TRUE,
        seed = 12321,
        cost = "1",
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
        type = NULL,
        auxiliary.data = NULL,
        correction = NULL,
        robust.se = FALSE,
        importance.absolute = NULL,
        interaction = NULL,
        outlier.prop.to.remove = numeric(0),
        stacked.data.check = NULL,
        unstacked.data = NULL
    )
    expect_equal(class(input.model), c("MachineLearning", "SupportVectorMachine", "list"))
    expect_equal(attr(input.model, "ChartData")[["Root Mean Squared Error"]], 66.5077101772600)

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
        outcome <- predictOutcome(input.model, DF, is.numeric = TRUE)
        expect_equal(outcome, 10.4188913913500)
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
        outcome <- predictOutcome(input.model, DF, is.numeric = TRUE)
        expect_equal(outcome, 22.4643491266700)
    }
})
test_that("Gradient Boost Categories", {
    Q13_2_3 <- cell.phone.driver.analysis$Q13_2_3
    Q16_1 <- cell.phone.driver.analysis$Q16_1
    Q16_2 <- cell.phone.driver.analysis$Q16_2
    Q16_3 <- cell.phone.driver.analysis$Q16_3
    input.model <- MachineLearning(
        formula = Q13_2_3 ~ Q16_1 + Q16_2 + Q16_3,
        algorithm = "Gradient Boosting",
        weights = NULL,
        subset = TRUE,
        missing = "Exclude cases with missing data",
        output = "Accuracy",
        show.labels = TRUE,
        seed = 12321,
        cost = NULL,
        booster = "gbtree",
        grid.search = FALSE,
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
        type = NULL,
        auxiliary.data = NULL,
        correction = NULL,
        robust.se = FALSE,
        importance.absolute = NULL,
        interaction = NULL,
        outlier.prop.to.remove = numeric(0),
        stacked.data.check = NULL,
        unstacked.data = NULL
    )
    expect_equal(class(input.model), c("MachineLearning", "GradientBoost", "list"))
    expect_equal(attr(input.model$confusion, "accuracy"), .6887966804979)

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
        expect_equal(outcome, "Promoter")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("Detractor", "Passive/Neutral", "Promoter"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(37.1048539876900, 22.4186316132500, 40.4765099287000))
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
        expect_equal(outcome, "Passive/Neutral")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("Detractor", "Passive/Neutral", "Promoter"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(17.5841569900500, 51.3770222663900, 31.0388177633300))
    }
})
test_that("Gradient Boost Numeric", {
    Q13_2 <- cell.phone.driver.analysis$Q13_2
    Q16_1 <- cell.phone.driver.analysis$Q16_1
    Q16_2 <- cell.phone.driver.analysis$Q16_2
    Q16_3 <- cell.phone.driver.analysis$Q16_3
    input.model <- MachineLearning(
        formula = Q13_2 ~ Q16_1 + Q16_2 + Q16_3,
        algorithm = "Gradient Boosting",
        weights = NULL,
        subset = TRUE,
        missing = "Exclude cases with missing data",
        output = "Accuracy",
        show.labels = TRUE,
        seed = 12321,
        cost = NULL,
        booster = "gbtree",
        grid.search = FALSE,
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
        type = NULL,
        auxiliary.data = NULL,
        correction = NULL,
        robust.se = FALSE,
        importance.absolute = NULL,
        interaction = NULL,
        outlier.prop.to.remove = numeric(0),
        stacked.data.check = NULL,
        unstacked.data = NULL
    )
    expect_equal(class(input.model), c("MachineLearning", "GradientBoost", "list"))
    expect_equal(attr(input.model, "ChartData")[["Root Mean Squared Error"]], 50.8055426627900)

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
        outcome <- predictOutcome(input.model, DF, is.numeric = TRUE)
        expect_equal(outcome, -3.8710298538210)
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
        outcome <- predictOutcome(input.model, DF, is.numeric = TRUE)
        expect_equal(outcome, -81.2083892822300)
    }
})
test_that("LDA Categories", {
    Q13_2_3 <- cell.phone.driver.analysis$Q13_2_3
    Q16_1 <- cell.phone.driver.analysis$Q16_1
    Q16_2 <- cell.phone.driver.analysis$Q16_2
    Q16_3 <- cell.phone.driver.analysis$Q16_3
    input.model <- MachineLearning(
        formula = Q13_2_3 ~ Q16_1 + Q16_2 + Q16_3,
        algorithm = "Linear Discriminant Analysis",
        weights = NULL,
        subset = TRUE,
        missing = "Exclude cases with missing data",
        output = "Means",
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
        prior = "Observed",
        prune = NULL,
        early.stopping = NULL,
        predictor.level.treatment = NULL,
        outcome.level.treatment = NULL,
        long.running.calculations = NULL,
        type = NULL,
        auxiliary.data = NULL,
        correction = NULL,
        robust.se = FALSE,
        importance.absolute = NULL,
        interaction = NULL,
        outlier.prop.to.remove = numeric(0),
        stacked.data.check = NULL,
        unstacked.data = NULL
    )
    expect_equal(class(input.model), c("MachineLearning", "LDA", "list"))
    expect_equal(attr(input.model$confusion, "accuracy"), .6431535269710)

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
        expect_equal(outcome, "Detractor")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("Detractor", "Passive/Neutral", "Promoter"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(59.8302617032700, 23.7648499676300, 16.4048883291000))
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
        expect_equal(outcome, "Promoter")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("Detractor", "Passive/Neutral", "Promoter"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(5.4001325582240, 44.0048361643000, 50.5950312774800))
    }
})
