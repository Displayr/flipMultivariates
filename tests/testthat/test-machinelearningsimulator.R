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
# cola.sav <- list(
#     Q27 = Q27,
#     Q2 = Q2,
#     Q3 = Q3,
#     Q6_A = Q6_A,
#     Q6_B = Q6_B,
#     Q6_C = Q6_C,
#     Q6_D = Q6_D,
#     Q6_E = Q6_E,
#     Q6_F = Q6_F,
#     Q5_5_1 = Q5_5_1
# )
# flipAPI::QSaveData(cola.sav, "cola.sav.rds")
cola.sav <- readRDS("cola.sav.rds")

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
test_that("Deep Learning Multi", {
    Q27 <- cola.sav$Q27
    Q2 <- cola.sav$Q2
    Q3 <- cola.sav$Q3
    Q6_A <- cola.sav$Q6_A
    Q6_B <- cola.sav$Q6_B
    Q6_C <- cola.sav$Q6_C
    Q6_D <- cola.sav$Q6_D
    Q6_E <- cola.sav$Q6_E
    Q6_F <- cola.sav$Q6_F
    Q5_5_1 <- cola.sav$Q5_5_1
    input.model <- MachineLearning(
        formula = Q27 ~ Q2 + Q3 + Q6_A + Q6_B + Q6_C + Q6_D + Q6_E + Q6_F,
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
        hidden.nodes = "10, 10",
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
    expect_equal(attr(input.model$confusion, "accuracy"), .4929078014184)

    # Test 1: Base case
    {
        combo.box.1 <- c("Male")
        combo.box.2 <- c("18 to 24")
        text.box.1 <- 2
        text.box.2 <- 2
        text.box.3 <- 2
        text.box.4 <- 2
        text.box.5 <- 2
        text.box.6 <- 2
        all.combo.boxes <- c(combo.box.1, combo.box.2)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q2" = factor(combo.box.1, levels = xlevels[["Q2"]], ordered = FALSE),
            "Q3" = factor(combo.box.2, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_A" = as.numeric(text.box.1),
            "Q6_B" = as.numeric(text.box.2),
            "Q6_C" = as.numeric(text.box.3),
            "Q6_D" = as.numeric(text.box.4),
            "Q6_E" = as.numeric(text.box.5),
            "Q6_F" = as.numeric(text.box.6),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "I typically eat and drink whatever I feel like")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("I am on a diet, so I tend to watch what I eat and drink", "I tend watch what I eat and drink, but don’t consider myself", "I typically eat and drink whatever I feel like"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(24.4136109948200, 36.7897808551800, 38.7966066598900))
    }

    # Test 2: Non-base case
    {
        combo.box.1 <- c("Female")
        combo.box.2 <- c("18 to 24")
        text.box.1 <- 2
        text.box.2 <- 2
        text.box.3 <- 2
        text.box.4 <- 2
        text.box.5 <- 2
        text.box.6 <- 2
        all.combo.boxes <- c(combo.box.1, combo.box.2)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q2" = factor(combo.box.1, levels = xlevels[["Q2"]], ordered = FALSE),
            "Q3" = factor(combo.box.2, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_A" = as.numeric(text.box.1),
            "Q6_B" = as.numeric(text.box.2),
            "Q6_C" = as.numeric(text.box.3),
            "Q6_D" = as.numeric(text.box.4),
            "Q6_E" = as.numeric(text.box.5),
            "Q6_F" = as.numeric(text.box.6),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "I tend watch what I eat and drink, but don’t consider myself")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("I am on a diet, so I tend to watch what I eat and drink", "I tend watch what I eat and drink, but don’t consider myself", "I typically eat and drink whatever I feel like"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(34.5954507589300, 38.9608293771700, 26.4437168836600))
    }
})
test_that("Binary Logit Multi", {
    Q5_5_1.from.Cola.sav <- cola.sav$Q5_5_1
    Q2.from.Cola.sav <- cola.sav$Q2
    Q3.from.Cola.sav <- cola.sav$Q3
    input.model <- MachineLearning(
        formula = Q5_5_1.from.Cola.sav ~ Q2.from.Cola.sav + Q3.from.Cola.sav,
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
    expect_equal(input.model$original$aic, 159.4598176745000)

    # When creating the model in displayr, the factors are exported as "0" and "1"
    # However, when the data is exported to R and the model is created in R, the factors are "No" and "Yes"
    # I assume this is because I only exported Q5_5_1, not all of Q5, and the other questions have different factors, so Displayr normalises them
    # This doesn't affect the computation, just the labels
    # q5.factors <- c("0", "1")
    q5.factors <- c("No", "Yes")
    q5.first.factor <- q5.factors[[1]]
    # Test 1: Base case
    {
        combo.box.1 <- c("Male")
        combo.box.2 <- c("18 to 24")
        all.combo.boxes <- c(combo.box.1, combo.box.2)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q2.from.Cola.sav" = factor(combo.box.1, levels = xlevels[["Q2.from.Cola.sav"]], ordered = FALSE),
            "Q3.from.Cola.sav" = factor(combo.box.2, levels = xlevels[["Q3.from.Cola.sav"]], ordered = FALSE),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, q5.first.factor)
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(q5.factors, c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(98.2944227629900, 1.7055772370060))
    }

    # Test 2: Non-base case
    {
        combo.box.1 <- c("Female")
        combo.box.2 <- c("18 to 24")
        all.combo.boxes <- c(combo.box.1, combo.box.2)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q2.from.Cola.sav" = factor(combo.box.1, levels = xlevels[["Q2.from.Cola.sav"]], ordered = FALSE),
            "Q3.from.Cola.sav" = factor(combo.box.2, levels = xlevels[["Q3.from.Cola.sav"]], ordered = FALSE),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, q5.first.factor)
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(q5.factors, c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(92.6146681461000, 7.3853318539030))
    }
})
test_that("CART Multi", {
    Q27 <- cola.sav$Q27
    Q2 <- cola.sav$Q2
    Q3 <- cola.sav$Q3
    Q6_A <- cola.sav$Q6_A
    Q6_B <- cola.sav$Q6_B
    Q6_C <- cola.sav$Q6_C
    Q6_D <- cola.sav$Q6_D
    Q6_E <- cola.sav$Q6_E
    Q6_F <- cola.sav$Q6_F
    input.model <- MachineLearning(
        formula = Q27 ~ Q2 + Q3 + Q6_A + Q6_B + Q6_C + Q6_D + Q6_E + Q6_F,
        algorithm = "CART",
        weights = NULL,
        subset = TRUE,
        missing = "Exclude cases with missing data",
        output = "Text",
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
    expect_equal(attr(input.model$confusion, "accuracy"), .5957446808511)

    # Test 1: Base case
    {
        combo.box.1 <- c("Mal")
        combo.box.2 <- c("18To24")
        text.box.1 <- 2
        text.box.2 <- 2
        text.box.3 <- 2
        text.box.4 <- 2
        text.box.5 <- 2
        text.box.6 <- 2
        all.combo.boxes <- c(combo.box.1, combo.box.2)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q2" = factor(combo.box.1, levels = xlevels[["Q2"]], ordered = FALSE),
            "Q3" = factor(combo.box.2, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_A" = as.numeric(text.box.1),
            "Q6_B" = as.numeric(text.box.2),
            "Q6_C" = as.numeric(text.box.3),
            "Q6_D" = as.numeric(text.box.4),
            "Q6_E" = as.numeric(text.box.5),
            "Q6_F" = as.numeric(text.box.6),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "I typically eat and drink whatever I feel like")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("I am on a diet, so I tend to watch what I eat and drink", "I tend watch what I eat and drink, but don’t consider myself", "I typically eat and drink whatever I feel like"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(6.4516129032260, 30.6451612903200, 62.9032258064500))
    }

    # Test 2: Non-base case
    {
        combo.box.1 <- c("Mal")
        combo.box.2 <- c("18To24")
        text.box.1 <- 2
        text.box.2 <- 2
        text.box.3 <- 2
        text.box.4 <- 2
        text.box.5 <- 3 # This is the only change that I could find that produces a different result
        text.box.6 <- 2
        all.combo.boxes <- c(combo.box.1, combo.box.2)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q2" = factor(combo.box.1, levels = xlevels[["Q2"]], ordered = FALSE),
            "Q3" = factor(combo.box.2, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_A" = as.numeric(text.box.1),
            "Q6_B" = as.numeric(text.box.2),
            "Q6_C" = as.numeric(text.box.3),
            "Q6_D" = as.numeric(text.box.4),
            "Q6_E" = as.numeric(text.box.5),
            "Q6_F" = as.numeric(text.box.6),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "I tend watch what I eat and drink, but don’t consider myself")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("I am on a diet, so I tend to watch what I eat and drink", "I tend watch what I eat and drink, but don’t consider myself", "I typically eat and drink whatever I feel like"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(9.5454545454550, 58.6363636363600, 31.8181818181800))
    }
})
test_that("Gradient Boost Multi", {
    Q27 <- cola.sav$Q27
    Q2 <- cola.sav$Q2
    Q3 <- cola.sav$Q3
    Q6_A <- cola.sav$Q6_A
    Q6_B <- cola.sav$Q6_B
    Q6_C <- cola.sav$Q6_C
    Q6_D <- cola.sav$Q6_D
    Q6_E <- cola.sav$Q6_E
    Q6_F <- cola.sav$Q6_F
    input.model <- MachineLearning(
        formula = Q27 ~ Q2 + Q3 + Q6_A + Q6_B + Q6_C + Q6_D + Q6_E + Q6_F,
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
    expect_equal(attr(input.model$confusion, "accuracy"), .8368794326241)

    # Test 1: Base case
    {
        combo.box.1 <- c("Male")
        combo.box.2 <- c("18 to 24")
        text.box.1 <- 2
        text.box.2 <- 2
        text.box.3 <- 2
        text.box.4 <- 2
        text.box.5 <- 2
        text.box.6 <- 2
        all.combo.boxes <- c(combo.box.1, combo.box.2)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q2" = factor(combo.box.1, levels = xlevels[["Q2"]], ordered = FALSE),
            "Q3" = factor(combo.box.2, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_A" = as.numeric(text.box.1),
            "Q6_B" = as.numeric(text.box.2),
            "Q6_C" = as.numeric(text.box.3),
            "Q6_D" = as.numeric(text.box.4),
            "Q6_E" = as.numeric(text.box.5),
            "Q6_F" = as.numeric(text.box.6),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "I typically eat and drink whatever I feel like")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("I am on a diet, so I tend to watch what I eat and drink", "I tend watch what I eat and drink, but don’t consider myself", "I typically eat and drink whatever I feel like"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(21.3520139455800, 37.6844733953500, 40.9635156393100))
    }

    # Test 2: Non-base case
    {
        combo.box.1 <- c("Female")
        combo.box.2 <- c("18 to 24")
        text.box.1 <- 2
        text.box.2 <- 2
        text.box.3 <- 2
        text.box.4 <- 2
        text.box.5 <- 2
        text.box.6 <- 2
        all.combo.boxes <- c(combo.box.1, combo.box.2)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q2" = factor(combo.box.1, levels = xlevels[["Q2"]], ordered = FALSE),
            "Q3" = factor(combo.box.2, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_A" = as.numeric(text.box.1),
            "Q6_B" = as.numeric(text.box.2),
            "Q6_C" = as.numeric(text.box.3),
            "Q6_D" = as.numeric(text.box.4),
            "Q6_E" = as.numeric(text.box.5),
            "Q6_F" = as.numeric(text.box.6),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "I typically eat and drink whatever I feel like")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("I am on a diet, so I tend to watch what I eat and drink", "I tend watch what I eat and drink, but don’t consider myself", "I typically eat and drink whatever I feel like"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(23.4692707657800, 31.5052866935700, 45.0254410505300))
    }
})
test_that("LDA Multi", {
    Q27 <- cola.sav$Q27
    Q2 <- cola.sav$Q2
    Q3 <- cola.sav$Q3
    Q6_A <- cola.sav$Q6_A
    Q6_B <- cola.sav$Q6_B
    Q6_C <- cola.sav$Q6_C
    Q6_D <- cola.sav$Q6_D
    Q6_E <- cola.sav$Q6_E
    Q6_F <- cola.sav$Q6_F
    input.model <- MachineLearning(
        formula = Q27 ~ Q2 + Q3 + Q6_A + Q6_B + Q6_C + Q6_D + Q6_E + Q6_F,
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
    expect_equal(attr(input.model$confusion, "accuracy"), .5921985815603)

    # Test 1: Base case
    {
        combo.box.1 <- c("Male")
        combo.box.2 <- c("18 to 24")
        text.box.1 <- 2
        text.box.2 <- 2
        text.box.3 <- 2
        text.box.4 <- 2
        text.box.5 <- 2
        text.box.6 <- 2
        all.combo.boxes <- c(combo.box.1, combo.box.2)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q2" = factor(combo.box.1, levels = xlevels[["Q2"]], ordered = FALSE),
            "Q3" = factor(combo.box.2, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_A" = as.numeric(text.box.1),
            "Q6_B" = as.numeric(text.box.2),
            "Q6_C" = as.numeric(text.box.3),
            "Q6_D" = as.numeric(text.box.4),
            "Q6_E" = as.numeric(text.box.5),
            "Q6_F" = as.numeric(text.box.6),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "I tend watch what I eat and drink, but don’t consider myself")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("I am on a diet, so I tend to watch what I eat and drink", "I tend watch what I eat and drink, but don’t consider myself", "I typically eat and drink whatever I feel like"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(1.4156325527480, 53.4222514983900, 45.1621159488600))
    }

    # Test 2: Non-base case
    {
        combo.box.1 <- c("Female")
        combo.box.2 <- c("18 to 24")
        text.box.1 <- 2
        text.box.2 <- 2
        text.box.3 <- 2
        text.box.4 <- 2
        text.box.5 <- 2
        text.box.6 <- 2
        all.combo.boxes <- c(combo.box.1, combo.box.2)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q2" = factor(combo.box.1, levels = xlevels[["Q2"]], ordered = FALSE),
            "Q3" = factor(combo.box.2, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_A" = as.numeric(text.box.1),
            "Q6_B" = as.numeric(text.box.2),
            "Q6_C" = as.numeric(text.box.3),
            "Q6_D" = as.numeric(text.box.4),
            "Q6_E" = as.numeric(text.box.5),
            "Q6_F" = as.numeric(text.box.6),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "I tend watch what I eat and drink, but don’t consider myself")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("I am on a diet, so I tend to watch what I eat and drink", "I tend watch what I eat and drink, but don’t consider myself", "I typically eat and drink whatever I feel like"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(1.6308676460910, 50.8039694425900, 47.5651629113200))
    }
})
test_that("Linear Regression Multi", {
    Q6_A <- cola.sav$Q6_A
    Q3 <- cola.sav$Q3
    Q6_D <- cola.sav$Q6_D
    input.model <- MachineLearning(
        formula = Q6_A ~ Q3 + Q6_D,
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
    expect_equal(unname(input.model$r.squared), .1000098675218)

    # Test 1: Base case
    {
        combo.box.1 <- c("18 to 24")
        text.box.1 <- c(2)
        all.combo.boxes <- c(combo.box.1)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q3" = factor(combo.box.1, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_D" = as.numeric(text.box.1),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, 4.7369237093650)
    }

    # Test 2: Non-base case
    {
        combo.box.1 <- c("18 to 24")
        text.box.1 <- c(1)
        all.combo.boxes <- c(combo.box.1)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q3" = factor(combo.box.1, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_D" = as.numeric(text.box.1),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, 4.4983280123950)
    }
})
test_that("Multinomial Logit Multi", {
    Q6_A <- cola.sav$Q6_A
    Q3 <- cola.sav$Q3
    Q6_D <- cola.sav$Q6_D
    input.model <- MachineLearning(
        formula = Q6_A ~ Q3 + Q6_D,
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
    expect_equal(unname(input.model$r.squared), .1164935590785)

    # Test 1: Base case
    {
        combo.box.1 <- c("18 to 24")
        text.box.1 <- c(2)
        all.combo.boxes <- c(combo.box.1)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q3" = factor(combo.box.1, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_D" = as.numeric(text.box.1),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "6")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("2", "3", "4", "5", "6"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(.0000354214575, 18.6135511652300, 14.2351821751200, 16.4345753140100, 50.7166559241900))
    }

    # Test 2: Non-base case
    {
        combo.box.1 <- c("65 or more")
        text.box.1 <- c(2)
        all.combo.boxes <- c(combo.box.1)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q3" = factor(combo.box.1, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_D" = as.numeric(text.box.1),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "2")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("2", "3", "4", "5", "6"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(43.6754070967400, 32.9271783689800, .0000000000000, 14.5672302485900, 8.8301842856970))
    }
})
test_that("Ordered Logit Multi", {
    Q6_A <- cola.sav$Q6_A
    Q3 <- cola.sav$Q3
    Q6_D <- cola.sav$Q6_D
    input.model <- MachineLearning(
        formula = Q6_A ~ Q3 + Q6_D,
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
    expect_equal(unname(input.model$r.squared), .0281740223357)

    # Test 1: Base case
    {
        combo.box.1 <- c("18 to 24")
        text.box.1 <- c(2)
        all.combo.boxes <- c(combo.box.1)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q3" = factor(combo.box.1, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_D" = as.numeric(text.box.1),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "5")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("2", "3", "4", "5", "6"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(4.1870727024600, 6.7535080370060, 15.4590049661700, 43.8472250435000, 29.7531892508700))
    }

    # Test 2: Non-base case
    {
        combo.box.1 <- c("65 or more")
        text.box.1 <- c(2)
        all.combo.boxes <- c(combo.box.1)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q3" = factor(combo.box.1, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_D" = as.numeric(text.box.1),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "5")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("2", "3", "4", "5", "6"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(10.3163249828900, 14.1184235637500, 24.1289532458000, 37.5756569936400, 13.8606412139200))
    }
})
test_that("Random Forest Multi", {
    Q27 <- cola.sav$Q27
    Q2 <- cola.sav$Q2
    Q3 <- cola.sav$Q3
    Q6_A <- cola.sav$Q6_A
    Q6_B <- cola.sav$Q6_B
    Q6_C <- cola.sav$Q6_C
    Q6_D <- cola.sav$Q6_D
    Q6_E <- cola.sav$Q6_E
    Q6_F <- cola.sav$Q6_F
    input.model <- MachineLearning(
        formula = Q27 ~ Q2 + Q3 + Q6_A + Q6_B + Q6_C + Q6_D + Q6_E + Q6_F,
        algorithm = "Random Forest",
        weights = NULL,
        subset = TRUE,
        missing = "Exclude cases with missing data",
        output = "Importance",
        show.labels = TRUE,
        seed = 12321,
        cost = NULL,
        booster = NULL,
        grid.search = NULL,
        sort.by.importance = TRUE,
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
    expect_equal(class(input.model), c("MachineLearning", "RandomForest", "list"))
    expect_equal(attr(input.model$confusion, "accuracy"), .9929078014184)

    # Test 1: Base case
    {
        combo.box.1 <- c("Male")
        combo.box.2 <- c("18 to 24")
        text.box.1 <- 2
        text.box.2 <- 2
        text.box.3 <- 2
        text.box.4 <- 2
        text.box.5 <- 2
        text.box.6 <- 2
        all.combo.boxes <- c(combo.box.1, combo.box.2)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q2" = factor(combo.box.1, levels = xlevels[["Q2"]], ordered = FALSE),
            "Q3" = factor(combo.box.2, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_A" = as.numeric(text.box.1),
            "Q6_B" = as.numeric(text.box.2),
            "Q6_C" = as.numeric(text.box.3),
            "Q6_D" = as.numeric(text.box.4),
            "Q6_E" = as.numeric(text.box.5),
            "Q6_F" = as.numeric(text.box.6),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "I typically eat and drink whatever I feel like")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("I am on a diet, so I tend to watch what I eat and drink", "I tend watch what I eat and drink, but don’t consider myself", "I typically eat and drink whatever I feel like"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(14.0000000000000, 20.0000000000000, 66.0000000000000))
    }

    # Test 2: Non-base case
    {
        combo.box.1 <- c("Female")
        combo.box.2 <- c("18 to 24")
        text.box.1 <- 2
        text.box.2 <- 2
        text.box.3 <- 2
        text.box.4 <- 2
        text.box.5 <- 2
        text.box.6 <- 2
        all.combo.boxes <- c(combo.box.1, combo.box.2)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q2" = factor(combo.box.1, levels = xlevels[["Q2"]], ordered = FALSE),
            "Q3" = factor(combo.box.2, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_A" = as.numeric(text.box.1),
            "Q6_B" = as.numeric(text.box.2),
            "Q6_C" = as.numeric(text.box.3),
            "Q6_D" = as.numeric(text.box.4),
            "Q6_E" = as.numeric(text.box.5),
            "Q6_F" = as.numeric(text.box.6),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "I typically eat and drink whatever I feel like")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("I am on a diet, so I tend to watch what I eat and drink", "I tend watch what I eat and drink, but don’t consider myself", "I typically eat and drink whatever I feel like"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(18.8000000000000, 18.8000000000000, 62.4000000000000))
    }
})
test_that("SVM Multi", {
    Q27 <- cola.sav$Q27
    Q2 <- cola.sav$Q2
    Q3 <- cola.sav$Q3
    Q6_A <- cola.sav$Q6_A
    Q6_B <- cola.sav$Q6_B
    Q6_C <- cola.sav$Q6_C
    Q6_D <- cola.sav$Q6_D
    Q6_E <- cola.sav$Q6_E
    Q6_F <- cola.sav$Q6_F
    input.model <- MachineLearning(
        formula = Q27 ~ Q2 + Q3 + Q6_A + Q6_B + Q6_C + Q6_D + Q6_E + Q6_F,
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
    expect_equal(attr(input.model$confusion, "accuracy"), .6134751773050)

    # Test 1: Base case
    {
        combo.box.1 <- c("Male")
        combo.box.2 <- c("18 to 24")
        text.box.1 <- 2
        text.box.2 <- 2
        text.box.3 <- 2
        text.box.4 <- 2
        text.box.5 <- 2
        text.box.6 <- 2
        all.combo.boxes <- c(combo.box.1, combo.box.2)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q2" = factor(combo.box.1, levels = xlevels[["Q2"]], ordered = FALSE),
            "Q3" = factor(combo.box.2, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_A" = as.numeric(text.box.1),
            "Q6_B" = as.numeric(text.box.2),
            "Q6_C" = as.numeric(text.box.3),
            "Q6_D" = as.numeric(text.box.4),
            "Q6_E" = as.numeric(text.box.5),
            "Q6_F" = as.numeric(text.box.6),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "I tend watch what I eat and drink, but don’t consider myself")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("I tend watch what I eat and drink, but don’t consider myself", "I typically eat and drink whatever I feel like", "I am on a diet, so I tend to watch what I eat and drink"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(47.5147625695800, 41.6768051872900, 10.8084322431300))
    }

    # Test 2: Non-base case
    {
        combo.box.1 <- c("Male")
        combo.box.2 <- c("65 or more")
        text.box.1 <- 2
        text.box.2 <- 2
        text.box.3 <- 2
        text.box.4 <- 2
        text.box.5 <- 2
        text.box.6 <- 2
        all.combo.boxes <- c(combo.box.1, combo.box.2)
        xlevels <- organiseCategoricalPredictors(input.model, all.combo.boxes)
        DF <- data.frame(
            "Q2" = factor(combo.box.1, levels = xlevels[["Q2"]], ordered = FALSE),
            "Q3" = factor(combo.box.2, levels = xlevels[["Q3"]], ordered = FALSE),
            "Q6_A" = as.numeric(text.box.1),
            "Q6_B" = as.numeric(text.box.2),
            "Q6_C" = as.numeric(text.box.3),
            "Q6_D" = as.numeric(text.box.4),
            "Q6_E" = as.numeric(text.box.5),
            "Q6_F" = as.numeric(text.box.6),
            check.names = FALSE
        )
        outcome <- predictOutcome(input.model, DF, is.numeric = FALSE)
        expect_equal(outcome, "I tend watch what I eat and drink, but don’t consider myself")
        probabilities <- predictProbabilities(input.model, DF)
        expect_equal(attr(probabilities, "dimnames"), list(c("I tend watch what I eat and drink, but don’t consider myself", "I typically eat and drink whatever I feel like", "I am on a diet, so I tend to watch what I eat and drink"), c("Probability (%)")))
        expect_equal(as.vector(probabilities), c(48.4407112954800, 40.7734690820800, 10.7858196224400))
    }
})
