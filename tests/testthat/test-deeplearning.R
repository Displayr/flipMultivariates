context("Deep Learning")

data(adult.2000, package = "flipExampleData")
# create some missing data
adult.2000[["relationship"]][1:200] <- NA
set.seed(1234)
adult.2000[["wgt"]] <- runif(2000) * 10
adult.2000[["subset"]] <- rep(c(TRUE, TRUE, TRUE, FALSE), 500)

test_that("Print Deep Learning: outcome types", {

    # binary outcome, confusion output
    (dl <- DeepLearning(
        sex ~ occupation + relationship + race + workclass + hrs_per_week, data = adult.2000,
        hidden.nodes = 15, max.epochs = 10, output = "Prediction-Accuracy Table"
    )) |>
        expect_warning("Cross validation loss is still decreasing.")
    expect_error(capture.output(print(dl)), NA)
    expect_equal(attr(dl, "ChartData"), ExtractChartData(dl$confusion))

    # multiclass outcoume, xval output, 2 layer
    (dl <- DeepLearning(
        workclass ~ occupation + relationship + race + sex + hrs_per_week, data = adult.2000,
        hidden.nodes = c(20, 10), max.epochs = 10, output = "Cross Validation"
    )) |>
        expect_warning("Cross validation loss is still decreasing.")
    expect_error(capture.output(print(dl)), NA)
    expect_equal(attr(dl, "ChartData"), dl$cross.validation$metrics, check.attributes = FALSE)

    # numeric outcome, accuracy output, no normalization
    (dl <- DeepLearning(
        hrs_per_week ~ occupation + relationship + race + workclass + sex, data = adult.2000,
        hidden.nodes = 8, max.epochs = 10, output = "Accuracy", normalize = FALSE
    )) |>
        expect_warning("Cross validation loss is still decreasing.")
    expect_error(capture.output(print(dl)), NA)
    expect_equal(names(attr(dl, "ChartData")), c("Root Mean Squared Error", "R-squared"))

    # imbalanced multiclass (90% of country is USA), layers output, 3 layer
    (dl <- DeepLearning(
        country ~ ., data = adult.2000,
        hidden.nodes = c(50, 35, 20), max.epochs = 10, output = "Network Layers"
    )) |>
        expect_warning("Cross validation loss is still decreasing.")
    expect_error(capture.output(print(dl)), NA)
    expect_true(startsWith(attr(dl, "ChartData")[1], "Model"))

    # single predictor
    (dl <- DeepLearning(
        occupation ~ age, data = adult.2000,
        hidden.nodes = 5, max.epochs = 10, output = "Detail"
    )) |>
        expect_warning("Cross validation loss is still decreasing.")
    expect_error(capture.output(print(dl)), NA)
    expect_true(startsWith(attr(dl, "ChartData")[1], "Model"))
})

test_that("Deep Learning: Weights and Filters", {
    DeepLearning(
        hrs_per_week ~ ., data = adult.2000,
        hidden.nodes = 15, max.epochs = 10, subset = subset
    ) |>
        expect_warning("Cross validation loss is still decreasing.")
    DeepLearning(
        income ~ ., data = adult.2000,
        hidden.nodes = 15, max.epochs = 10, weights = wgt
    ) |>
        expect_warning("Cross validation loss is still decreasing.")
    DeepLearning(
        occupation ~ ., data = adult.2000,
        hidden.nodes = 15, max.epochs = 10, subset = subset, weights = wgt
    ) |>
        expect_warning("Cross validation loss is still decreasing.")
})


test_that("Deep Learning: predictions and probabilities", {
    (dl <- DeepLearning(
        age ~ ., data = adult.2000,
        hidden.nodes = 20, max.epochs = 10, subset = subset, weights = wgt
    )) |>
        expect_warning("Cross validation loss is still decreasing.")
    predict(dl) |> expect_length(2000)
    Probabilities(dl) |> expect_error("Probabilities are only applicable to models with categorical outcome variables.")

    (dl <- DeepLearning(
        marital ~ ., data = adult.2000,
        hidden.nodes = 20, max.epochs = 10, subset = subset, weights = wgt
    )) |>
        expect_warning("Cross validation loss is still decreasing.")
    predict(dl) |> expect_length(2000)
    prob <- Probabilities(dl)
    prob |> expect_shape(dim = c(2000, 6))
})

test_that("Deep Learning: errors", {
    DeepLearning(
        income ~ ., data = adult.2000,
        hidden.nodes = "10, hello"
    ) |>
        expect_error("Nodes of hidden layers.")
    DeepLearning(
        income ~ ., data = adult.2000,
        hidden.nodes = "20, -5"
    ) |>
        expect_error("Nodes of hidden layers.")
})

test_that("Deep Learning: missing data", {
    DeepLearning(
        sex ~ occupation + relationship + race + workclass + hrs_per_week, data = adult.2000,
        hidden.nodes = 15, max.epochs = 10, output = "Prediction-Accuracy Table",
        missing = "Imputation (replace missing values with estimates)"
    ) |>
        expect_warning("Cross validation loss is still decreasing.")
    DeepLearning(
        sex ~ occupation + relationship + race + workclass + hrs_per_week, data = adult.2000,
        hidden.nodes = 15, max.epochs = 10, output = "Prediction-Accuracy Table",
        missing = "Error if missing data"
    ) |>
        expect_error("The data contains missing values.")
})
