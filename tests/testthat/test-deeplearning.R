context("Deep Learning")

data(adult.2000, package = "flipExampleData")
# create some missing data
adult.2000$relationship[1:200] <- NA
set.seed(1234)
adult.2000$wgt <- runif(2000) * 10
adult.2000$subset <- rep(c(TRUE, TRUE, TRUE, FALSE), 500)

test_that("Print Deep Learning: outcome types", {

    # binary outcome, confusion output
    dl <- suppressWarnings(DeepLearning(sex ~ occupation + relationship + race + workclass + hrs_per_week, data = adult.2000,
                       hidden.nodes = 15, max.epochs = 10, output = "Prediction-Accuracy Table"))
    expect_error(capture.output(print(dl)), NA)
    expect_equal(attr(dl, "ChartData"), ExtractChartData(dl$confusion))

    # multiclass outcoume, xval output, 2 layer
    dl <- suppressWarnings(DeepLearning(workclass ~ occupation + relationship + race + sex + hrs_per_week, data = adult.2000,
                        hidden.nodes = c(20, 10), max.epochs = 10, output = "Cross Validation"))
    expect_error(capture.output(print(dl)), NA)
    expect_equal(attr(dl, "ChartData"), dl$cross.validation$metrics, check.attributes = FALSE)

    # numeric outcome, accuracy output, no normalization
    dl <- suppressWarnings(DeepLearning(hrs_per_week ~ occupation + relationship + race + workclass + sex, data = adult.2000,
                       hidden.nodes = 8, max.epochs = 10, output = "Accuracy", normalize = FALSE))
    expect_error(capture.output(print(dl)), NA)
    expect_equal(names(attr(dl, "ChartData")), c('Root Mean Squared Error', 'R-squared'))

    # imbalanced multiclass (90% of country is USA), layers output, 3 layer
    dl <- suppressWarnings(DeepLearning(country ~ ., data = adult.2000,
                        hidden.nodes = c(50, 35, 20), max.epochs = 10, output = "Network Layers"))
    expect_error(capture.output(print(dl)), NA)
    expect_true(startsWith(attr(dl, "ChartData")[1] , "Model"))

    # single predictor
    dl <- suppressWarnings(DeepLearning(occupation ~ age, data = adult.2000,
                                        hidden.nodes = 5, max.epochs = 10, output = "Detail"))
    expect_error(capture.output(print(dl)), NA)
    expect_true(startsWith(attr(dl, "ChartData")[1], "Model"))

})

test_that("Deep Learning: Weights and Filters", {

    expect_error(dl <- suppressWarnings(DeepLearning(hrs_per_week ~ ., data = adult.2000,
                                    hidden.nodes = 15, max.epochs = 10, subset = subset)), NA)
    expect_error(dl <- suppressWarnings(DeepLearning(income ~ ., data = adult.2000,
                                    hidden.nodes = 15, max.epochs = 10, weights = wgt)), NA)
    expect_error(dl <- suppressWarnings(DeepLearning(occupation ~ ., data = adult.2000,
                                    hidden.nodes = 15, max.epochs = 10, subset = subset, weights = wgt)), NA)
})


test_that("Deep Learning: predictions and probabilities",
          {
              dl <- suppressWarnings(DeepLearning(age ~ ., data = adult.2000,
                                              hidden.nodes = 20, max.epochs = 10, subset = subset, weights = wgt))
              expect_equal(length(suppressWarnings(predict(dl))), 2000)
              expect_error(Probabilities(dl), "Probabilities are only applicable to models with categorical outcome variables.")

              dl <- suppressWarnings(DeepLearning(marital ~ ., data = adult.2000,
                                 hidden.nodes = 20, max.epochs = 10, subset = subset, weights = wgt))
              expect_equal(length(suppressWarnings(predict(dl))), 2000)
              expect_error(suppressWarnings(Probabilities(dl)), NA)
})

test_that("Deep Learning: errors",{

    expect_error(DeepLearning(income ~ ., data = adult.2000,
                                                     hidden.nodes = "10, hello"), "Nodes of hidden layers.")
    expect_error(DeepLearning(income ~ ., data = adult.2000,
                                                     hidden.nodes = "20, -5"), "Nodes of hidden layers.")
})

test_that("Deep Learning: missing data",{
    expect_warning(DeepLearning(sex ~ occupation + relationship + race + workclass + hrs_per_week, data = adult.2000,
                              hidden.nodes = 15, max.epochs = 10, output = "Prediction-Accuracy Table",
                              missing = "Imputation (replace missing values with estimates)"),
                                "Cross validation loss is still decreasing.")
    expect_error(DeepLearning(sex ~ occupation + relationship + race + workclass + hrs_per_week, data = adult.2000,
                                hidden.nodes = 15, max.epochs = 10, output = "Prediction-Accuracy Table",
                                missing = "Error if missing data"), "The data contains missing values.")
})
