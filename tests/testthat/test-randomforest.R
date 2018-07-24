context("Random Forest")

test_that("CE-676 Incorrect error about missing values",{
    data(colas, package = "flipExampleData")
    expect_error(suppressWarnings(RandomForest(d1 ~ q2d, data = colas)), NA)
})

data(hbatwithsplits, package = "flipExampleData")
hair <- hbatwithsplits

hair1  <- flipTransformations::AsNumeric(hair[, paste0("x",6:18)], binary = FALSE, remove.first = TRUE)
hair1$x1 <- hair$x1
hair1$split60 <- hair$split60
hair1$id <- hair$id
hair1$num <- suppressWarnings(flipTransformations::AsNumeric(hair1$x1, binary = FALSE))
hair1$numeric <- hair1$num + runif(length(hair1$num)) / 10
attr(hair1$x7, "question") <- "Variable number 7"
hair1$cat <- factor(hair1$num)
hair1 <- cbind(hair1, hair1[, "x18"])
colnames(hair1)[ncol(hair1)] <- "dollar$x18"
# Create a smaller subset of variables for testing dot on RHS
hair2  <- flipTransformations::AsNumeric(hair[, paste0("x",6:18)], binary = FALSE, remove.first = TRUE)

test_that("Random forests: Outputs, labels and outcome variable types",{
    z <- RandomForest(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + dollar$x18,
                      show.labels = TRUE, output = "Importance", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(capture.output(print(z)), NA)
    z <- RandomForest(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + dollar$x18,
                      show.labels = FALSE, output = "Importance", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(capture.output(print(z)), NA)
    z <- RandomForest(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + dollar$x18,
                      show.labels = TRUE, output = "Detail", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(capture.output(print(z)), NA)
    z <- RandomForest(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + dollar$x18,
                      show.labels = FALSE, output = "Detail", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(capture.output(print(z)), NA)
    z <- RandomForest(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + dollar$x18,
                      show.labels = TRUE, output = "Prediction-Accuracy Table", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(capture.output(print(z)), NA)
    z <- RandomForest(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + dollar$x18,
                      show.labels = FALSE, output = "Prediction-Accuracy Table", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(capture.output(print(z)), NA)
    z <- RandomForest(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                      show.labels = TRUE, output = "Importance", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(capture.output(print(z)), NA)
    z <- RandomForest(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                      show.labels = FALSE, output = "Importance", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(capture.output(print(z)), NA)
    z <- RandomForest(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                      show.labels = TRUE, output = "Detail", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(capture.output(print(z)), NA)
    z <- RandomForest(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                      show.labels = FALSE, output = "Detail", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(capture.output(print(z)), NA)
    z <- RandomForest(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                      show.labels = TRUE, output = "Prediction-Accuracy Table", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(capture.output(print(z)), NA)
    z <- RandomForest(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                      show.labels = FALSE, output = "Prediction-Accuracy Table", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(capture.output(print(z)), NA)
})

library(flipRegression)
test_that("Random forests: weights and filters",{

    # no weight, no filter
    expect_error(z <- RandomForest(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                   data = hair1, show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Filtered
    expect_error(z <- RandomForest(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                   data = hair1, subset = split60 == "Estimation Sample", show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Weighted and filtered
    expect_error(z <- RandomForest(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                   data = hair1, weights = hair1$id, subset = split60 == "Estimation Sample", show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Filtered
    expect_error(z <- RandomForest(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                   data = hair1, subset = split60 == "Estimation Sample", show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    ## numeric dependent variable
 # no weight, no filte
    expect_error(z <- suppressWarnings(RandomForest(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                                    data = hair1, show.labels = TRUE), NA))
    Accuracy(z)
    ConfusionMatrix(z)

    # Filtered
    expect_error(z <- suppressWarnings(RandomForest(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                                    data = hair1, subset = split60 == "Estimation Sample", show.labels = TRUE), NA))
    Accuracy(z)
    ConfusionMatrix(z)

    # Weighted and filtered
    expect_error(z <- suppressWarnings(RandomForest(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                                    data = hair1, weights = hair1$id, subset = split60 == "Estimation Sample",
                                                    show.labels = TRUE)), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Filtered
    expect_error(z <- suppressWarnings(RandomForest(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                                    data = hair1, subset = split60 == "Estimation Sample", show.labels = TRUE)), NA)
    Accuracy(z)
    ConfusionMatrix(z)
    })

test_that("Save variables",
{

    z <- RandomForest(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                      show.labels = FALSE, output = "Detail", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(predict(z), NA)
    expect_equal(length(predict(z)), 100)
    expect_error(flipData::Observed(z), NA)
    expect_error(flipData::Probabilities(z))
    z <- RandomForest(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                      show.labels = FALSE, output = "Detail", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(predict(z), NA)
    expect_equal(length(predict(z)), 100)
    expect_error(flipData::Observed(z), NA)
    expect_error(flipData::Probabilities(z), NA)
})

test_that("Random forests: dot in formula",{
    z <- RandomForest(x6 ~ ., show.labels = FALSE, output = "Prediction-Accuracy Table", data = hair2)
    z2 <- RandomForest(x6 ~ x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                       show.labels = FALSE, output = "Prediction-Accuracy Table", data = hair2)
    z$original$call <- z$formula <- NULL
    z2$original$call <- z2$formula <- NULL
    expect_equal(z, z2)
})

test_that("Random forests: missing data",{
    hair2$x6[runif(nrow(hair2)) > 0.8] <- NA
    hair2$x7[runif(nrow(hair2)) > 0.8] <- NA
    expect_error(RandomForest(x6 ~ x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                       show.labels = FALSE, output = "Prediction-Accuracy Table",
                       missing = "Imputation (replace missing values with estimates)", data = hair2), NA)
    expect_error(RandomForest(x6 ~ x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = FALSE, output = "Prediction-Accuracy Table",
                              missing = "Error if missing data", data = hair2), "The data contains missing values.")
})
