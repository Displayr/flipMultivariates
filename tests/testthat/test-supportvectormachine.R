context("Support Vector Machine")

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
# Create a smaller subset of variables for testing dot on RHS
hair2  <- flipTransformations::AsNumeric(hair[, paste0("x",6:18)], binary = FALSE, remove.first = TRUE)

test_that("SVM: Costs and outputs",{
    z <- SupportVectorMachine(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = TRUE, output = "Accuracy", data = hair1,
                              subset = split60 == "Estimation Sample", cost = 0.001)
    expect_error(capture.output(print(z)), NA)
    z <- SupportVectorMachine(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = FALSE, output = "Accuracy", data = hair1,
                              subset = split60 == "Estimation Sample", cost = 1000000)
    expect_error(capture.output(print(z)), NA)
    z <- SupportVectorMachine(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = TRUE, output = "Prediction-Accuracy Table", data = hair1,
                              subset = split60 == "Estimation Sample", cost = 1000)
    expect_error(capture.output(print(z)), NA)
    z <- SupportVectorMachine(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = FALSE, output = "Prediction-Accuracy Table", data = hair1,
                              subset = split60 == "Estimation Sample", cost = 100)
    expect_error(capture.output(print(z)), NA)
    z <- SupportVectorMachine(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = TRUE, output = "Detail", data = hair1,
                              subset = split60 == "Estimation Sample", cost = 0.00001)
    expect_error(capture.output(print(z)), NA)
    z <- SupportVectorMachine(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = FALSE, output = "Detail", data = hair1,
                              subset = split60 == "Estimation Sample", cost = 1000000000)
    expect_error(capture.output(print(z)), NA)
    z <- SupportVectorMachine(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = TRUE, output = "Accuracy", data = hair1,
                              subset = split60 == "Estimation Sample", cost = 0.001)
    expect_error(capture.output(print(z)), NA)
    z <- SupportVectorMachine(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = FALSE, output = "Accuracy", data = hair1,
                              subset = split60 == "Estimation Sample", cost = 1000000)
    expect_error(capture.output(print(z)), NA)
    z <- SupportVectorMachine(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = TRUE, output = "Prediction-Accuracy Table", data = hair1,
                              subset = split60 == "Estimation Sample", cost = 1000)
    expect_error(capture.output(print(z)), NA)
    z <- SupportVectorMachine(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = FALSE, output = "Prediction-Accuracy Table", data = hair1,
                              subset = split60 == "Estimation Sample", cost = 100)
    expect_error(capture.output(print(z)), NA)
    z <- SupportVectorMachine(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = TRUE, output = "Detail", data = hair1,
                              subset = split60 == "Estimation Sample", cost = 0.00001)
    expect_error(capture.output(print(z)), NA)
    z <- SupportVectorMachine(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = FALSE, output = "Detail", data = hair1,
                              subset = split60 == "Estimation Sample", cost = 1000000000)
    expect_error(capture.output(print(z)), NA)
})

test_that("SVM: Dot in formula",{
    z <- SupportVectorMachine(x6 ~ ., show.labels = FALSE, output = "Prediction-Accuracy Table", data = hair2)
    z2 <- SupportVectorMachine(x6 ~ x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                               show.labels = FALSE, output = "Prediction-Accuracy Table", data = hair2)
    z$original$call <- z$formula <- NULL
    z2$original$call <- z2$formula <- NULL
    expect_equal(z, z2)
})


library(flipRegression)
test_that("SVM: Weights and filters",{

    # no weight, no filter
    expect_error(z <- SupportVectorMachine(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           data = hair1, show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Filtered
    expect_error(z <- SupportVectorMachine(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           data = hair1, subset = split60 == "Estimation Sample", show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Weighted
    expect_error(z <- SupportVectorMachine(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           data = hair1, weights = hair1$id, show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Weighted and filtered
    expect_error(z <- SupportVectorMachine(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           data = hair1, weights = hair1$id, subset = split60 == "Estimation Sample",
                                           show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    ## numeric dependent variable
    # no weight, no filter
    expect_error(z <- SupportVectorMachine(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           data = hair1, show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Filtered
    expect_error(z <- SupportVectorMachine(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           data = hair1, subset = split60 == "Estimation Sample", show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Weighted
    expect_error(z <- SupportVectorMachine(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           data = hair1, weights = hair1$id, show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Weighted and filtered
    expect_error(z <- SupportVectorMachine(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           data = hair1, weights = hair1$id, subset = split60 == "Estimation Sample",
                                           show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)
})

test_that("SVM: Errors",{

    # zero and negative costs
    expect_error(z <- SupportVectorMachine(x1 ~ x6 + x7 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           data = hair1, show.labels = FALSE, cost = 0), "cost must be positive")
    expect_error(z <- SupportVectorMachine(x1 ~ x6 + x7 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           data = hair1, show.labels = TRUE, cost = -10.5), "cost must be positive")
    # insufficient data
    expect_error(z <- SupportVectorMachine(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           data = hair1[1:5, ], show.labels = TRUE), "There are fewer observations*")
})

test_that("SVM: Save variables",
          {
              z <- SupportVectorMachine(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                        show.labels = FALSE, output = "Detail", data = hair1,
                                        subset = split60 == "Estimation Sample")
              expect_error(predict(z), NA)
              expect_equal(length(predict(z)), 100)
              expect_error(flipData::Observed(z), NA)
              expect_error(flipData::Probabilities(z),
                           "Probabilities are only applicable to models with categorical outcome variables.")
              z <- SupportVectorMachine(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                        show.labels = FALSE, output = "Detail", data = hair1,
                                        subset = split60 == "Estimation Sample")
              expect_error(predict(z), NA)
              expect_equal(length(predict(z)), 100)
              expect_error(flipData::Observed(z), NA)
              expect_error(flipData::Probabilities(z), NA)
          })

test_that("SVM: missing data",{
    hair2$x6[runif(nrow(hair2)) > 0.8] <- NA
    hair2$x7[runif(nrow(hair2)) > 0.8] <- NA
    expect_error(SupportVectorMachine(x6 ~ x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = FALSE, output = "Prediction-Accuracy Table",
                              missing = "Imputation (replace missing values with estimates)", data = hair2), NA)
    expect_error(SupportVectorMachine(x6 ~ x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = FALSE, output = "Prediction-Accuracy Table",
                              missing = "Error if missing data", data = hair2), "The data contains missing values.")
})
