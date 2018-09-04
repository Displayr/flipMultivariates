context("Gradient Boost")

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
hair1$bin <- hair1$num > 1
# Create a smaller subset of variables for testing dot on RHS
hair2  <- flipTransformations::AsNumeric(hair[, paste0("x",6:18)], binary = FALSE, remove.first = TRUE)

test_that("Print Gradient Boost: outputs and boosters",{
    for (output in c("Detail", "Accuracy", "Prediction-Accuracy Table"))
        for (booster in c("gbtree", "gblinear"))
            for (grid.search in c(TRUE, FALSE))
            {
                z <- GradientBoost(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                   show.labels = TRUE, output = output, data = hair1, subset = split60 == "Estimation Sample",
                                   booster = booster, grid.search = grid.search)
                expect_error(capture.output(print(z)), NA)
                z <- GradientBoost(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                   show.labels = FALSE, output = output, data = hair1, subset = split60 == "Estimation Sample",
                                   booster = booster, grid.search = grid.search)
                expect_error(capture.output(print(z)), NA)
            }
})

test_that("Print Gradient Boost: binary outcome",{
    for (booster in c("gbtree", "gblinear"))
    {
        z <- GradientBoost(bin ~ x6 + x7 + x8 + x9,
                           show.labels = FALSE, output = "Accuracy", data = hair1, subset = split60 == "Estimation Sample",
                           booster = booster, grid.search = FALSE)
        expect_error(capture.output(print(z)), NA)
    }
})

test_that("Print Gradient Boost Importance",{
    for (grid.search in c(TRUE, FALSE))
    {
        z <- GradientBoost(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                           show.labels = TRUE, output = "Importance", data = hair1, subset = split60 == "Estimation Sample",
                            booster = "gbtree", grid.search = grid.search)
        expect_error(capture.output(print(z)), NA)
        z <- GradientBoost(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                           show.labels = FALSE, output = "Imporance", data = hair1, subset = split60 == "Estimation Sample",
                           booster = "gbtree", grid.search = grid.search)
        expect_error(capture.output(print(z)), NA)
            }
})

library(flipRegression)
test_that("Gradient Boost Weights and Filters",{

    # no weight, no filter
    expect_error(z <- GradientBoost(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                    data = hair1, booster = "gbtree"), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Filtered
    expect_error(z <- GradientBoost(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                    data = hair1, subset = split60 == "Estimation Sample", booster = "gblinear"), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Weighted
    expect_error(z <- GradientBoost(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                    data = hair1, weights = hair1$id, booster = "gblinear"), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Weighted and filtered
    expect_error(z <- GradientBoost(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                    data = hair1, weights = hair1$id, subset = split60 == "Estimation Sample",
                                    booster = "gbtree"), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    ## numeric dependent variable
    # no weight, no filter
    expect_error(z <- GradientBoost(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                    data = hair1, booster = "gblinear"), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Filtered
    expect_error(z <- GradientBoost(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                    data = hair1, subset = split60 == "Estimation Sample", booster = "gbtree"), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Weighted
    expect_error(z <- GradientBoost(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                    data = hair1, weights = hair1$id, booster = "gbtree"), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Weighted and filtered
    expect_error(z <- GradientBoost(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                    data = hair1, weights = hair1$id, subset = split60 == "Estimation Sample",
                                    booster = "gblinear"), NA)
    Accuracy(z)
    ConfusionMatrix(z)
})

test_that("Gradient Boost Errors",{

    # insufficient data
    expect_error(z <- GradientBoost(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                    data = hair1[1:5, ], show.labels = TRUE), "There are fewer observations*")
    # importance for linear booster
    expect_error(z <- GradientBoost(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12, data = hair1, booster = "gblinear",
                                    output = "Importance", show.labels = TRUE), "Importance is only available for*")
})

test_that("Save variables",
          {
              z <- GradientBoost(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                 show.labels = FALSE, output = "Detail", data = hair1, subset = split60 == "Estimation Sample")
              expect_error(predict(z), NA)
              expect_equal(length(predict(z)), 100)
              expect_error(flipData::Observed(z), NA)
              expect_error(flipData::Probabilities(z), "Probabilities are only applicable to models with categorical outcome variables.")
              z <- GradientBoost(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                 booster = "gbtree", data = hair1, subset = split60 == "Estimation Sample")
              expect_error(predict(z), NA)
              expect_equal(length(predict(z)), 100)
              expect_error(flipData::Observed(z), NA)
              expect_error(flipData::Probabilities(z), NA)
              z <- GradientBoost(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                 booster = "gblinear", data = hair1, subset = split60 == "Estimation Sample")
              expect_error(predict(z), NA)
              expect_equal(length(predict(z)), 100)
              expect_error(flipData::Observed(z), NA)
              expect_error(flipData::Probabilities(z), NA)
})

test_that("Gradient Boost: dot in formula",{
    z <- GradientBoost(x6 ~ ., data = hair2, booster = "gblinear")
    z2 <- GradientBoost(x6 ~ x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                        data = hair2, booster = "gblinear")
    z$original$call <- z$formula <- NULL
    z2$original$call <- z2$formula <- NULL
    expect_equal(z, z2)
})

test_that("Gradient Boost: missing data",{
    hair2$x6[runif(nrow(hair2)) > 0.8] <- NA
    hair2$x7[runif(nrow(hair2)) > 0.8] <- NA
    expect_error(GradientBoost(x6 ~ x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = FALSE, output = "Prediction-Accuracy Table",
                              missing = "Imputation (replace missing values with estimates)", data = hair2), NA)
    expect_error(GradientBoost(x6 ~ x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              show.labels = FALSE, output = "Prediction-Accuracy Table",
                              missing = "Error if missing data", data = hair2), "The data contains missing values.")
})
