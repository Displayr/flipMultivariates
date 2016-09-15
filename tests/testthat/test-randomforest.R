context("Random Forest")

data(hbatwithsplits, package = "flipExampleData")
hair <- hbatwithsplits

hair1  <- flipTransformations::AsNumeric(hair[, paste0("x",6:18)], binary = FALSE, remove.first = TRUE)
hair1$x1 <- hair$x1
hair1$split60 <- hair$split60
hair1$id <- hair$id
    hair1$num <- flipTransformations::AsNumeric(hair1$x1, binary = FALSE)

test_that("Print Random forests",{
    hair1$numeric <- hair1$num + runif(length(hair1$num)) / 10
    attr(hair1$x7, "question") <- "Variable number 7"
    z <- RandomForest(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, show.labels = TRUE, output = "Importance", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(z, NA)
    z <- RandomForest(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, show.labels = FALSE, output = "Importance", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(z, NA)
    z <- RandomForest(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, show.labels = TRUE, output = "Detail", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(z, NA)
    z <- RandomForest(numeric ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, show.labels = FALSE, output = "Detail", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(z, NA)
    hair1$cat <- factor(hair1$num)
    z <- RandomForest(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, show.labels = TRUE, output = "Importance", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(z, NA)
    z <- RandomForest(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, show.labels = FALSE, output = "Importance", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(z, NA)
    z <- RandomForest(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, show.labels = TRUE, output = "Detail", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(z, NA)
    z <- RandomForest(cat ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, show.labels = FALSE, output = "Detail", data = hair1, subset = split60 == "Estimation Sample")
    expect_error(z, NA)
})

library(flipRegression)
test_that("Random forests",{

    # no weight, no filte
    expect_error(z <- RandomForest(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data = hair1, show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Filtered
    expect_error(z <- RandomForest(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data = hair1, subset = split60 == "Estimation Sample", show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Weighted and filtered
    expect_error(z <- RandomForest(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data = hair1, weights = hair1$id, subset = split60 == "Estimation Sample", show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Filtered
    expect_error(z <- RandomForest(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data = hair1, subset = split60 == "Estimation Sample", show.labels = TRUE), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    ## numeric dependent variable
 # no weight, no filte
    expect_error(z <- suppressWarnings(RandomForest(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data = hair1, show.labels = TRUE), NA))
    Accuracy(z)
    ConfusionMatrix(z)

    # Filtered
    expect_error(z <- suppressWarnings(RandomForest(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data = hair1, subset = split60 == "Estimation Sample", show.labels = TRUE), NA))
    Accuracy(z)
    ConfusionMatrix(z)

    # Weighted and filtered
    expect_error(z <- suppressWarnings(RandomForest(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data = hair1, weights = hair1$id, subset = split60 == "Estimation Sample", show.labels = TRUE)), NA)
    Accuracy(z)
    ConfusionMatrix(z)

    # Filtered
    expect_error(z <- suppressWarnings(RandomForest(num ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data = hair1, subset = split60 == "Estimation Sample", show.labels = TRUE)), NA)
    Accuracy(z)
    ConfusionMatrix(z)
    })

