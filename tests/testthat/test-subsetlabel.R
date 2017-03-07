context("subset label")
test_that("Subset info is good",
          {
              data(colas, package = "flipExampleData")
              zLDA <- suppressWarnings(LDA(d1 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, data = colas, prior = "Observed"))
              zLDA
              zLDA <- suppressWarnings(LDA(d1 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, subset = colas$Q5_16_1 == "Yes", data = colas, prior = "Observed"))
              zLDA
              z <- colas$Q5_16_1 == "Yes"
              zLDA <- suppressWarnings(LDA(d1 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, subset = z, data = colas, prior = "Observed"))
              zLDA
              attr(z, "label") <- "A nice, long, subset description"
              zLDA <- suppressWarnings(LDA(d1 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, subset = z, data = colas, prior = "Observed"))
              zLDA
              zw <- as.numeric(unclass(colas$q8)) / 10
              zLDA <- suppressWarnings(LDA(d1 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, subset = z, weights = zw, data = colas, prior = "Observed"))
              zLDA
          }
)
