context("LDA")

# x1 <- runif(100)
# x2 <- runif(100) * 3
# flipFormat::Labels(x1) <- "Predictors x1"
# flipFormat::Labels(x2) <- "Predictors x2"
# y <- x1 + x2
# y <- round(y / max(y) * 2)
# LDA(y ~ x1 + x2)
# dat <- data.frame(x1, x2)
# flipFormat::ExtractCommonPrefix(flipFormat::Labels(dat))

data(hbatwithsplits, package = "flipExampleData")
hair <- hbatwithsplits

hair1  <- flipTransformations::AsNumeric(hair[, paste0("x",6:18)], binary = FALSE, remove.first = TRUE)
hair1$x1 <- hair$x1
hair1$split60 <- hair$split60
hair1$id <- hair$id
hair2 <- hair1
hair2$split60 <- hair2$id <- NULL


test_that("LDA: plots",
          {
              expect_error(suppressWarnings(LDA(x6 ~ x12 + x13 + x14 + x15 + x16 + x17 + x18, method = "moment",
                                                data = hair1, show.labels = TRUE, output = "Confusion Matrix"))
                           , "LDA requires the outcome variable to be categorical or a count.")
              zLDA <- suppressWarnings(LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           method = "moment", data = hair1, subset = split60 == "Estimation Sample",
                                           show.labels = TRUE, output = "Scatterplot"))
              expect_error(print(zLDA), NA)
              zLDA <- suppressWarnings(LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           method = "moment", data = hair1, subset = split60 == "Estimation Sample",
                                           show.labels = TRUE, output = "Moonplot"))
              expect_error(print(zLDA), NA)
              zLDA <- suppressWarnings(LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           method = "moment", data = hair1, subset = split60 == "Estimation Sample",
                                           show.labels = TRUE, output = "Prediction-Accuracy Table"))
              expect_error(print(zLDA), NA)
              hair1$X1 <- hair1$x1
              flipFormat::Labels(hair1$X1) <- "Duration"
              hair1$X6 <- hair1$x6
              flipFormat::Labels(hair1$X6) <- "Importance Pizza"
              hair1$X7 <- hair1$x7
              flipFormat::Labels(hair1$X7) <- "Importance Paste"
              hair1$X8 <- hair1$x8
              flipFormat::Labels(hair1$X8) <- "Importance Risotto"
              suppressWarnings(LDA(X1 ~ X6 + X7 + X8, data = hair1, output = "Scatterplot", show.labels = FALSE))
              suppressWarnings(LDA(X1 ~ X6 + X7 + X8, data = hair1, output = "Scatterplot", show.labels = TRUE))
              suppressWarnings(LDA(X1 ~ X6 + X7 + X8, data = hair1, output = "Moonplot", show.labels = FALSE))
              suppressWarnings(LDA(X1 ~ X6 + X7 + X8, data = hair1, output = "Moonplot", show.labels = TRUE))
              suppressWarnings(LDA(X1 ~ X6 + X7 + X8, data = hair1, output = "Prediction-Accuracy Table", show.labels = FALSE))
              suppressWarnings(LDA(X1 ~ X6 + X7 + X8, data = hair1, output = "Prediction-Accuracy Table", show.labels = TRUE))
              suppressWarnings(LDA(X1 ~ X6 + X7 + X8, data = hair1, show.labels = FALSE))
              suppressWarnings(LDA(X1 ~ X6 + X7 + X8, data = hair1, show.labels = TRUE))
          })

test_that("System is computationally singular)",
           {
               hair1$x <- rnorm(100)
               hair1$y <- rnorm(100)
               hair1$z <- hair1$x
                expect_warning(print(LDA(x1 ~ x + y + z, data = hair1)), "Variables are colinear.")
           })


# In SPSS, the priors are always the oberved priors when fitting the model. In MASS:lda, the priors are used when fitting.
test_that("Replicating SPSS defaults using MASS:LDA",
          {
              #### Reproducing SPSS default results.
              # Mass.
              library(MASS)
              zlda = lda(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                         data = hair1, subset = split60 == "Estimation Sample", method = "moment")
              variance.explained <- round(zlda$svd^2/sum(zlda$svd^2), 4L)
              expect_equal(0.86959956, variance.explained[1], tolerance = 0.001)
              zlda.discriminant.variables <- MASS:::predict.lda(zlda, newdata = hair1[,1:13], na.action = na.pass)[["x"]]
              expect_equal(.45479754052345, abs(zlda.discriminant.variables[2,2]), tolerance = 0.001)
              zlda.probs <- MASS:::predict.lda(zlda, newdata = hair1[,1:13], prior = rep(1/3, 3), na.action = na.pass)[["posterior"]]
              expect_equal(.83131933755411, zlda.probs[5,3], tolerance = 0.001)
          })

test_that("LDA Replicating SPSS defaults - unweighted",
          {
              zLDA <- suppressWarnings(LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           method = "moment", data = hair1, subset = split60 == "Estimation Sample",
                                           show.labels = TRUE, prior = "Observed"))
              variance.explained <- round(zLDA$original$svd^2/sum(zLDA$original$svd^2), 4L)
              expect_equal(0.86959956, variance.explained[1], tolerance = 0.001)
              zLDA.discriminant.variables <- DiscriminantVariables(zLDA)
              expect_equal(.45479754052345, as.numeric(abs(zLDA.discriminant.variables[2,2])), tolerance = 0.001)
              zLDA.probs <- flipData::Probabilities(zLDA)
              expect_equal(0.8929186, as.numeric(zLDA.probs[5,3]), tolerance = 0.001)
              expect_equal(as.numeric(zLDA$confusion), c(21,0,0,1,10,0,0,3,25))
              expect_equal(24.56271, zLDA$original$discriminant.functions[2,2], tolerance = 0.001)
          }
)

test_that("LDA Replicating SPSS defaults - weighted",
          {
              zLDA <- suppressWarnings(LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           method = "moment", data = hair1, weights = hair1$id,
                                           subset = split60 == "Estimation Sample", show.labels = TRUE, prior = "Observed"))
              variance.explained <- round(zLDA$original$svd^2/sum(zLDA$original$svd^2), 4L)
              expect_equal(0.787, variance.explained[1], tolerance = 0.001)
              zLDA.discriminant.variables <- DiscriminantVariables(zLDA)
              expect_equal(0.07987, as.numeric(abs(zLDA.discriminant.variables[2,2])), tolerance = 0.001)
              zLDA.probs <- flipData::Probabilities(zLDA)
              expect_equal(0.9294585, as.numeric(zLDA.probs[5,3]), tolerance = 0.001)
              expect_equal(as.numeric(zLDA$confusion), c(1056,8,0,10,493,0,0,43,1107))
              expect_equal(19.98625, zLDA$original$discriminant.functions[3,3], tolerance = 0.001)
          }
)


test_that("LDA Replicating SPSS - compute prior from group sizes - unweighted",
          {
              zLDA <- suppressWarnings(LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           method = "moment", data = hair1, prior = "Observed",
                                           subset = split60 == "Estimation Sample", show.labels = TRUE))
              variance.explained <- round(zLDA$original$svd^2/sum(zLDA$original$svd^2), 4L)
              expect_equal(0.87, variance.explained[1], tolerance = 0.001)
              zLDA.discriminant.variables <- DiscriminantVariables(zLDA)
              expect_equal(0.45480, abs(as.numeric(abs(zLDA.discriminant.variables[2,2]))), tolerance = 0.001)
              zLDA.probs <- flipData::Probabilities(zLDA)
              expect_equal(0.89292, as.numeric(zLDA.probs[5,3]), tolerance = 0.001)
              expect_equal(as.numeric(zLDA$confusion), c(21,0,0,1,10,0,0,3,25))
              expect_equal(-7.578596, zLDA$original$discriminant.functions[4,1], tolerance = 0.001)
          }
)

test_that("LDA Replicating SPSS - compute prior from group sizes - weighted",
          {
              zLDA <- suppressWarnings(LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           method = "moment", data = hair1, prior = "Observed",
                                           weights = hair1$id, subset = split60 == "Estimation Sample", show.labels = TRUE))
              variance.explained <- round(zLDA$original$svd^2/sum(zLDA$original$svd^2), 4L)
              expect_equal(0.787, variance.explained[1], tolerance = 0.001)
              zLDA.discriminant.variables <- DiscriminantVariables(zLDA)
              expect_equal(0.07987, abs(as.numeric(abs(zLDA.discriminant.variables[2,2]))), tolerance = 0.001)
              zLDA.probs <- flipData::Probabilities(zLDA)
              expect_equal(0.92946, as.numeric(zLDA.probs[5,3]), tolerance = 0.001)
              expect_equal(as.numeric(zLDA$confusion), c(1056,8,0,10,493,0,0,43,1107))
              expect_equal(-359.3452, zLDA$original$discriminant.functions[1,1], tolerance = 0.001)
          }
)


test_that("Replicating colas example in SPSS - default", {
    data(colas, package = "flipExampleData")
              zLDA <- suppressWarnings(LDA(q3 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, data = colas, prior = "Observed"))
              variance.explained <- round(zLDA$original$svd^2/sum(zLDA$original$svd^2), 4L)
              expect_equal(0.7650, variance.explained[1], tolerance = 0.001)
              zLDA.discriminant.variables <- DiscriminantVariables(zLDA)
              expect_equal(0.3144915587291, abs(as.numeric(abs(zLDA.discriminant.variables[2,2]))), tolerance = 0.001)
              zLDA.probs <- flipData::Probabilities(zLDA)
              expect_equal(0.1965148, as.numeric(zLDA.probs[5,3]), tolerance = 0.001)
              expect_equal(-0.9738335, zLDA$original$discriminant.functions[1,1], tolerance = 0.001)
              # Note that confusion does not match, due to multiple groups with equivalent probabilities.
})


test_that("Replicating colas example in SPSS - compute from group sizes", {
    data(colas, package = "flipExampleData")
              zLDA <- suppressWarnings(LDA(q3 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, data = colas, prior = "Observed"))
              variance.explained <- round(zLDA$original$svd^2/sum(zLDA$original$svd^2), 4L)
              expect_equal(0.7650, variance.explained[1], tolerance = 0.001)
              zLDA.discriminant.variables <- DiscriminantVariables(zLDA)
              expect_equal(.3144915587291, abs(as.numeric(abs(zLDA.discriminant.variables[2,2]))), tolerance = 0.001)
              zLDA.probs <- flipData::Probabilities(zLDA)
              expect_equal(.19651, as.numeric(zLDA.probs[5,3]), tolerance = 0.001)
              expect_equal(as.numeric(zLDA$confusion[, 1]), c(141, 37, 61, 25, 9, 43, 2, 3))
              expect_equal(0.2883744, zLDA$original$discriminant.functions[2,2], tolerance = 0.001)
})

test_that("LDA wrong prior", {
    data(colas, package = "flipExampleData")
    zLDA <- expect_error(suppressWarnings(LDA(q3 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, data = colas, prior = rep(0.1, 8)),
                                        "The 'prior' must be one of."))
})


hair2$x6[5:10] <- NA

test_that("LDA: dot in formula", {
    zLDA <- suppressWarnings(LDA(x1 ~ ., method = "moment", data = hair2, subset = hair1$split60 == "Estimation Sample",
                                 show.labels = TRUE, output = "Means"))
    zLDA2 <- suppressWarnings(LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                              method = "moment", data = hair2, subset = hair1$split60 == "Estimation Sample",
                              show.labels = TRUE, output = "Means"))
    zLDA$original$call <- zLDA$formula <- zLDA$call <- NULL
    zLDA2$original$call <- zLDA2$formula <- zLDA2$call <- NULL
    expect_equal(zLDA, zLDA2)
})

test_that("LDA: missing data",{
    hair2$x1[runif(nrow(hair2)) > 0.8] <- NA
    hair2$x6[runif(nrow(hair2)) > 0.8] <- NA
    expect_warning(LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12,
                              show.labels = FALSE, output = "Prediction-Accuracy Table",
                              missing = "Imputation (replace missing values with estimates)", data = hair2),
                                "The smallest category of the outcome variable.")
    expect_error(LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12,
                              show.labels = FALSE, output = "Prediction-Accuracy Table",
                              missing = "Error if missing data", data = hair2), "The data contains missing values.")
})


# In SPSS, the priors are always the oberved priors when fitting the model. In MASS:lda, the priors are used when fitting.
