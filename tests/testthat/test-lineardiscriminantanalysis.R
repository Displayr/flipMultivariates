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

              chart.dat <- structure(c(-2.40270212048022, -0.38347784494597, 2.3137863453945,
                                       3.23130876867615, -0.443477820196444, 0.530232594522982, 3.59454746040973,
                                       -0.0493608999949436, 3.65010223613683, 0.229409884683453, -2.42165670744295,
                                       0.995933738442906, 0.433713532891357, 2.98425747335485, -0.133295738357835,
                                       3.40255715844592, 0.518272387014377, -1.53367270819624, 0.341430107689394,
                                       1.18022258606677, -0.630008440453795, -0.43066522130126, -0.931270319099291,
                                       -0.678378921931945, 0.230057553267276, -0.80591921707889, -0.434283879369377,
                                       -0.730858735891654, 0.0223151985836516, -1.16703971960613, -1.90751457543177,
                                       -1.22275817781825), .Dim = c(16L, 2L), .Dimnames = list(c("Less than 1 year",
                                       "1 to 5 years", "Over 5 years", "x6", "x7", "x8", "x9", "x10",
                                       "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18"), c("LD1", "LD2")))
              expect_equal(attr(zLDA, "ChartData"), chart.dat)

              zLDA <- suppressWarnings(LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           method = "moment", data = hair1, subset = split60 == "Estimation Sample",
                                           show.labels = TRUE, output = "Moonplot"))
              expect_error(print(zLDA), NA)
              expect_equal(attr(zLDA, "ChartData"), chart.dat)

              zLDA <- suppressWarnings(LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18,
                                           method = "moment", data = hair1, subset = split60 == "Estimation Sample",
                                           show.labels = TRUE, output = "Prediction-Accuracy Table"))
              expect_error(print(zLDA), NA)
              expect_equal(attr(zLDA, "ChartData"), ExtractChartData(zLDA$confusion))

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
                                           method = "moment", data = hair1, prior = "Observed", output = "Means",
                                           weights = hair1$id, subset = split60 == "Estimation Sample", show.labels = TRUE))

              expect_equal(attr(zLDA, "ChartData"), structure(c(7.24437148217636, 3.46397748592871, 4.47664165103189,
                                3.9202626641651, 3.58227016885553, 4.90891181988743, 4.52626641651032,
                                7.11022514071295, 5.55037523452158, 4.7077861163227, 3.15881801125704,
                                3.85384615384615, 2.97110694183865, 6.65514705882353, 3.58897058823529,
                                6.12922794117647, 6.01764705882353, 4.45919117647059, 5.64209558823529,
                                5.19944852941176, 7.2375, 6.44779411764706, 5.41783088235294,
                                4.68051470588235, 5.50569852941177, 4.27481617647059, 9.09566395663957,
                                3.43757904245709, 5.57967479674797, 6.30957542908762, 3.94182475158085,
                                6.9289972899729, 4.97208672086721, 5.64733514001807, 6.08563685636856,
                                5.3349593495935, 4.61996386630533, 4.05871725383921, 4.20578139114725,
                                0.531333942621244, 0.0410825933510373, 0.0267111883273091, 0.585708825864053,
                                0.0386762017579645, 0.533420779027016, 0.05653914609799, 0.248650932783968,
                                0.0841609994183441, 0.0075103300872595, 0.467772114218906, 0.305738726431116,
                                0.585007716402078, 1.82787618374647e-10, 0.361443659250526, 0.601056506239137,
                                5.94653493113384e-12, 0.331205336516674, 3.9773209725702e-10,
                                0.199630239638239, 0.000155258576763351, 0.0814002174179213,
                                1, 5.75728985319213e-09, 1.39183742617899e-05, 4.36342628695741e-12
              ), .Dim = c(13L, 5L), .Dimnames = list(c(x6 = "x6", x7 = "x7",
                               x8 = "x8", x9 = "x9", x10 = "x10", x11 = "x11", x12 = "x12",
                               x13 = "x13", x14 = "x14", x15 = "x15", x16 = "x16", x17 = "x17",
                               x18 = "x18"), c("Less than 1 year", "1 to 5 years", "Over 5 years",
                               "r.squared", "p.values"))))
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
    zLDA <- expect_error(expect_warning(LDA(q3 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, data = colas, prior = rep(0.1, 8)),
                                        paste0("The smallest category of the outcome variable (Dislike all cola) ",
                                               "contains 2 observations; a robust model is unlikely."),
                                        fixed = TRUE),
                         "The 'prior' must be one of.")
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

test_that("Output contains the right class for extension buttons", {
    # NOTE: if the test below fails due to class names changing, ALL
    #       extension buttons in the wiki that refer to this class name should
    #       be updated with the new class name.

    result <- suppressWarnings(LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12,
                                  show.labels = FALSE,
                                  output = "Prediction-Accuracy Table",
                                  data = hair))

    expect_true(inherits(result, "LDA"))
})

# In SPSS, the priors are always the oberved priors when fitting the model. In MASS:lda, the priors are used when fitting.

# Ensure that factor levels are not being converted to a matrix. DS-3583
test_that("Factor level info stored as list", {
    data(colas, package = "flipExampleData")
    expect_warning(zLDA <- LDA(q3 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, data = colas, prior = "Observed"), "smallest category of the outcome")
    expect_type(zLDA$factor.levels, "list")
})

test_that("DS-3851 Character varibles are handled appropriately", {
    test <- replicate(4L, sample(letters[1:3], size = 100, replace = TRUE), simplify = FALSE)
    test <- as.data.frame(test)
    names(test) <- c("Outcome", paste0("Var", 1:3))
    expected.warning <- paste0(sQuote("data"), " argument contains character variables",
                               " (", paste0(dQuote(names(test)), collapse = ", "), "). ",
                               "These have been changed to categorical factor variables for analysis.")
    expect_warning(LDA(Outcome ~ Var1 + Var2 + Var3, data = test), expected.warning, fixed = TRUE)
})

test_that("DS-4360 LDA predict working with newdata argument", {
    smaller.hair <- subset(hair, select = c("x1", "x6", "x5", "x19", "x4"))
    expect_equal(vapply(smaller.hair, class, character(1L)),
                 c(x1 = "factor", x6 = "numeric", x5 = "factor", x19 = "numeric", x4 = "factor"))
    expect_silent(hairlda <- LDA(x1 ~ x6 + x5 + x19 + x4, data = smaller.hair))
    # Expect error if wrong input for newdata
    for (newdat.arg in list(1, "foo", list(1, 2, 3)))
        expect_error(predict(hairlda, newdata = newdat.arg),
                     "newdata must be a data.frame", fixed = TRUE)
    # basic tests
    basic.df <- data.frame(x6  = 5:10,
                           x5  = smaller.hair[["x5"]][1:2],
                           x19 = 4:9,
                           x4  = smaller.hair[["x4"]][1:2])
    outcome.levels <- levels(smaller.hair[["x1"]])
    expected.predictions <- factor(c(1, 1, 1, 3, 2, 3), labels = levels(smaller.hair[["x1"]]))
    expect_equal(predict(hairlda, newdata = basic.df), expected.predictions)
    # Unused variables should be ignored
    test.df <- basic.df
    test.df[["foo"]] <- 1:6
    expect_equal(predict(hairlda, newdata = test.df), expected.predictions)
    # Unused factors also ignored
    test.df <- basic.df
    test.df[["foo"]] <- factor(1:6)
    expect_equal(predict(hairlda, newdata = test.df), expected.predictions)
    # Character variables should be converted to factors
    test.df <- basic.df
    test.df[vapply(test.df, is.factor, logical(1L))] <- lapply(test.df[vapply(test.df, is.factor, logical(1L))],
                                                               as.character) |> as.data.frame()
    expect_equal(predict(hairlda, newdata = test.df), expected.predictions)
    # Incorrect leveling handled
    test.df <- basic.df
    test.df[["x4"]] <- relevel(basic.df[["x4"]], ref = levels(basic.df[["x4"]])[2])
    test.df[["x5"]] <- relevel(basic.df[["x5"]], ref = levels(basic.df[["x5"]])[2])
    expect_equal(predict(hairlda, newdata = test.df), expected.predictions)
    # Character representation handled
    test.df <- basic.df
    test.df[["x4"]] <- as.character(test.df[["x4"]])
    test.df[["x5"]] <- as.character(test.df[["x5"]])
    expect_equal(predict(hairlda, newdata = test.df), expected.predictions)
    # integer representation recoverable
    numeric.df <- smaller.hair |> lapply(as.numeric) |> as.data.frame()
    numeric.df[["x4"]] <- as.factor(numeric.df[["x4"]])
    numeric.df[["x5"]] <- as.factor(numeric.df[["x5"]])
    numeric.df[["x1"]] <- smaller.hair[["x1"]]
    expect_silent(hairlda.int <- LDA(x1 ~ x6 + x5 + x19 + x4, data = numeric.df))
    test.df <- basic.df
    test.df[["x4"]] <- as.integer(test.df[["x4"]])
    test.df[["x5"]] <- as.integer(test.df[["x5"]])
    expect_equal(predict(hairlda.int, newdata = test.df), expected.predictions)
})
