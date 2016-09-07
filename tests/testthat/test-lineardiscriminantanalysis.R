context("LDA")

data(hbatwithsplits, package = "flipExampleData")
hair <- hbatwithsplits

hair1  <- flipTransformations::AsNumeric(hair[, paste0("x",6:18)], binary = FALSE, remove.first = TRUE)
hair1$x1 <- hair$x1
hair1$split60 <- hair$split60
hair1$id <- hair$id

test_that(paste("Replicating SPSS and hair - confusion"),
          {

              # no weight, filtered
              z <- LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data = hair1, subset = split60 == "Estimation Sample", show.labels = TRUE)
              z
              expect_equal(as.numeric(z$confusion), c(21,0,0,1,11,0,0,2,25))
              # noweight, filtered
              z <- LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data = hair1, weights = hair1$id,subset = split60 == "Estimation Sample", show.labels = TRUE)
              z
              # # Factor
              # hair2 <- hair1
              # hair2$x6 <- flipTransformations::Factor(hair1$x6)
              # LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data = hair2, weights = hair1$id,subset = split60 == "Estimation Sample", show.labels = TRUE)
              # LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data = hair2, weights = hair1$id,subset = split60 == "Estimation Sample", show.labels = TRUE, binary = TRUE)


          })

# In SPSS, the priors are always the oberved priors when fitting the model. In MASS:lda, the priors are used when fitting.
test_that("Replicating SPSS defaults using MASS:LDA",
          {
              #### Reproducing SPSS default results.
              # Mass.
              library(MASS)
              zlda = lda(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, data = hair1, subset = split60 == "Estimation Sample", method = "moment")#, prior = rep(1/3,3))#, method = "mle")   #, "moment")[1])
              variance.explained <- round(zlda$svd^2/sum(zlda$svd^2), 4L)
              expect_equal(0.86959956, variance.explained[1], tolerance = 0.001)
              zlda.discriminant.variables <- MASS:::predict.lda(zlda, newdata = hair1[,1:13], na.action = na.pass)[["x"]]
              head(zlda.discriminant.variables)
              expect_equal(.45479754052345, abs(zlda.discriminant.variables[2,2]), tolerance = 0.001)
              zlda.probs <- MASS:::predict.lda(zlda, newdata = hair1[,1:13], prior = rep(1/3, 3), na.action = na.pass)[["posterior"]]
              expect_equal(.83131933755411, zlda.probs[5,3], tolerance = 0.001)
          })

test_that("LDA Replicating SPSS defaults - unweighted",
          {
              zLDA <- LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, method = "moment", data = hair1, subset = split60 == "Estimation Sample", show.labels = TRUE)
              variance.explained <- round(zLDA$original$svd^2/sum(zLDA$original$svd^2), 4L)
              expect_equal(0.86959956, variance.explained[1], tolerance = 0.001)
              zLDA.discriminant.variables <- DiscriminantVariables(zLDA)
              expect_equal(.45479754052345, as.numeric(abs(zLDA.discriminant.variables[2,2])), tolerance = 0.001)
              zLDA.probs <- flipData::Probabilities(zLDA)
              expect_equal(.83131933755411, as.numeric(zLDA.probs[5,3]), tolerance = 0.001)
              expect_equal(as.numeric(zLDA$confusion), c(21,0,0,1,11,0,0,2,25))
          }
)

test_that("LDA Replicating SPSS defaults - weighted",
          {
              zLDA <- LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, method = "moment", data = hair1, weights = hair1$id, subset = split60 == "Estimation Sample", show.labels = TRUE)
              variance.explained <- round(zLDA$original$svd^2/sum(zLDA$original$svd^2), 4L)
              expect_equal(0.787, variance.explained[1], tolerance = 0.001)
              zLDA.discriminant.variables <- DiscriminantVariables(zLDA)
              head(zLDA.discriminant.variables)
              expect_equal(0.07987, as.numeric(abs(zLDA.discriminant.variables[2,2])), tolerance = 0.001)
              zLDA.probs <- flipData::Probabilities(zLDA)
              head(zLDA.probs)
              expect_equal(0.92558, as.numeric(zLDA.probs[5,3]), tolerance = 0.001)
              expect_equal(as.numeric(zLDA$confusion), c(1023,0,0,43,501,0,0,43,1107))
          }
)


test_that("LDA Replicating SPSS - compute prior from group sizes - unweighted",
          {
              zLDA <- LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, method = "moment", data = hair1, prior = "Observed", subset = split60 == "Estimation Sample", show.labels = TRUE)
              variance.explained <- round(zLDA$original$svd^2/sum(zLDA$original$svd^2), 4L)
              expect_equal(0.87, variance.explained[1], tolerance = 0.001)
              zLDA.discriminant.variables <- DiscriminantVariables(zLDA)
              head(zLDA.discriminant.variables)
              expect_equal(0.45480, abs(as.numeric(abs(zLDA.discriminant.variables[2,2]))), tolerance = 0.001)
              zLDA.probs <- flipData::Probabilities(zLDA)
              head(zLDA.probs)
              expect_equal(0.89292, as.numeric(zLDA.probs[5,3]), tolerance = 0.001)
              expect_equal(as.numeric(zLDA$confusion), c(1023,0,0,43,501,0,0,43,1107))
          }
)

test_that("LDA Replicating SPSS - compute prior from group sizes - weighted",
          {
              zLDA <- LDA(x1 ~ x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18, method = "moment", data = hair1, prior = "Observed", weights = hair1$id, subset = split60 == "Estimation Sample", show.labels = TRUE)
              variance.explained <- round(zLDA$original$svd^2/sum(zLDA$original$svd^2), 4L)
              expect_equal(0.787, variance.explained[1], tolerance = 0.001)
              zLDA.discriminant.variables <- DiscriminantVariables(zLDA)
              head(zLDA.discriminant.variables)
              expect_equal(0.07987, abs(as.numeric(abs(zLDA.discriminant.variables[2,2]))), tolerance = 0.001)
              zLDA.probs <- flipData::Probabilities(zLDA)
              head(zLDA.probs)
              expect_equal(0.92946, as.numeric(zLDA.probs[5,3]), tolerance = 0.001)
              expect_equal(as.numeric(zLDA$confusion), c(1023,0,0,43,501,0,0,43,1107))
          }
)






dat.original = foreign::read.spss("http://www.ats.ucla.edu/stat/spss/output/discrim.sav", to.data.frame = TRUE)
dat <- dat.original[sample(1:nrow(dat.original),91),]
wgt1 <- c(10,rep(1,90))
dat1 <- dat[c(rep(1,10),2:91),]
data(bank, package = "flipExampleData")
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 500"
wgt <- bank$ID / 100
attr(wgt, "label") <- "ID"
bank$dep <- (unclass(bank$Overall) - 1) / 6
attr(bank$Fees, "label") = "Bank Fees"
attr(bank$Overall, "label") = "Overall satisfaction"

LDA(Overall ~ Interest + Fees + Phone + Branch + Online + ATM, output = "Means", data = bank, subset = sb, weights = wgt, show.labels = TRUE)
LDA(Overall ~ Interest + Fees + Phone + Branch + Online + ATM, output = "Confusion Matrix", data = bank, subset = sb, weights = wgt, show.labels = TRUE)
LDA(Overall ~ Interest + Fees + Phone + Branch + Online + ATM, output = "Detail", data = bank, subset = sb, weights = wgt, show.labels = TRUE)
LDA(job ~ outdoor + social + conservative, output = "Means", data = dat, prior = rep(1/3,3), weights = wgt1)
LDA(job ~ outdoor + social + conservative, output = "Confusion Matrix", data = dat, prior = rep(1/3,3), weights = wgt1)
LDA(job ~ outdoor + social + conservative, output = "Detail", data = dat, prior = rep(1/3,3), weights = wgt1)
#
#
#
# for (method in c("mle", "moment"))
#     for (result in c("svd", "means", "scaling"))
#     test_that(paste("Unweighted LDA", method, result),
#     {
#         z = MASS::lda(dat1[,1:3], dat1$job , prior = rep(1/3,3), method = method)
#         z1 = LDA.fit(dat1[,1:3], dat1$job, prior = rep(1/3,3), method = method)
#         expect_equal(z[[result]], z1[[result]])
#
#         z2 = LDA(job ~ outdoor + social + conservative, data = dat1, variance = method)$original
#         expect_equal(z2[[result]], z1[[result]])
#         expect_equal(z2[[result]], z3[[result]])
#     })
#
# for (method in c("mle", "moment"))
#     for (result in c("svd", "means", "scaling"))
#     test_that(paste("Weighted LDA", method, result),
#     {
#         z = MASS::lda(dat1[,1:3], dat1$job , prior = rep(1/3,3), method = method)
#         z1 =  LDA.fit(dat[,1:3], dat$job, weights = wgt1,  prior = rep(1/3,3), method = method)
#         expect_equal(z[[result]], z1[[result]])
#         z2 = LDA(job ~ outdoor + social + conservative, data = dat, prior = rep(1/3,3), weights = wgt1,  variance = method)$original
#         expect_equal(z2[[result]], z1[[result]])
#     })
#
#
#
# for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
#         test_that(paste("Filtering and weighting work: ", missing),
#       {
#           # no weight, no filter
#           expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank)), NA)
#           # weight
#           expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = NULL)), NA)
#           # weight, filter
#           expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = wgt)), NA)
#           # weight, filter
#           expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = wgt)), NA)
#       })
#
#
# for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
#         test_that(paste("Filtering and weighting work: ", missing),
#       {
#           # no weight, no filter
#           expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank)), NA)
#           # weight
#           expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = NULL)), NA)
#           # weight, filter
#           expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = wgt)), NA)
#           # weight, filter
#           expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = wgt)), NA)
#       })
#
#
#
# test_that(paste("Predict works"),
#       {
#           # no weight, no filter
#           expect_error(z <- suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)), NA)
#           expect_error(flipRegression::Accuracy(z), NA)
#           expect_error(flipRegression::ConfusionMatrix(z), NA)
#           # weight
#           expect_error(z <- suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb,  weights = NULL)), NA)
#           expect_error(flipRegression::Accuracy(z, weights = wgt), NA)
#           expect_error(flipRegression::ConfusionMatrix(z, weights = wgt), NA)
#           # filter
#           expect_error(z <- suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt)), NA)
#           expect_error(flipRegression::Accuracy(z, sb), NA)
#           expect_error(flipRegression::ConfusionMatrix(z, sb), NA)
#           # weight, filter
#           expect_error(z <- suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb,  weights = wgt)), NA)
#           expect_error(flipRegression::Accuracy(z, sb, wgt), NA)
#           expect_error(flipRegression::ConfusionMatrix(z, sb, wgt), NA)
#       })
#
