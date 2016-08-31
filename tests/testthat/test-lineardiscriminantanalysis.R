library(foreign)
context("LDA")
dat.original = read.spss("http://www.ats.ucla.edu/stat/spss/output/discrim.sav", to.data.frame = TRUE)
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



for (method in c("mle", "moment"))
    for (result in c("svd", "means", "scaling"))
    test_that(paste("Unweighted LDA", method, result),
    {
        z = MASS::lda(dat1[,1:3], dat1$job , prior = rep(1/3,3), method = method)
        z1 = LDA.fit(dat1[,1:3], dat1$job, prior = rep(1/3,3), method = method)
        expect_equal(z[[result]], z1[[result]])
        z2 = LDA(job ~ outdoor + social + conservative, data = dat1, prior = "Equal", variance = method)$original
        z3 = MASS:::lda(job ~ outdoor + social + conservative, data = dat1, prior = rep(1/3,3), method = method)
        expect_equal(z2[[result]], z1[[result]])
        expect_equal(z2[[result]], z3[[result]])
    })

for (method in c("mle", "moment"))
    for (result in c("svd", "means", "scaling"))
    test_that(paste("Weighted LDA", method, result),
    {
        z = MASS::lda(dat1[,1:3], dat1$job , prior = rep(1/3,3), method = method)
        z1 =  LDA.fit(dat[,1:3], dat$job, weights = wgt1,  prior = rep(1/3,3), method = method)
        expect_equal(z[[result]], z1[[result]])
        z2 = LDA(job ~ outdoor + social + conservative, data = dat, prior = rep(1/3,3), weights = wgt1,  variance = method)$original
        expect_equal(z2[[result]], z1[[result]])
    })



for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
        test_that(paste("Filtering and weighting work: ", missing),
      {
          # no weight, no filter
          expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank)), NA)
          # weight
          expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = NULL)), NA)
          # weight, filter
          expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = wgt)), NA)
          # weight, filter
          expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = wgt)), NA)
      })


for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
        test_that(paste("Filtering and weighting work: ", missing),
      {
          # no weight, no filter
          expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank)), NA)
          # weight
          expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = NULL)), NA)
          # weight, filter
          expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = wgt)), NA)
          # weight, filter
          expect_error(suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = wgt)), NA)
      })



test_that(paste("Predict works"),
      {
          # no weight, no filter
          expect_error(z <- suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)), NA)
          expect_error(flipRegression::Accuracy(z), NA)
          expect_error(flipRegression::ConfusionMatrix(z), NA)
          # weight
          expect_error(z <- suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb,  weights = NULL)), NA)
          expect_error(flipRegression::Accuracy(z, weights = wgt), NA)
          expect_error(flipRegression::ConfusionMatrix(z, weights = wgt), NA)
          # filter
          expect_error(z <- suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt)), NA)
          expect_error(flipRegression::Accuracy(z, sb), NA)
          expect_error(flipRegression::ConfusionMatrix(z, sb), NA)
          # weight, filter
          expect_error(z <- suppressWarnings(LDA(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb,  weights = wgt)), NA)
          expect_error(flipRegression::Accuracy(z, sb, wgt), NA)
          expect_error(flipRegression::ConfusionMatrix(z, sb, wgt), NA)
      })

