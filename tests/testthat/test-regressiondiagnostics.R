context("Regression Diagnostics")
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
data(bank)
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
wgt[is.na(wgt)] = 0
attr(wgt, "label") <- "ID"



for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c( "Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD", "Multinomial Logit"))
        test_that(paste("Accuracy and R-square:",missing, type),
        {
            z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, type = type)
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
            z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  type = type)
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
            z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = wgt, missing = missing, data = bank, type = type)
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
            z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = wgt, missing = missing, data = bank, subset = sb,  type = type)
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
        })


test_that("Tests of homogenous variance (Breush-Pagen test)",
{
    # Unfilitered
    z = BreuschPagan(Regression(zformula, data = bank))
    z1 = car::ncvTest(lm(zformula, data = bank))
    expect_equal(z$p, z1$p, tolerance = 1.0e-8)

    # Filitered
    z = BreuschPagan(Regression(zformula, data = bank, subset = sb))
    z1 = car::ncvTest(lm(zformula, data = bank, subset = sb))
    expect_equal(z$p, z1$p, tolerance = 1.0e-8)

    z = BreuschPagan(Regression(zformula, data = bank,  weights = wgt))
    z1 = car::ncvTest(lm(zformula, data = bank, wgt))
    expect_false(round(z$p - z1$p,5) == 0)

    # Weighted and filtered
    z = BreuschPagan(Regression(zformula, data = bank, subset = sb,  weights = wgt))
    z1 = car::ncvTest(lm(zformula, data = bank, subset = sb,  wgt))
    expect_false(round(z$p - z1$p,5) == 0)

    # Weighted and filtered with various missing value settings
    expect_that(Regression(zformula, missing = "Exclude cases with missing data", data = bank, subset = sb,  weights = wgt), not(throws_error()))
    expect_that(Regression(zformula, missing = "Error if missing data", data = bank, subset = sb,  weights = wgt), throws_error())
    z <- bank[complete.cases(bank),]
    expect_that(Regression(zformula, missing = "Error if missing data", data = z, subset = z$ID > 100,  weights = z$ID), not(throws_error()))
    expect_that(Regression(zformula, missing = "Imputation (replace missing values with estimates)", data = bank, subset = sb,  weights = wgt), not(throws_error()))
    expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = "Use partial data (pairwise correlations)", data = bank, subset = sb,  weights = wgt), not(throws_error()))

    data(bank)
     # Unweighted
    zRegression <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
    zWLS <- lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
    expect_equal(BreuschPagan(zRegression)$p, BreuschPagan(zWLS)$p)
    # Filtered
    zRegression <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100)
    zWLS <- lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100)
    expect_equal(BreuschPagan(zRegression)$p, BreuschPagan(zWLS)$p)
    # Weighted.
    zRegression <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, weights = bank$ID)
    zWLS <- lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, weights = bank$ID)
    expect_equal(round(BreuschPagan(zRegression)$p, 1),round(BreuschPagan(zWLS)$p,1))

})

test_that("VIF",
           {
     library(car)
     # Unweighted - linear
     zRegression <- vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))
     zR <- vif(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))
     expect_equal(zRegression, zR)
     # Filtered - linear
     zRegression <- vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100))
     zR <- vif(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100))
     expect_equal(zRegression, zR)
     # Weighted.
     expect_that(vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, weights = bank$ID)), not(throws_error()))
     # Logit (used as a proxy for all the glms)
     type = "Binary Logit"
     zRegression <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type)
     zR <- glm(Overall >= 4 ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank,  family = binomial(link = "logit"))
     expect_equal(vif(zRegression), vif(zR))
     # Logit - filtered
     zRegression <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = bank$ID > 100)
     zR <- glm(Overall >= 4 ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank,  family = binomial(link = "logit"), subset = bank$ID > 100)
     expect_equal(vif(zRegression), vif(zR))
     # Logit - filtered and weighted
     z = wgt > 0 & complete.cases(bank[,c("Overall","Fees","Interest","Phone","Branch","Online","ATM")])
     zBank = bank[z, ]
     zBank$dd = zBank$Overall >= 4
     zwgt = wgt[z]
     zRegression <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = zBank, weights = zwgt, type = type)
     zR <- survey::svyglm(dd ~ Fees + Interest + Phone + Branch + Online + ATM, data = zBank, design = flipMultivariates:::weightedSurveyDesign(zBank, zwgt), family = quasibinomial())
     expect_equal(vif(zRegression), vif(zR))
     # Checking for errors in other types of models
     for (type in c("Poisson", "Quasi-Poisson", "Ordered Logit", "NBD", "Multinomial Logit"))
        expect_that(vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = bank$ID > 100, weights = bank$ID)), not(throws_error()))
 })





