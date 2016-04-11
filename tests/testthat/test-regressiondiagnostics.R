context("Regression Diagnostics")
context("Regression")
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
data(bank)
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"

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
    expect_equal(round(BreuschPagan(zRegression)$p),round(BreuschPagan(zWLS)$p,1))

})

data(bank)

test_that("VIF",
           {
     library(car)
     # Unweighted
     zRegression <- vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))
     zR <- vif(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))
     expect_equal(zRegression, zR)
     # Filtered
     zRegression <- vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100))
     zR <- vif(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100))
     expect_equal(zRegression, zR)
     # Weighted.
     zRegression <- vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, weights = bank$ID))
     zR <- vif(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, weights = bank$ID))
     expect_equal(zRegression, zR)
     # Logit
     type = "Binary Logit"
     zRegression <- vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type))
     zR <- vif(glm(Overall >= 4 ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank,  family = "binomial"))
     expect_equal(zRegression, zR)
     # Logit, filtered
     type = "Binary Logit"
     zRegression <- vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = bank$ID > 100))
     zR <- vif(glm(Overall >= 4 ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank,  family = "binomial", subset = bank$ID > 100))
     expect_equal(zRegression, zR)
     # Logit, weighted, filtered
     type = "Binary Logit"
     zRegression <- vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = bank$ID > 100, weights = bank$ID))
     zR <- vif(glm(Overall >= 4 ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank,  family = "binomial", subset = bank$ID > 100, weights = bank$ID))
     expect_equal(zRegression, zR)
 })




