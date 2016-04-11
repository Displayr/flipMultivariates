context("Regression")
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
data(bank)
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"


for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c( "Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD"))
        test_that(paste("Residuals", missing, type),
      {
          # no weight, no filter
          z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = NULL, type = type)
          # weight
          expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = NULL, type = type), not(throws_error()))
          # weight, filter
          expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = wgt, type = type), not(throws_error()))
          # weight, filter
          expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = wgt, type = type), not(throws_error()))
      })


test_that("allEffects works on Regression object",
{
    data(cpus, package = "MASS")
    z <- Regression(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data = cpus)
    zlm <- lm(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data = cpus)
    expect_equal(effects::allEffects(z), effects::allEffects(zlm))
})

for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c("Multinomial Logit","Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD"))
        test_that(paste("Stops gracefully with small sample size", missing, type),
{
     expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = wgt > 30000, type = type), throws_error())
     expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = wgt > 30000,  weights = wgt, type = type), throws_error())
})


test_that("Error due to missing data",
{
    missing = "Error if missing data"
    expect_error(Regression(zFormula, data = bank, missing = missing), tolerance = 1.0e-8)
})

missing <- "Exclude cases with missing data"
test_that(missing,
{
    z <- as.numeric(Regression(zformula, data = bank, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.27732,4))
    z <- as.numeric(Regression(zformula, data = bank, subset = sb,  missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.25451,4))
    z <- as.numeric(Regression(zformula, data = bank, weights = wgt, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.2611546, 4))
    z <- as.numeric(Regression(zformula, data = bank, weights = wgt, subset = sb, missing = missing)$coef[3])
    expect_equal(round(z,4),round(0.2539403,4))
})

missing <- "Imputation (replace missing values with estimates)"
test_that(missing,
{
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)$coef[3])
    expect_equal(round(z, 3), 0.312)
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)$coef[3])
    expect_equal(round(z, 3), 0.299)
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing)$coef[3])
    expect_equal(round(z, 3), 0.303)
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = sb, missing = missing)$coef[3])
    expect_equal(round(z, 3), 0.312)
})

missing <- "Use partial data (pairwise correlations)"
test_that(missing,
{
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.2923788,4))
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.2991385,4))
})

missing <- "Use partial data (pairwise correlations)"
test_that(paste(missing, " with integer weights"),
{
    wgt <- ceiling(bank$ID/10)
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, missing = missing)$coef[3])
    expect_equal(round(z,3), round(0.303, 3))
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = wgt > 1, missing = missing)$coef[3])
    expect_equal(round(z,3),round(0.303,4))
})


missing <- "Use partial data (pairwise correlations)"
test_that(paste(missing, " with numeric weights"),
{
    wgt <- wgt/100
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt / 100, missing = missing)$coef[3])
    expect_equal(round(z,3), round(0.661,3))
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt / 100, subset = sb, missing = missing)$coef[3])
    expect_equal(round(z,3),round(0.132,3))
})

for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c("Multinomial Logit", "Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD"))
        for (detail in c(FALSE, TRUE))
            test_that(paste("No error", missing, type, "detail =", detail),
{
     # no weight, no filter
     expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE, detail = detail, weights = NULL, type = type), not(throws_error()))
     # weight
     expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb, detail = detail, weights = NULL, type = type), not(throws_error()))
     # weight, filter
     expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE, detail = detail, weights = wgt, type = type), not(throws_error()))
     # weight, filter
     expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = wgt, detail = detail, type = type), not(throws_error()))
})


# missing = "Imputation (replace missing values with estimates)"
# type = "Multinomial Logit"
# detail = FALSE
# Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE, detail = detail, weights = NULL, type = type)
#

