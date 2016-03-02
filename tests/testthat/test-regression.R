context("Regression")
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
data(bank)
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"

test_that("allEffects works on Regression object",
{
    data(cpus, package = "MASS")
    z <- Regression(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data = cpus)
    zlm <- lm(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data = cpus)
    expect_equal(effects::allEffects(z), effects::allEffects(zlm))
})


for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c("Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered", "NBD"))
        test_that(paste("Stops gracefully with small sample size", missing, type),
{
     expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = wgt > 30000, type = type), throws_error())
     expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = wgt > 30000,  weights = wgt, type = type), throws_error())
})




test_that("Tests of homogenous variance (Breusch-Pagen test)",
{
    # Unfiltered
    z = BreuschPagan(Regression(zformula, data = bank))
    z1 = car::ncvTest(lm(zformula, data = bank))
    expect_equal(z$p, z1$p, tolerance = 1.0e-8)

    # Filtered
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
    for (type in c("Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered", "NBD"))
        test_that(paste("No error", missing, type),
{
     # no weight, no filter
     expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = NULL, type = type), not(throws_error()))
     # weight
     expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = NULL, type = type), not(throws_error()))
     # weight, filter
     expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = wgt, type = type), not(throws_error()))
     # weight, filter
     expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = wgt, type = type), not(throws_error()))
})


test_that("Printing of Regression objects works with direct and text formulae",
{
    z.text <- suppressWarnings(Regression(zformula, data = bank))
    z.direct <- suppressWarnings(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))

    expect_that(suppressWarnings(capture.output(print(z.text))), not(throws_error()))
    expect_equal(formula(z.text), formula(z.direct))
})



# Nice summary printing function
for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c("Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered", "NBD"))
        test_that(paste("No error in nice print function", missing, type),
                  {
                      z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = NULL, type = type, r.output = FALSE)
                      expect_that(print(z), not(throws_error()))
                  })




# Robust SE


