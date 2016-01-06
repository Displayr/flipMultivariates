context("Linear regression")
data(bank)

test_that("allEffects works on Regression object",
{
    data(cpus, package = "MASS")
    z <- Regression(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data = cpus)
    zlm <- lm(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data = cpus)
    expect_equal(effects::allEffects(z), effects::allEffects(zlm))
})


zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
data(bank)

test_that("Tests of homogenous variance (Breush-Pagen test)",
{
    # Unfilitered
    z = BreuschPagan(Regression(zformula, data = bank))
    z1 = car::ncvTest(lm(zformula, data = bank))
    expect_equal(z$p, z1$p, tolerance = 1.0e-8)

    # Filitered
    z = BreuschPagan(Regression(zformula, data = bank, subset = bank$ID > 100))
    z1 = car::ncvTest(lm(zformula, data = bank, subset = ID > 100))
    expect_equal(z$p, z1$p, tolerance = 1.0e-8)

    z = BreuschPagan(Regression(zformula, data = bank,  weights = bank$ID))
    z1 = car::ncvTest(lm(zformula, data = bank, weights = ID))
    expect_false(round(z$p - z1$p,5) == 0)

    # Weighted and filtered
    z = BreuschPagan(Regression(zformula, data = bank, subset = bank$ID > 100,  weights = bank$ID))
    z1 = car::ncvTest(lm(zformula, data = bank, subset = ID > 100,  weights = ID))
    expect_false(round(z$p - z1$p,5) == 0)

    # Weighted and filtered with various missing value settings
    expect_that(Regression(zformula, missing = "Exclude cases with missing data", data = bank, subset = bank$ID > 100,  weights = bank$ID), not(throws_error()))
    expect_that(Regression(zformula, missing = "Error if missing data", data = bank, subset = bank$ID > 100,  weights = bank$ID), throws_error())
    z <- bank[complete.cases(bank),]
    expect_that(Regression(zformula, missing = "Error if missing data", data = z, subset = bank$ID > 100,  weights = bank$ID), not(throws_error()))
    expect_that(Regression(zformula, missing = "Imputation", data = bank, subset = bank$ID > 100,  weights = bank$ID), not(throws_error()))
    expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = "Use partial data (pairwise)", data = bank, subset = bank$ID > 100,  weights = bank$ID), not(throws_error()))
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
    z <- as.numeric(Regression(zformula, data = bank, subset = bank$ID > 100, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.25451,4))
    z <- as.numeric(Regression(zformula, data = bank, weights = bank$ID, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.2611546, 4))
    z <- as.numeric(Regression(zformula, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing)$coef[3])
    expect_equal(round(z,4),round(0.2539403,4))
})


missing <- "Imputation"
test_that(missing,
{
    z <- as.numeric(Regression(zformula, data = bank, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.3115914,4))
    z <- as.numeric(Regression(zformula, data = bank, subset = bank$ID > 100, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.3220946,4))
    z <- as.numeric(Regression(zformula, data = bank, weights = bank$ID, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.3345016,4))
    z <- as.numeric(Regression(zformula, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing)$coef[3])
    expect_equal(round(z,4),round(0.3358332,4))
})

missing <- "Use partial data (pairwise)"
test_that(missing,
{
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.2773201,4))
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.2545135,4))
})

missing <- "Use partial data (pairwise)"
test_that(paste(missing, " with integer weights"),
{
    wgt <- ceiling(bank$ID/100)
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = wgt, weights = wgt, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.2611546,4))
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = wgt, subset = bank$ID > 300, missing = missing)$coef[3])
    expect_equal(round(z,4),round(0.2539403,4))
})


missing <- "Use partial data (pairwise)"
test_that(paste(missing, " with numeric weights"),
{
    wgt <- bank$ID/100
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID / 100, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.2611546,4))
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID / 100, subset = bank$ID > 100, missing = missing)$coef[3])
    expect_equal(round(z,4),round(0.2539403,4))
})
