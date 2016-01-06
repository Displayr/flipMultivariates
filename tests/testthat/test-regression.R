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
    expect_that(Regression(zformula, missing = "Use partial data (pairwise)", data = bank, subset = bank$ID > 100,  weights = bank$ID), not(throws_error()))
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
    expect_equal(round(z,4), round(0.2802536,4))
    z <- as.numeric(Regression(zformula, data = bank, subset = bank$ID > 100, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.265103,4))
    z <- as.numeric(Regression(zformula, data = bank, weights = bank$ID, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.2702182,4))
    z <- as.numeric(Regression(zformula, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing)$coef[3])
    expect_equal(round(z,4),round(0.2636018,4))
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
