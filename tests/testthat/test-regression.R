context("Regression")
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
data(bank)
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
attr(wgt, "label") <- "ID"

test_that(paste("Alternative ways of passing data in"),
{
      type = "Linear"
      # no weight, no filter
      z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = NULL, type = type)
      attach(bank)
      z1 = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = NULL, type = type)
      expect_true(all.equal(z$coefficients, z1$coefficients))
      detach(bank)
      # filter and weight a part of the data frame.
      zbank <- cbind(bank, w = wgt, f = sb)
      z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = zbank, subset = f, weights = w, type = type)
      attach(zbank)
      z1 = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = w, subset = f, type = type)
      detach(zbank)
      expect_true(all.equal(z$coefficients, z1$coefficients))
      # filter and weight a part of the data frame and are formulas.
      zbank <- cbind(bank, w = wgt, f = sb)
      z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = zbank, subset = f == TRUE, weights = w, type = type)
      attach(zbank)
      z1 = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = w, subset = f == TRUE, type = type)
      detach(zbank)
      expect_true(all.equal(z$coefficients, z1$coefficients))
      # filter and weight are not part of the data frame.
      z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, weights = wgt, type = type)
      attach(zbank)
      z1 = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, subset = sb, weights = wgt, type = type)
      detach(zbank)
      expect_true(all.equal(z$coefficients, z1$coefficients))

})

test_that(paste("Robust se does something"),
{
      type = "Linear"
      # no weight, no filter
      z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = NULL, type = type)
      zs = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, robust.se = TRUE, subset = TRUE,  weights = NULL, type = type)
      expect_false(isTRUE(all.equal(z$summary$coefficients[,2], zs$summary$coefficients[,2])))
      z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, detail = FALSE, subset = TRUE,  weights = NULL, type = type)
      zs = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, detail = FALSE, robust.se = TRUE, subset = TRUE,  weights = NULL, type = type)
      expect_false(isTRUE(all.equal(z$summary$coefficients[,2], zs$summary$coefficients[,2])))
      expect_that(capture.output(print(z)), not(throws_error()))
      expect_that(capture.output(print(zs)), not(throws_error()))

      type = "Poisson"
      # no weight, no filter
      z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = NULL, type = type)
      zs = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, robust.se = TRUE, subset = TRUE,  weights = NULL, type = type)
      expect_false(isTRUE(all.equal(z$summary$coefficients[,2], zs$summary$coefficients[,2])))
      z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, detail = FALSE, subset = TRUE,  weights = NULL, type = type)
      zs = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, detail = FALSE, robust.se = TRUE, subset = TRUE,  weights = NULL, type = type)
      expect_false(isTRUE(all.equal(z$summary$coefficients[,2], zs$summary$coefficients[,2])))
      expect_that(capture.output(print(z)), not(throws_error()))
      expect_that(capture.output(print(zs)), not(throws_error()))
})


for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c("Multinomial Logit", "Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD"))
        test_that(paste("Residuals", missing, type),
      {
          # no weight, no filter
          z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = NULL, type = type)
          # weight
          expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = NULL, type = type), not(throws_error()))
          # weight, filter
          expect_that(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = TRUE,  weights = wgt, type = type), not(throws_error()))
          # weight, filter
          expect_that(z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  weights = wgt, type = type), not(throws_error()))
          expect_that(capture.output(print(z)), not(throws_error()))
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
    z <- as.numeric(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.27732,4))
    z <- as.numeric(Regression(zformula, data = bank, subset = sb,  missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.25451,4))
    z <- as.numeric(Regression(zformula, data = bank, weights = wgt, missing = missing)$coef[3])
    expect_equal(round(z,4), round(0.2611546, 4))
    z <- as.numeric(Regression(zformula, data = bank, weights = wgt, subset = sb, missing = missing)$coef[3])
    expect_equal(round(z,4),round(0.2539403,4))
})

z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)

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
    z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)
    z <- as.numeric(z$coef[3])
    expect_equal(round(z,4), round(0.2923788,4))
    z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = sb, missing = missing)
    z <- as.numeric(z$coef[3])
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


for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c("Multinomial Logit", "Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD"))
        test_that(paste(type, " save variables"),{
            z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, missing = missing, weights = wgt / 100, subset = sb)
            expect_equal(length(predict(z)), 896)
          })


for (type in c("Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD"))
    test_that(paste(type, "does not have an error when producing non-detailed outputs"),{
            z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, missing = missing, weights = wgt / 100, subset = sb, detail = FALSE)
            expect_that(capture.output(print(z)), not(throws_error()))
          })
