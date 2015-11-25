context("Linear regression")

test_that("allEffects works on Regression object",
{
    data(cpus, package = "MASS")
    z <- LinearRegression(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data = cpus)
    zlm <- lm(log10(perf) ~ syct+mmin+mmax+cach+chmin+chmax, data = cpus)
    expect_equal(effects::allEffects(z), effects::allEffects(zlm))
})
