context("subset label")
test_that("Subset info is good", {
    data(colas, package = "flipExampleData")
    expected.warn <- r"(The smallest category of the outcome variable \(\d{2} or more\) contains \d{2} observations; a robust model is unlikely)"
    expect_warning(LDA(d1 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, data = colas, prior = "Observed"),
                   expected.warn)
    expect_warning(LDA(d1 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, subset = colas$Q5_16_1 == "Yes", data = colas, prior = "Observed"),
                   expected.warn)
    z <- colas$Q5_16_1 == "Yes"
    expect_warning(LDA(d1 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, subset = z, data = colas, prior = "Observed"),
                   expected.warn)
    attr(z, "label") <- "A nice, long, subset description"
    expect_warning(LDA(d1 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, subset = z, data = colas, prior = "Observed"),
                   expected.warn)
    zw <- as.numeric(unclass(colas$q8)) / 10
    expect_warning(LDA(d1 ~ Q5_5_1 + Q5_7_1 + Q5_13_1, subset = z, weights = zw, data = colas, prior = "Observed"),
                   expected.warn)
})
