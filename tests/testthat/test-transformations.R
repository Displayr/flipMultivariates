context("Linear regression")
data(bank)

test_that("Dichotomizing works",
{
    expect_true(all(table(DichotomizeFactor(factor(LETTERS[1:10]))) == c(5,5)))
    expect_true(all(table(DichotomizeFactor(bank$Overall)) == c(265, 497)))

})
