context("missingdata")

dat <- data.frame(a = rep((1:10)/10,2),
                  b = rep(1:10,2),
                  c = factor(rep(c(rep("A",5),rep("B",5)),2)),
                  d = ordered(rep(c(rep("A",5),rep("B",5)),2)), e = rep("dog",20), stringsAsFactors = FALSE)
for (i in 1:5)
    dat[i, i] <- NA

test_that("AnyNA",
{
    expect_that(AnyNA(dat), is_true())
    expect_that(AnyNA(dat[11:20,]), is_false())
    expect_that(AnyNA(dat[3:20, ], c ~ a + b), is_true())
    expect_that(AnyNA(dat[3:20, ], ~ a + b + c + d), is_true())
    expect_that(AnyNA(dat[3:20, ], a ~ b), is_false())
    expect_that(AnyNA(dat[3:20, ], ~ a + b), is_false())
    expect_that(AnyNA(dat[3:20, ], ~ a + b), is_false())
})


test_that("Missing data algorithms",
{
    # Default
    si <- SingleImputation(dat)
    expect_equal(nrow(si), 20)
    expect_equal(attr(si, "imputation.method"), "hot decking")

    si <- SingleImputation(dat, ~ a + b + c + d)
    expect_equal(nrow(si), 20)
    expect_equal(attr(si, "imputation.method"), "hot decking")

    si <- SingleImputation(dat, a  ~ b + c + d)
    expect_equal(nrow(si), 19)
    expect_equal(attr(si, "imputation.method"), "hot decking")

    # mice errors
    expect_error(SingleImputation(dat, method = "mice"), "Mice imputation failed.")
    expect_error(SingleImputation(dat,  ~ a + b + c + d, method = "mice"), "Mice imputation failed.")

    #choice of algorithm.
    data(bank)
    si <- SingleImputation(bank, method = "mice")
    expect_equal(attr(si, "imputation.method"), "chained equations (predictive mean matching)")

    si <- SingleImputation(bank, method = "hot deck")
    expect_equal(attr(si, "imputation.method"), "hot decking")
})
