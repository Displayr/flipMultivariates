context("CART")

for (type in c("Sankey Plot", "Tree", "Text"))
    test_that(paste("No error", type),
    {
        # Changing data
        library(DAAG)
        spam.sample <- spam7[sample(seq(1,4601), 500, replace=FALSE), ]
        expect_that(print(CART(yesno ~ crl.tot + dollar + bang + money + n000 + make, data = spam.sample, subset = TRUE,  weights = NULL, output = type)), not(throws_error()))
        data(colas)
        expect_that(print(CART(d2 ~ d1, data = colas, subset = TRUE,  weights = NULL, output = type)), not(throws_error()))
        data(bank)
        expect_that(print(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = NULL, output = type)), not(throws_error()))
        # filter
        expect_that(print(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100,  weights = NULL, output = type)), not(throws_error()))
        # weight
        expect_that(print(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = bank$ID, output = type)), not(throws_error()))
        # weight and filter
        expect_that(print(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100,  weights = bank$ID, output = type)), not(throws_error()))
    })


