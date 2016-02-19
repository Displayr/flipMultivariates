context("CART")

data("spam7", package = "DAAG")
spam.sample <- spam7[sample(seq(1,4601), 500, replace=FALSE), ]
data(colas)
data(bank)

test_that("Error if missing data",
{
    # Changing data
    expect_that(print(CART(yesno ~ crl.tot + dollar + bang + money + n000 + make, data = spam.sample, missing = "Error if missing data")), not(throws_error()))
    expect_that(print(CART(d2 ~ d1, data = colas, subset = TRUE,  missing = "Error if missing data")), (throws_error()))
    expect_that(print(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = NULL, output = type, missing = "Error if missing data")), (throws_error()))
    # filter
    expect_that(print(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100,  weights = NULL, output = type, missing = "Error if missing data")), (throws_error()))
    # weight
    expect_that(print(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = bank$ID, output = type, missing = "Error if missing data")), (throws_error()))
    # weight and filter
    expect_that(print(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100,  weights = bank$ID, missing = "Error if missing data")), (throws_error()))
})


# CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = NULL, output = "Sankey", missing = "Imputation (replace missing values with estimates)")
# CART(yesno ~ crl.tot + dollar + bang + money + n000 + make, data = spam.sample, subset = TRUE,  weights = NULL, output = "Sankey", missing = "Imputation (replace missing values with estimates)")
#data(colas)
#CART(d2 ~ d1, data = colas, subset = TRUE,  weights = NULL, output = "Sankey", missing = "Imputation (replace missing values with estimates)")





for (missing in c("Exclude cases with missing data",
                  "Use partial data",
                  "Imputation (replace missing values with estimates)"))
    for (type in c("Sankey", "Tree", "Text"))
        test_that(paste(missing, type),
        {
            expect_that(print(CART(yesno ~ crl.tot + dollar + bang + money + n000 + make, data = spam.sample, subset = TRUE,  weights = NULL, output = type, missing = missing)), not(throws_error()))
            expect_that(print(CART(d2 ~ d1, data = colas, subset = TRUE,  weights = NULL, output = type, missing = missing)), not(throws_error()))
            expect_that(print(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = NULL, output = type, missing = missing)), not(throws_error()))
            # filter
            expect_that(print(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100,  weights = NULL, output = type, missing = missing)), not(throws_error()))
            # weight
            expect_that(print(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = bank$ID, output = type, missing = missing)), not(throws_error()))
            # weight and filter
            expect_that(print(CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100,  weights = bank$ID, output = type, missing = missing)), not(throws_error()))
        })


# CART(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = TRUE,  weights = NULL, output = "Sankey", missing = "Imputation (replace missing values with estimates)")
# CART(yesno ~ crl.tot + dollar + bang + money + n000 + make, data = spam.sample, subset = TRUE,  weights = NULL, output = "Sankey", missing = "Imputation (replace missing values with estimates)")
#data(colas)
#CART(d2 ~ d1, data = colas, subset = TRUE,  weights = NULL, output = "Sankey", missing = "Imputation (replace missing values with estimates)")
