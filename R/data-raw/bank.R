library(foreign)
bank <- read.spss("C:/q/Install/Examples/Satisfaction with missing data.sav",
                  to.data.frame = TRUE)
set.seed(54543)
bank[runif(nrow(bank)) < 0.1, 2] <- NA #Adding missing values to the dependent variable
bank$weight <- NA
for (i in unique(bank$ID))
    bank$weight[bank$ID == i] <- ifelse(runif(1)<.05, NA, max(1, min(rnorm(5,2),10)))
print(summary(bank$weight))
devtools::use_data(bank, internal = FALSE, overwrite = TRUE)


# Model type
data(bank)
zz <- NULL
Regression(Overall ~ Fees , weights = zz, subset = TRUE, data = bank, missing = "Imputation")


zdata <- bank#data.frame(Overall = Overall, Fees = Fees)

Regression(Overall ~ Fees, data)




# Examples
data(bank)
missing = "Error if missing data"
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)

LinearRegression(log(Overall) ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)


LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, robust.se = TRUE)

missing <- "Exclude cases with missing data"
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing)
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, missing = missing)
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing)

summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = ID, subset = ID > 100))

zw <- bank$ID
zs <- bank$ID > 100
summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = zw, subset = zs))


missing <- "Imputation"
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing)
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, missing = missing)
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing)

missing <- "Use partial data (pairwise)"
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing)
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, missing = missing)
LinearRegression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing)
