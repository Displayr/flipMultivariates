library(foreign)
bank <- read.spss("C:/q/Install/Examples/Satisfaction.sav",
                  to.data.frame = TRUE)
set.seed(54543)
bank[matrix(runif(prod(dim(bank))), nrow = nrow(bank)) < 0.05] <- NA # Adding missing data 5%.
bank[runif(nrow(bank)) < 0.1, "Overall"] <- NA #Adding missing values to the dependent variable 20%
bank[runif(nrow(bank)) < 0.5, "Branch"] <- NA #Adding missing values to branch the dependent variable 20%
bank$weight <- NA
for (i in unique(bank$ID))
    bank$weight[bank$ID == i] <- ifelse(runif(1)<.05, NA, max(1, min(rnorm(5,2),10)))
devtools::use_data(bank, internal = FALSE, overwrite = TRUE)

summary(bank)

    # Model type
data(bank)
z <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, type = "Linear", data = bank, weight = bank$weight)

zz <- NULL
Regression(Overall ~ Fees , weights = zz, subset = TRUE, data = bank, missing = "Imputation")


zdata <- bank#data.frame(Overall = Overall, Fees = Fees)

Regression(Overall ~ Fees, data)




# Examples
data(bank)
missing = "Error if missing data"
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)

Regression(log(Overall) ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)


Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, robust.se = TRUE)

missing <- "Exclude cases with missing data"
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing)

summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = ID, subset = ID > 100))

zw <- bank$ID
zs <- bank$ID > 100
summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = zw, subset = zs))


missing <- "Imputation"
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing)

missing <- "Use partial data (pairwise)"
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing)
