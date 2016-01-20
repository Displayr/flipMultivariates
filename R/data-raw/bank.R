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
bank$weight <- bank$weight / mean(bank$weight, na.rm = TRUE)
devtools::use_data(bank, internal = FALSE, overwrite = TRUE)

# type type
data(bank)
Regression(Overall ~ Fees, data = bank, missing = "Imputation (replace missing values with estimates"))
Regression(Overall ~ Fees, data = bank)
zz <- NULL
Regression(Overall ~ Fees , weights = zz, subset = TRUE, data = bank, missing = "Imputation (replace missing values with estimates"))


zdata <- bank#data.frame(Overall = Overall, Fees = Fees)
Regression(Overall ~ Fees, zdata)

Regression(log(Overall) ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)


Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, robust.se = TRUE)




summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = ID, subset = ID > 100))

zw <- bank$ID
zs <- bank$ID > 100
summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = zw, subset = zs))

z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
flipMultivariates:::breusch.pagan(z)
flipMultivariates:::BreuschPagan(z)
car:::ncvTest(z)
summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))

missing <- "Exclude cases with missing data"
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing)

missing <- "Imputation (replace missing values with estimates)"
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing)

missing <- "Use partial data (pairwise correlations)"
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, missing = missing)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing)


Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing)
zbank <- subset(bank, bank$ID > 100 & !is.na(bank$ID))
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = zbank, missing = missing)
summary(zbank)

print(dim(zbank))
cv <- cov(zbank, use = "pairwise.complete.obs")
psych::setCor(2, 3:8, zbank, std = FALSE)

psych::setCor(2, 3:8, cv, std = FALSE, square = TRUE)


psych::setCor(2, 3:8, zbank, std = TRUE)$beta


psych::setCor(2, 3:8, cv, std = TRUE, square = TRUE)$beta


psych::setCor(2, 3:8, zbank, std = FALSE)



    .pairwise.regression <- psych::setCor
    n <- length(body(.pairwise.regression))
    while(as.character(body(.pairwise.regression)[n]) != "setCor.diagram(set.cor, main = main)")
        n <- n - 1
    body(.pairwise.regression)[n] <- NULL
    .pairwise.regression(2, 3:8, zbank)

    print(sum(bank,na.rm = TRUE))

zzbank = AdjustDataToReflectWeights(zbank, weights)
psych::setCor(2, 3:8, zzbank)

zzzbank = bank[complete.cases(bank), ]
psych::setCor(2, 3:8, zzzbank)$beta



psych::setCor(2, 3:8, zzzbank, STD = TRUE)$beta

unscaled.data <- zzzbank
scaled.data <- as.data.frame(scale(zzzbank))
sds.independent = apply(unscaled.data[,3:8], 2, sd, na.rm = TRUE)# / sd(zzzbank[,2], na.rm = TRUE)
sds.independent
sd.dependent <- sd(unscaled.data[,2], na.rm = TRUE)
sd.dependent



lm.unscaled <-summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = unscaled.data))$coef[-1,1]
lm.unscaled
lm.scaled <-summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = scaled.data))$coef[-1,1]
lm.scaled

lm.unscaled * (sds.independent / sd.dependent)

lm.unscaled
lm.unscaled / (sds.independent / sd.dependent)

lm.scaled / (sds.independent / sd.dependent)
lm.unscaled

psych::setCor(2, 3:8, zzzbank, std = FALSE)$beta
psych::setCor(2, 3:8, zzzbank, std = TRUE)$beta / (sds.independent / sd.dependent)



summary(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = as.data.frame(scale(zzzbank))))$coef
psych::setCor(2, 3:8, zzzbank, std = TRUE)$beta

covm <- cov(bank)


psych::setCor(2, 3:8, bank, std = TRUE)$beta


bank.filtered <- subset(bank, bank$ID > 100)
library(psych)
psych::setCor(2, 3:8, bank.filtered)$beta

Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing)


), predictors.index,
        data = estimation.data, std = FALSE)
    partial.coefs <- cbind(lm.cov$beta, lm.cov$se, lm.cov$t, lm.cov$Probability)
    dimnames(partial.coefs) <- list(variable.names[predictors.index],
        c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    beta <- as.matrix(lm.cov$beta)
    fitted <- as.matrix(estimation.data[, predictors.index]) %*% beta
    intercept <- mean(estimation.data[, outcome.name], na.rm = TRUE) - mean(fitted, na.rm = TRUE)
    fitted <- as.matrix(data[, predictors.index]) %*% beta
    result$flip.fitted.values <- fitted + intercept

missing <- "Imputation (replace missing values with estimates)"
type = "Poisson"
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, type = type)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing, type = type)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, missing = missing, type = type)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing, type = type)

missing <- "Imputation (replace missing values with estimates)"
type = "Quasi-Poisson"
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, type = type)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing, type = type)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, missing = missing, type = type)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing, type = type)

missing <- "Imputation (replace missing values with estimates)"
type = "Binary Logit"
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, missing = missing, type = type)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, missing = missing, type = type)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, missing = missing, type = type)
Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, weights = bank$ID, subset = bank$ID > 100, missing = missing, type = type)
