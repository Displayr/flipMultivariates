library(foreign)
bank <- read.spss("C:/q/Install/Examples/Satisfaction with missing data.sav",
                  to.data.frame = TRUE)
set.seed(54543)
bank[runif(nrow(bank)) < 0.1, 2] <- NA #Adding missing values to the dependent variable
devtools::use_data(bank, internal = FALSE, overwrite = TRUE)



round(cor(SingleImputaton(bank, "Overall")),2)

round(cor(bank, use = "pairwise.complete.obs"),2)

dim(bank)

names(bank)
library(mice)
set.seed(12321)
bank.imputed <- SingleImputaton(bank, "Overall")
round(cor(bank.imputed),2)

row.names <- rownames(bank)
nrow(bank)
nrow(bank.imputed)
