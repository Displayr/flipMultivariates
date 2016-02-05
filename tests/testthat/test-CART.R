context("CART")

library(DAAG)
library(DAAG)
spam.sample <- spam7[sample(seq(1,4601), 500, replace=FALSE), ]

library(rpart)
spam.rpart <- rpart(formula = yesno ~ crl.tot + dollar + bang +
                    money + n000 + make,  method="class", data=spam7)


spam.rpart <- CART(formula = yesno ~ crl.tot + dollar + bang + money + n000 + make,  data=spam7)
spam.rpart



plot(spam.rpart)     # Draw tree
text(spam.rpart)     # Add labeling

