library(AER)
data("RecreationDemand", package = "AER")
park.visits <- RecreationDemand[,c(1,2,4,7)]
names(park.visits)[[4]] <- "price"
devtools::use_data(park.visits, internal = FALSE, overwrite = TRUE)


This example is based on data and methods  from Cameron, A Colin and Pravin K. Trivedi (1998), "Regression analsyes of count data",chapter 6.



reg.lm <- LinearRegression(trips ~ quality + price + income, recreational.demand)
reg.lm


predict(reg.lm)
recreational.demand$visited <- as.integer(recreational.demand$trips == 1)
reg.lm <- LinearRegression(visited ~ quality + price + income, recreational.demand)
reg.lm



reg.p <- PoissonRegression(trips ~ quality + price + income, recreational.demand)
reg.pois


# car::ncvTest(reg.lm)
#
#
#
# fm_nb <- glm.nb(trips ~ ., data = RecreationDemand)
# coeftest(fm_nb, vcov = vcovOPG)
# logLik(fm_nb)


