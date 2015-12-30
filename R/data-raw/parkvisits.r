library(AER)
data("RecreationDemand", package = "AER")
park.visits <- RecreationDemand[,c(1,2,4,7)]
names(park.visits)[[4]] <- "price"
devtools::use_data(park.visits, internal = FALSE, overwrite = TRUE)


This example is based on data and methods  from Cameron, A Colin and Pravin K. Trivedi (1998), "Regression analsyes of count data",chapter 6.


data(park.visits)
Regression(trips ~ quality + price + income, data = park.visits)
Regression(trips ~ quality + price + income, data = park.visits, robust.se = TRUE)

Regression(trips ~ quality + price + income, data = park.visits, method = "Poisson")
Regression(trips ~ quality + price + income, data = park.visits, method = "Quasi-Poisson", robust.SE = TRUE)


LinearRegression(trips ~ quality + price + income, data = recreational.demand)


predict(reg.lm)
recreational.demand$visited <- as.integer(recreational.demand$trips == 1)
reg.lm <- LinearRegression(visited ~ quality + price + income, recreational.demand)
reg.lm



reg.p <- PoissonRegression(trips ~ quality + price + income, park.visits)
reg.pois


data(park.visits)
CART(trips ~ quality + price + income, park.visits)
CART(trips ~ quality + price + income, park.visits, output = "Tree")
CART(trips ~ quality + price + income, park.visits, output = "Text")


data(cpus, package="MASS")
cpuss <- cpus
QPopulationWeight = runif(nrow(cpus))
cpuss$weights = QPopulationWeight
QFilter = runif(nrow(cpus)) > 0.5
cpuss$mmax <- factor(round(cpus$mmax / 10000) + 1, labels= c("Near 0", "10,000", "20,000", "30,000", "Sixty Thousands"))



# car::ncvTest(reg.lm)
#
#
#
# fm_nb <- glm.nb(trips ~ ., data = RecreationDemand)
# coeftest(fm_nb, vcov = vcovOPG)
# logLik(fm_nb)


