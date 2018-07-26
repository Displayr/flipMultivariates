context("Machine Learning")

data(adult.2000, package = "flipExampleData")
# create some missing data
adult.2000$relationship[1:200] <- NA
set.seed(1234)
adult.2000$wgt <- runif(2000) * 10
adult.2000$subset <- rep(c(TRUE, TRUE, TRUE, FALSE), 500)

algorithms <- c("Support Vector Machine", "Random Forest", "Deep Learning",
               "Gradient Boosting", "Linear Discriminant Analysis", "CART", "Regression")

for (alg in algorithms)
{
    test_that(paste0("Machine Learning: ", alg), {

        expect_error(ml <- suppressWarnings(MachineLearning(alg,
                              formula = sex ~ education_num + marital + workclass,
                              data = adult.2000)), NA)
        suppressWarnings(print(ml))
    })
}
