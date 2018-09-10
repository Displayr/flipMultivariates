context("Machine Learning Ensembles")

library(flipMultivariates)
data(adult.2000, package = "flipExampleData")

set.seed(1234)
missing.marital <- runif(2000) > 0.9
missing.occupation <- runif(2000) > 0.9
adult.2000$marital[missing.marital] <- NA
adult.2000$occupation[missing.occupation] <- NA
train.subset <- c(rep(TRUE, 1000), rep(FALSE, 1000))
#attr(train.subset, "name") <- "training data"
eval.subset <- !train.subset
weights <- runif(2000)

# Binary outcome
formula <- income ~ education_num + sex + race
model.types <- c("Random Forest", "Support Vector Machine", "Regression",
                 "Gradient Boost", "CART", "Deep Learning", "LDA")

models <- suppressWarnings(lapply(model.types, function(x) MachineLearning(algorithm = x,
                                                                           type = "Binary Logit",
                                                                           formula = formula,
                                                                           data = adult.2000,
                                                                           missing = "Exclude cases with missing data",
                                                                           subset = train.subset,
                                                                           weights = weights)))

test_that("MachineLearningEnsemble: binary outcome", {

    en <- MachineLearningEnsemble(models,
                                  compare.only = TRUE,
                                  evaluation.filter = eval.subset,
                                  evaluation.weights = weights)

    en <- MachineLearningEnsemble(models,
                                  compare.only = FALSE,
                                  evaluation.filter = eval.subset,
                                  evaluation.weights = weights)

    en <- MachineLearningEnsemble(models,
                                  compare.only = FALSE,
                                  evaluation.filter = eval.subset,
                                  evaluation.weights = weights,
                                  output = "Ensemble")
})

# Categorical outcome
formula <- occupation ~ education_num + sex + race
model.types <- c("Random Forest", "Support Vector Machine", "Regression",
                 "Gradient Boost", "CART", "Deep Learning", "LDA")

models <- suppressWarnings(lapply(model.types, function(x) MachineLearning(algorithm = x,
                                                                           type = "Multinomial Logit",
                                                                           formula = formula,
                                                                           data = adult.2000,
                                                                           missing = "Exclude cases with missing data",
                                                                           subset = train.subset,
                                                                           weights = weights)))

test_that("MachineLearningEnsemble: categorical outcome", {

    en <- MachineLearningEnsemble(models,
                                compare.only = TRUE,
                                evaluation.filter = eval.subset,
                                evaluation.weights = weights)

    en <- MachineLearningEnsemble(models,
                                compare.only = FALSE,
                                evaluation.filter = eval.subset,
                                evaluation.weights = weights)

    en <- MachineLearningEnsemble(models,
                                compare.only = FALSE,
                                evaluation.filter = eval.subset,
                                evaluation.weights = weights,
                                output = "Ensemble")
})

# Numeric outcome
formula <- education_num ~ occupation + sex + race + income
model.types <- c("Random Forest", "Support Vector Machine", "Regression",
                 "Gradient Boost", "CART", "Deep Learning")

models <- suppressWarnings(lapply(model.types, function(x) MachineLearning(algorithm = x,
                                                                           type = "Linear",
                                                                           formula = formula,
                                                                           data = adult.2000,
                                                                           missing = "Exclude cases with missing data",
                                                                           subset = train.subset,
                                                                           weights = weights)))


test_that("MachineLearningEnsemble: numeric outcome", {

    en <- MachineLearningEnsemble(models,
                                compare.only = TRUE,
                                evaluation.filter = eval.subset,
                                evaluation.weights = weights)

    en <- MachineLearningEnsemble(models,
                                compare.only = FALSE,
                                evaluation.filter = eval.subset,
                                evaluation.weights = weights)

    en <- MachineLearningEnsemble(models,
                                compare.only = FALSE,
                                evaluation.filter = eval.subset,
                                evaluation.weights = weights,
                                output = "Ensemble")
})

