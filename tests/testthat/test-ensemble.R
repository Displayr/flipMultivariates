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

    expect_error(en <- MachineLearningEnsemble(models,
                                  compare.only = TRUE,
                                  evaluation.subset = eval.subset,
                                  evaluation.weights = weights), NA)

    expect_error(en <- MachineLearningEnsemble(models,
                                  compare.only = FALSE,
                                  evaluation.subset = eval.subset,
                                  evaluation.weights = weights), NA)

    expect_error(en <- MachineLearningEnsemble(models,
                                  compare.only = FALSE,
                                  evaluation.subset = eval.subset,
                                  evaluation.weights = weights,
                                  output = "Ensemble"), NA)

    expect_error(predict(en), NA)
    expect_error(Probabilities.MachineLearningEnsemble(en), NA)

    # no filter or weights - only training accuracy output
    expect_error(en <- MachineLearningEnsemble(models,
                                  compare.only = FALSE,
                                  evaluation.subset = NULL,
                                  evaluation.weights = NULL), NA)

    # same filter as training - different weights warning
    expect_warning(en <- MachineLearningEnsemble(models,
                                  compare.only = FALSE,
                                  evaluation.subset = train.subset,
                                  evaluation.weights = NULL),
                   "Weights used for training the models differ from evaluation weights.")

    # different weights warning
    expect_warning(en <- MachineLearningEnsemble(models,
                                               compare.only = FALSE,
                                               evaluation.subset = eval.subset,
                                               evaluation.weights = weights + 1),
                    "Weights used for training the models differ from evaluation weights.")
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

    expect_error(en <- MachineLearningEnsemble(models,
                                compare.only = TRUE,
                                evaluation.subset = eval.subset,
                                evaluation.weights = weights), NA)

    expect_error(en <- MachineLearningEnsemble(models,
                                compare.only = FALSE,
                                evaluation.subset = eval.subset,
                                evaluation.weights = weights), NA)

    expect_error(en <- MachineLearningEnsemble(models,
                                compare.only = FALSE,
                                evaluation.subset = eval.subset,
                                evaluation.weights = weights,
                                output = "Ensemble"), NA)

    expect_error(predict(en), NA)
    expect_error(Probabilities.MachineLearningEnsemble(en), NA)

    # no filter or weights - only training accuracy output
    expect_error(en <- MachineLearningEnsemble(models,
                                               compare.only = FALSE,
                                               evaluation.subset = NULL,
                                               evaluation.weights = NULL), NA)

    # same filter as training - different weights warning
    expect_warning(en <- MachineLearningEnsemble(models,
                                                 compare.only = FALSE,
                                                 evaluation.subset = train.subset,
                                                 evaluation.weights = NULL),
                   "Weights used for training the models differ from evaluation weights.")

    # different weights warning
    expect_warning(en <- MachineLearningEnsemble(models,
                                                 compare.only = FALSE,
                                                 evaluation.subset = eval.subset,
                                                 evaluation.weights = weights + 1),
                   "Weights used for training the models differ from evaluation weights.")
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

    expect_error(en <- MachineLearningEnsemble(models,
                                compare.only = TRUE,
                                evaluation.subset = eval.subset,
                                evaluation.weights = weights), NA)

    expect_error(en <- MachineLearningEnsemble(models,
                                compare.only = FALSE,
                                evaluation.subset = eval.subset,
                                evaluation.weights = weights), NA)

    expect_error(en <- MachineLearningEnsemble(models,
                                compare.only = FALSE,
                                evaluation.subset = eval.subset,
                                evaluation.weights = weights,
                                output = "Ensemble"), NA)

    expect_error(predict(en), NA)
    expect_error(Probabilities.MachineLearningEnsemble(en),
                 "Probabilities are only applicable to models with categorical outcome variables.")

    # no filter or weights - only training accuracy output
    expect_error(en <- MachineLearningEnsemble(models,
                                               compare.only = FALSE,
                                               evaluation.subset = NULL,
                                               evaluation.weights = NULL), NA)

    # same filter as training - different weights warning
    expect_warning(en <- MachineLearningEnsemble(models,
                                                 compare.only = FALSE,
                                                 evaluation.subset = train.subset,
                                                 evaluation.weights = NULL),
                   "Weights used for training the models differ from evaluation weights.")

    # different weights warning
    expect_warning(en <- MachineLearningEnsemble(models,
                                                 compare.only = FALSE,
                                                 evaluation.subset = eval.subset,
                                                 evaluation.weights = weights + 1),
                   "Weights used for training the models differ from evaluation weights.")
})


