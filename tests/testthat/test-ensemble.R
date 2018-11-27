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

models.args <- lapply(model.types, function(x) list(algorithm = x, type = "Binary Logit"))

models <- suppressWarnings(lapply(model.types, function(x) MachineLearning(algorithm = x,
                                                                           type = "Binary Logit",
                                                                           formula = formula,
                                                                           data = adult.2000,
                                                                           missing = "Exclude cases with missing data",
                                                                           subset = train.subset,
                                                                           weights = weights,
                                                                           seed = 12321,
                                                                           show.labels = FALSE)))

test_that("MachineLearningEnsemble: binary outcome", {

    expect_error(en <- MachineLearningEnsemble(models,
                                  compare.only = TRUE,
                                  evaluation.subset = eval.subset,
                                  evaluation.weights = weights), NA)

    expect_error(en <- MachineLearningEnsemble(models,
                                  compare.only = FALSE,
                                  evaluation.subset = eval.subset,
                                  evaluation.weights = weights), NA)
    comparison.table <- en$comparison

    expect_error(en <- MachineLearningEnsemble(models,
                                               compare.only = FALSE,
                                               optimal.ensemble = TRUE,
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
                                  optimal.ensemble = TRUE,
                                  evaluation.subset = NULL,
                                  evaluation.weights = NULL), NA)

    # same filter as training - different weights warning
    expect_warning(en <- MachineLearningEnsemble(models,
                                  compare.only = FALSE,
                                  optimal.ensemble = TRUE,
                                  evaluation.subset = train.subset,
                                  evaluation.weights = NULL),
                   "Weights used for training the models differ from evaluation weights.")

    # different weights warning
    expect_warning(en <- MachineLearningEnsemble(models,
                                               compare.only = FALSE,
                                               optimal.ensemble = TRUE,
                                               evaluation.subset = eval.subset,
                                               evaluation.weights = weights + 1),
                    "Weights used for training the models differ from evaluation weights.")

    # Check that building the models before the ensemble is the same as building for the ensemble.
    new.models.en <- suppressWarnings(MachineLearningMulti(formula = formula,
                                                           data = adult.2000,
                                                           subset = train.subset,
                                                           weights = weights,
                                                           evaluation.subset = eval.subset,
                                                           missing = "Exclude cases with missing data",
                                                           models.args = models.args))
    # DeepLearning is random despite seeds set and same inputs.
    new.models.en$comparison[6, 4] <- comparison.table[6, 4] <- 0
    expect_true(isTRUE(all.equal(new.models.en$comparison, comparison.table)))
})

# Categorical outcome
formula <- occupation ~ education_num + sex + race
model.types <- c("Random Forest", "Support Vector Machine", "Regression",
                 "Gradient Boost", "CART", "Deep Learning", "LDA")

models.args <- lapply(model.types, function(x) list(algorithm = x, type = "Multinomial Logit"))

models <- suppressWarnings(lapply(model.types, function(x) MachineLearning(algorithm = x,
                                                                           type = "Multinomial Logit",
                                                                           formula = formula,
                                                                           data = adult.2000,
                                                                           missing = "Exclude cases with missing data",
                                                                           subset = train.subset,
                                                                           weights = weights,
                                                                           seed = 12321,
                                                                           show.labels = FALSE)))

test_that("MachineLearningEnsemble: categorical outcome", {

    expect_error(en <- MachineLearningEnsemble(models,
                                compare.only = TRUE,
                                evaluation.subset = eval.subset,
                                evaluation.weights = weights), NA)

    expect_error(en <- MachineLearningEnsemble(models,
                                compare.only = FALSE,
                                evaluation.subset = eval.subset,
                                evaluation.weights = weights), NA)
    comparison.table <- en$comparison

    expect_error(en <- MachineLearningEnsemble(models,
                                               compare.only = FALSE,
                                               optimal.ensemble = TRUE,
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
                                               optimal.ensemble = TRUE,
                                               evaluation.subset = NULL,
                                               evaluation.weights = NULL), NA)

    # same filter as training - different weights warning
    expect_warning(en <- MachineLearningEnsemble(models,
                                                 compare.only = FALSE,
                                                 optimal.ensemble = TRUE,
                                                 evaluation.subset = train.subset,
                                                 evaluation.weights = NULL),
                   "Weights used for training the models differ from evaluation weights.")

    # different weights warning
    expect_warning(en <- MachineLearningEnsemble(models,
                                                 compare.only = FALSE,
                                                 optimal.ensemble = TRUE,
                                                 evaluation.subset = eval.subset,
                                                 evaluation.weights = weights + 1),
                   "Weights used for training the models differ from evaluation weights.")

    # Check that building the models before the ensemble is the same as building for the ensemble.
    new.models.en <- suppressWarnings(MachineLearningMulti(formula = formula,
                                                           data = adult.2000,
                                                           subset = train.subset,
                                                           weights = weights,
                                                           evaluation.subset = eval.subset,
                                                           missing = "Exclude cases with missing data",
                                                           models.args = models.args))
    # DeepLearning is random despite seeds set and same inputs.
    new.models.en$comparison[6, 3:4] <- comparison.table[6, 3:4] <- 0
    expect_true(isTRUE(all.equal(new.models.en$comparison, comparison.table)))
})

# Numeric outcome
formula <- education_num ~ occupation + sex + race + income
model.types <- c("Random Forest", "Support Vector Machine", "Regression",
                 "Gradient Boost", "CART", "Deep Learning")

models.args <- lapply(model.types, function(x) list(algorithm = x, type = "Linear"))

models <- suppressWarnings(lapply(model.types, function(x) MachineLearning(algorithm = x,
                                                                           type = "Linear",
                                                                           formula = formula,
                                                                           data = adult.2000,
                                                                           missing = "Exclude cases with missing data",
                                                                           subset = train.subset,
                                                                           weights = weights,
                                                                           seed = 12321,
                                                                           show.labels = FALSE)))

test_that("MachineLearningEnsemble: numeric outcome", {

    expect_error(en <- MachineLearningEnsemble(models,
                                compare.only = TRUE,
                                evaluation.subset = eval.subset,
                                evaluation.weights = weights), NA)

    expect_error(en <- MachineLearningEnsemble(models,
                                compare.only = FALSE,
                                evaluation.subset = eval.subset,
                                evaluation.weights = weights), NA)
    comparison.table <- en$comparison

    expect_error(en <- MachineLearningEnsemble(models,
                                               compare.only = FALSE,
                                               optimal.ensemble = TRUE,
                                               evaluation.subset = eval.subset,
                                               evaluation.weights = weights), NA)

    expect_error(en <- MachineLearningEnsemble(models,
                                compare.only = FALSE,
                                optimal.ensemble = TRUE,
                                evaluation.subset = eval.subset,
                                evaluation.weights = weights,
                                output = "Ensemble"), NA)

    expect_error(predict(en), NA)
    expect_error(Probabilities.MachineLearningEnsemble(en),
                 "Probabilities are only applicable to models with categorical outcome variables.")

    # no filter or weights - only training accuracy output
    expect_error(en <- MachineLearningEnsemble(models,
                                               compare.only = FALSE,
                                               optimal.ensemble = TRUE,
                                               evaluation.subset = NULL,
                                               evaluation.weights = NULL), NA)

    # same filter as training - different weights warning
    expect_warning(en <- MachineLearningEnsemble(models,
                                                 compare.only = FALSE,
                                                 optimal.ensemble = TRUE,
                                                 evaluation.subset = train.subset,
                                                 evaluation.weights = NULL),
                   "Weights used for training the models differ from evaluation weights.")

    # different weights warning
    expect_warning(en <- MachineLearningEnsemble(models,
                                                 compare.only = FALSE,
                                                 optimal.ensemble = TRUE,
                                                 evaluation.subset = eval.subset,
                                                 evaluation.weights = weights + 1),
                   "Weights used for training the models differ from evaluation weights.")

    # Check that building the models before the ensemble is the same as building for the ensemble.
    new.models.en <- suppressWarnings(MachineLearningMulti(formula = formula,
                                                           data = adult.2000,
                                                           subset = train.subset,
                                                           weights = weights,
                                                           evaluation.subset = eval.subset,
                                                           missing = "Exclude cases with missing data",
                                                           models.args = models.args))
    # DeepLearning is random despite seeds set and same inputs.
    new.models.en$comparison[6, 3:6] <- comparison.table[6, 3:6] <- 0
    expect_true(isTRUE(all.equal(new.models.en$comparison, comparison.table)))
})


