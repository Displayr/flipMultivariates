context("Propensity Weights")

data(burger.brand.tracking, package = "flipExampleData")
burg <- subset(burger.brand.tracking, select = c("S1", "S2", "S3", "Q6", "Q10", "Q7"))
burg$burgers <- factor(as.numeric(with(burg, grepl("burger", Q6, ignore.case = TRUE))))
set.seed(12321)
burg <- burg[sample.int(nrow(burg), size = 1e3), ]
row.names(burg) <- 1:nrow(burg)

vectorOutput <- function(x)
{
    output <- PropensityWeights(x)
    expect_true(inherits(output, "numeric")) && expect_true(length(output) == NROW(x$model))
}

test_that("Only relevant models are possible to use", {
    binary.classifier.error.msg <- "Propensity weights can only be saved for binary classification models"
    numeric.formula <- Q10 ~ S1 + S2 + Q7
    binary.formula <- burgers ~ S1 + S2 + Q7
    multiclass.formula <- Q7 ~ S1 + S2
    numeric.regression <- c("Linear", "Poisson", "Quasi-Poisson", "NBD")
    reg.models <- lapply(numeric.regression,
                         function(x) flipRegression::Regression(numeric.formula, data = burg, type = x))
    lapply(reg.models, function(x) expect_error(PropensityWeights(x), binary.classifier.error.msg))
    ordinal.reg <- flipRegression::Regression(numeric.formula, data = burg, type = "Ordered Logit")
    expect_error(PropensityWeights(ordinal.reg), binary.classifier.error.msg)
    binary.class.reg <- c("Binary Logit", "Multinomial Logit")
    binary.reg <- lapply(binary.class.reg, function(x) flipRegression::Regression(binary.formula, data = burg, type = x))
    lapply(binary.reg, vectorOutput)
    # Multinomial logit not applicable if multiclass
    multiclass.error.msg <- "The supplied model is a multiclass classification model with 7 outcome categories/class labels"
    multinomial.model <- flipRegression::Regression(multiclass.formula, data = burg, type = "Multinomial Logit")
    expect_error(PropensityWeights(multinomial.model), multiclass.error.msg)

    ml.models <- c("CART", "Random Forest", "Deep Learning", "Support Vector Machine", "Gradient Boosting")
    numeric.ml.models <- lapply(ml.models,
                                function(x) do.call(MachineLearning,
                                                    list(algorithm = x, data = burg, formula = numeric.formula, max.epochs = 1e3)))
    lapply(numeric.ml.models, function(x) expect_error(PropensityWeights(x), binary.classifier.error.msg))
    ml.classifiers <- c(ml.models, "Linear Discriminant Analysis")
    classifier.ml <- lapply(ml.classifiers[-1L],
                            function(x) do.call(MachineLearning,
                                                list(algorithm = x, data = burg, formula = binary.formula)))
    lapply(classifier.ml, vectorOutput)
})

test_that("Binary Logit with single class handled", {
    y <- factor(rep(1, 10L))
    x <- runif(10L)
    expect_warning(logit.model <- flipRegression::Regression(y ~ x, type = "Binary Logit"),
                   "The Outcome variable needs to contain two or more categories")
    binary.classifier.error.msg <- "Propensity weights can only be saved for binary classification models"
    expect_warning(expect_error(PropensityWeights(logit.model), binary.classifier.error.msg),
                   "The Outcome variable only has a single category for this Binary Logit Regression model")
})
