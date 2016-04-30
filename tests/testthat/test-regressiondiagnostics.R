context("Regression Diagnostics")
zformula <- formula("Overall ~ Fees + Interest + Phone + Branch + Online + ATM")
data(bank)
sb <- bank$ID > 100
attr(sb, "label") <- "ID greater than 100"
wgt <- bank$ID
wgt[is.na(wgt)] = 0
attr(wgt, "label") <- "ID"


# for (type in c("Linear" ,"Poisson", "Quasi-Poisson", "Binary Logit", "NBD"))
# {
#     test_that(paste("diagnostic plots works", type),
#     {
#          z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = wgt, data = bank, missing = missing, detail = FALSE, type = type)
#         expect_that(residualPlots(z), not(throws_error()))
#     })
# }
#
#
# for (type in c("Ordered Logit",  "Multinomial Logit"))
# {
#     test_that(paste("diagnostic plots failed when not apropriate",type),
# {
#       z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = wgt, data = bank, missing = missing, detail = FALSE, type = type)
#       expect_that(car::residualPlots(z), throws_error())
#  #     expect_that(car::marginalModelPlots(z), throws_error())
# })
# }
#
test_that("Durbin Watson",
{
    # Negative auto correlation
    set.seed(1)
    x = runif(100) * 2
    y = x + rep(1:2,50)
    dat = data.frame(x, y)
    z = DurbinWatson((Regression(y ~ x, data = dat, type = "Binary Logit")))
    expect_equal(z$d, 2.62, tolerance = 1.0e-3)
    expect_equal(z$p, 0, tolerance = 1.0e-5)
    z = DurbinWatson((Regression(y ~ x, data = dat)))
    expect_equal(z$d, 3.94, tolerance = 1.0e-3)
    expect_equal(z$p, 0, tolerance = 1.0e-3)
    # Positive autocorrelation
    y = x + c(rep(0,50),rep(1,50))
    dat = data.frame(x, y)
    z = DurbinWatson((Regression(y ~ x, data = dat, type = "Binary Logit")))
    expect_equal(z$d, 1.49, tolerance = 1.0e-2)
    expect_equal(z$p, 0.004, tolerance = 1.0e-2)
    z = DurbinWatson((Regression(y ~ x, data = dat)))
    expect_equal(z$d, 0.0444, tolerance = 1.0e-3)
    expect_equal(z$p, 0, tolerance = 1.0e-3)
    # Random (no autocorrelation)
    y = x + rnorm(100)
    dat = data.frame(x, y)
    z = DurbinWatson((Regression(y ~ x, data = dat, type = "Binary Logit")))
    expect_equal(z$d, 2.06, tolerance = 1.0e-2)
    expect_equal(z$p, 0.712, tolerance = 1.0e-3)
    z = DurbinWatson((Regression(y ~ x, data = dat)))
    expect_equal(z$d, 2.08, tolerance = 1.0e-3)
    expect_equal(z$p, 0.646, tolerance = 1.0e-3)
    # Comparing with car package
    z = car::durbinWatsonTest(lm(y ~ x, data = dat), simulate = TRUE, reps = 100000, alternative = "two.sided")
    z1 <- DurbinWatson((Regression(y ~ x, data = dat)), n.permutations = 100000)
    expect_equal(z$dw, z1$d, tolerance = 1.0e-3)
    expect_equal(z$p, z1$p, tolerance = 1.0e-2)
    missing <- missing <- "Exclude cases with missing data"
    for (type in c( "Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD"))
    {
        z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, detail = FALSE, data = bank, type = type)
        expect_that(DurbinWatson(z), not(throws_error()))
    }
})

for (type in c("Linear", "Linear","Poisson", "Quasi-Poisson", "Binary Logit", "NBD"))
    test_that(paste("Cooks distance works:",type),
    {
        missing = "Exclude cases with missing data"
        z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, detail = FALSE, data = bank, type = type)
        expect_that((cooks.distance(z)), not(throws_error()))
    })

for (type in c("Ordered Logit",  "Multinomial Logit"))
    test_that(paste("Cooks distance does not works:",type),
    {
        missing = "Exclude cases with missing data"
        z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, detail = FALSE, data = bank, type = type)
        expect_that(cooks.distance(z), throws_error())
    })


for(missing in c("Imputation (replace missing values with estimates)", "Exclude cases with missing data"))
    for (type in c( "Linear","Poisson", "Quasi-Poisson","Binary Logit", "Ordered Logit", "NBD", "Multinomial Logit"))
        test_that(paste("Accuracy and R-square:",missing, type),
        {
            z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, type = type)
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
            z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, missing = missing, data = bank, subset = sb,  type = type)
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
            z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = wgt, missing = missing, data = bank, type = type)
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
            z = Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, weights = wgt, missing = missing, data = bank, subset = sb,  type = type)
            expect_true(Accuracy(z) > 0.2)
            expect_true(GoodnessOfFit(z)$value > 0.17)
        })


test_that("Tests of non-constant variance (Breush-Pagen test)",
{
    # Unfilitered
    z = ncvTest(Regression(zformula, data = bank))
    z1 = car::ncvTest(lm(zformula, data = bank))
    expect_equal(z$p, z1$p, tolerance = 1.0e-8)

    # Filitered
    z = ncvTest(Regression(zformula, data = bank, subset = sb))
    z1 = car::ncvTest(lm(zformula, data = bank, subset = sb))
    expect_equal(z$p, z1$p, tolerance = 1.0e-8)

    # Weighted.
    expect_that(ncvTest(Regression(zformula, data = bank,  weights = wgt)), throws_error())

    # Weighted and filtered
    expect_that(ncvTest(Regression(zformula, data = bank, subset = sb,  weights = wgt)), throws_error())

    # Unweighted
    zRegression <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
    zWLS <- lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank)
    expect_equal(ncvTest(zRegression)$p, ncvTest(zWLS)$p)
    # Filtered
    zRegression <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100)
    zWLS <- lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100)
    expect_equal(ncvTest(zRegression)$p, ncvTest(zWLS)$p)
    # Weighted.
    zRegression <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, weights = bank$ID)
    zWLS <- lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, weights = bank$ID)
    expect_equal(round(ncvTest(zRegression)$p, 1),round(ncvTest(zWLS)$p,1))

})

test_that("VIF",
           {
     library(car)
     # Unweighted - linear
     zRegression <- vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))
     zR <- vif(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank))
     expect_equal(zRegression, zR)
     # Filtered - linear
     zRegression <- vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100))
     zR <- vif(lm(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100))
     expect_equal(zRegression, zR)
     # Weighted.
     expect_that(vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, subset = bank$ID > 100, weights = bank$ID)), not(throws_error()))
     # Logit (used as a proxy for all the glms)
     type = "Binary Logit"
     zRegression <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type)
     zR <- glm(Overall >= 4 ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank,  family = binomial(link = "logit"))
     expect_equal(vif(zRegression), vif(zR))
     # Logit - filtered
     zRegression <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = bank$ID > 100)
     zR <- glm(Overall >= 4 ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank,  family = binomial(link = "logit"), subset = bank$ID > 100)
     expect_equal(vif(zRegression), vif(zR))
     # Logit - filtered and weighted
     z = wgt > 0 & complete.cases(bank[,c("Overall","Fees","Interest","Phone","Branch","Online","ATM")])
     zBank = bank[z, ]
     zBank$dd = zBank$Overall >= 4
     zwgt = wgt[z]
     zRegression <- Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = zBank, weights = zwgt, type = type)
     zR <- survey::svyglm(dd ~ Fees + Interest + Phone + Branch + Online + ATM, data = zBank, design = flipMultivariates:::weightedSurveyDesign(zBank, zwgt), family = quasibinomial())
     expect_equal(vif(zRegression), vif(zR))
     # Checking for no errors
     for (type in c("Poisson","NBD", "Quasi-Poisson"))
        expect_that(capture.output(vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = bank$ID > 100, weights = bank$ID))), not(throws_error()))
     # Checking for errors in other types of models
     for (type in c( "Ordered Logit",  "Multinomial Logit"))
        expect_that(capture.output(vif(Regression(Overall ~ Fees + Interest + Phone + Branch + Online + ATM, data = bank, type = type, subset = bank$ID > 100, weights = bank$ID))), (throws_error()))
 })
