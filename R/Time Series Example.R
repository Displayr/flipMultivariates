# seo <- read.csv('C:\\Users\\Tim\\Dropbox (Numbers)\\DataCracker Marketing\\SEO\\SEO 2014.csv')
#
#
#
# library(dynlm)
# names(seo)[2] <- "SignUps"
# seo$WeightedAveragePosition <- 67.3 / (67.3 + 19.4 + 10) * seo$AvgpositiononGoogle + 19.4 / (67.3 + 19.4 + 10) * seo$AvgpositiononYahoo + 10 / (67.3 + 19.4 + 10) * seo$AvgpositiononBing
# z <- is.na(seo$WeightedAveragePosition)
# seo$WeightedAveragePosition[z] <- 67.3 / (67.3 + 19.4) * seo$AvgpositiononGoogle[z] + 19.4 / (67.3 + 19.4) * seo$AvgpositiononYahoo[z]
# z <- is.na(seo$WeightedAveragePosition)
# seo$WeightedAveragePosition[z] <- 67.3 / (67.3 + 19.4) * seo$AvgpositiononGoogle[z] + 10 / (67.3 + 10) * seo$AvgpositiononBing[z]
# seo$WeightedAveragePosition2 <- seo$WeightedAveragePosition^2
# seo$WeightedAveragePosition3 <- seo$WeightedAveragePosition^3
# seo$LogSignUps <- log(seo$SignUps + 1)
# seo$dates <- seq(as.Date("2014/1/7"), by = "week", length.out = n)
#
# n <- nrow(seo)
# seo$LagLogSignUps <- c(NA, seo$SignUps[-n])
# seo$LagWeightedAveragePosition <- c(NA, seo$WeightedAveragePosition[-n])
#
# seo$DiffWeightedAveragePosition <- c(NA, seo$WeightedAveragePosition[-1] - seo$WeightedAveragePosition[-n])
# #seo <- ts(seo, start = c(2014,1),  frequency = 52.25) #creating a time series object
#
#
# trend <- dynlm(SignUps ~ trend(SignUps), data = seo)
# summary(trend)
#
# ltrend <- dynlm(LogSignUps ~ trend(SignUps), data = seo )
# summary(ltrend)
#
#
# naive <- dynlm(SignUps ~ Averageposition + AvgpositiononGoogle + AvgpositiononYahoo + AvgpositiononBing + Noofpage1ranking + Rankedpages+ Totalranking + ofkeywordsranking, data = seo)
# summary(naive)
#
# lnaive <- dynlm(LogSignUps ~ Averageposition + AvgpositiononGoogle + AvgpositiononYahoo + AvgpositiononBing + Noofpage1ranking + Rankedpages+ Totalranking + ofkeywordsranking, data = seo)
# summary(lnaive)
#
# lnaiveTrend <- dynlm(LogSignUps ~ trend(LogSignUps) + WeightedAveragePosition + Averageposition + AvgpositiononGoogle + AvgpositiononYahoo + AvgpositiononBing + Noofpage1ranking + Rankedpages+ Totalranking + ofkeywordsranking, data = seo)
# summary(lnaiveTrend)
#
# lAverage <- dynlm(LogSignUps ~ Averageposition + trend(LogSignUps), data = seo)
# summary(lAverage)
# lWAverage <- dynlm(LogSignUps ~ WeightedAveragePosition + trend(LogSignUps), data = seo)
# summary(lWAverage)
#
#
# model <- dynlm(LogSignUps ~ trend(LogSignUps) + WeightedAveragePosition + Averageposition +  Noofpage1ranking + Rankedpages+ Totalranking + ofkeywordsranking, data = seo)
# summary(model)
#
# model <- dynlm(LogSignUps ~ trend(LogSignUps) + WeightedAveragePosition + Averageposition +  Noofpage1ranking + Rankedpages, data = seo)
# summary(model)
#
# model <- dynlm(LogSignUps ~ trend(LogSignUps) + WeightedAveragePosition + Averageposition +  Rankedpages, data = seo)
# summary(model)
#
# model <- dynlm(LogSignUps ~ trend(LogSignUps) + WeightedAveragePosition, data = seo)
# summary(model)
#
# model <- dynlm(LogSignUps ~ trend(LogSignUps) + WeightedAveragePosition + WeightedAveragePosition2+ WeightedAveragePosition3, data = seo)
# summary(model)
#
# model <- dynlm(LogSignUps ~ L(LogSignUps) + WeightedAveragePosition + WeightedAveragePosition2+ WeightedAveragePosition3, data = seo)
# summary(model)
#
# model <- dynlm(LogSignUps ~ LagWeightedAveragePosition , data = seo)
# summary(model)
#
# model <- dynlm(LogSignUps ~ LagLogSignUps + WeightedAveragePosition + WeightedAveragePosition2+ WeightedAveragePosition3, data = seo)
# summary(model)
# plot(seo$dates , seo$SignUps, type = "l")
# lines(seo$dates , c(exp(predict(model)) - 1, NA), lty = 2)
#
#
# model <- dynlm(LogSignUps ~ trend(LogSignUps) + WeightedAveragePosition + WeightedAveragePosition2, data = seo)
# summary(model)
# plot(seo$dates , seo$SignUps, type = "l")
# lines(seo$dates , c(exp(predict(model)) - 1), lty = 2)
#
# model <- dynlm(LogSignUps ~ trend(LogSignUps) + WeightedAveragePosition , data = seo)
# summary(model)
# plot(seo$dates , seo$SignUps, type = "l")
# lines(seo$dates , c(exp(predict(model)) - 1), lty = 2)
#
# model <- dynlm(LogSignUps ~ trend(LogSignUps) + WeightedAveragePosition + DiffWeightedAveragePosition, data = seo)
# summary(model)
# plot(seo$dates , seo$SignUps, type = "l")
# lines(seo$dates , c(exp(predict(model)) - 1), lty = 2)
#
# model <- dynlm(LogSignUps ~ trend(LogSignUps) + WeightedAveragePosition , data = seo)
# summary(model)
# plot(seo$dates , seo$SignUps, type = "l")
# lines(seo$dates , c(exp(predict(model)) - 1), lty = 2)
#
#
# plot(seo$WeightedAveragePosition, seo$Averageposition)
# cor(seo$WeightedAveragePosition, seo$Averageposition)
# position <- dynlm(SignUps ~ trend(SignUps), data = )
#
# model <- dynlm(LogSignUps ~ LagLogSignUps + WeightedAveragePosition, data = seo)
# summary(model)
# plot(seo$dates , seo$SignUps, type = "l")
# lines(seo$dates , c(exp(predict(model)) - 1, NA), lty = 2)
#
#
# model <- dynlm(SignUps ~ LagLogSignUps + WeightedAveragePosition, data = seo)
# summary(model)
# plot(seo$dates , seo$SignUps, type = "l")
# lines(seo$dates , c(predict(model), NA), lty = 2)
#
# model <- dynlm(SignUps ~ trend(LagLogSignUps) + WeightedAveragePosition, data = seo)
# summary(model)
# plot(seo$dates , seo$SignUps, type = "l")
# lines(seo$dates , c(predict(model)), lty = 2)
#
#
# model <- dynlm(LogSignUps ~ trend(LagLogSignUps) + WeightedAveragePosition, data = seo)
# summary(model)
# plot(seo$dates , seo$SignUps, type = "l")
# lines(seo$dates , c(exp(predict(model)) - 1), lty = 2)
#
#
# seo$
#
# # Using data from: http://searchengineland.com/google-slightly-bing-yahoo-october-comscore-search-report-209459
#
#
# model2 <- dynlm(SignUps ~ Averageposition + AvgpositiononGoogle + AvgpositiononYahoo + AvgpositiononBing + Noofpage1ranking + Rankedpages+ Totalranking + ofkeywordsranking, data = seo)
# summary(model)
