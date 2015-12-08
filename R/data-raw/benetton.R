benetton <- data.frame(Sales = c(1261.08,1475.28,1657.52,2059.05,2303.76,2512.64,2751.46,2787.67,2939.13),
              Advertising = c(43.60,50.44,59.01,66.30,82.36,92.15,100.51,110.06,111.51))
devtools::use_data(benetton, internal = FALSE, overwrite = TRUE)

# Ftting the linear regression.

data(benetton)
benetton.linear <- Regression(Sales ~ Advertising, data = benetton)
benetton.linear

# Interpreting the regression.

Equation(benetton.linear)

PredictionPlot(benetton.linear)



library(effects)
effects <- allEffects(benetton.linear)#, xlevels = list(advertising = c(0, 50, 100, 200)))
plot(effects)

# Validation


benetton$Advertising[-1] / benetton$Sales[-length(benetton$Sales)]




benetton$Advertising[-1] / benetton$Sales[-length(benetton$sales)]

print(benetton.linear)
plot(effects(LinearRegression(sales ~ advertising, benetton)))

plot(effects, ylab = "Sales ($millions)", xlab = "Advertising expenditure ($millions)")

AllEffects("advertising", benetton.linear, allEffects())
help(effect)
