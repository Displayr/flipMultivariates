library(foreign)
colas <- read.spss("C:/q/Install/Examples/colas.sav",
                  to.data.frame = TRUE)
devtools::use_data(colas, internal = FALSE, overwrite = TRUE)

data(colas)
data <- data.frame(Cola.drinking.frequency.CocaCola = colas$q2a, Exercise.frequency = colas$q8)
QFilter = colas$d3 == "Male"
linear.regression <- Regression(Cola.drinking.frequency.CocaCola ~ Exercise.frequency, data, subset = QFilter)
linear.regression

CART(d2 ~ d1, data = colas)

