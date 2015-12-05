library(foreign)
mus03data <- read.dta("http://cameron.econ.ucdavis.edu/musbook/mus03data.dta")
summary(mus03data)
devtools::use_data(mus03data, internal = FALSE, overwrite = TRUE)


LinearRegression(ltotexp ~ suppins + phylim + actlim + totchr + age + female + income, data = mus03data)
LinearRegression(ltotexp ~ suppins + phylim + actlim + totchr + age + female + income, robust.se = TRUE, data = mus03data)
print.Regression(LinearRegression(ltotexp ~ suppins + phylim + actlim + totchr + age + female + income,
                                  robust.se = TRUE, data = mus03data))



 "dupersid" "year03"   ""      "famsze"   "educyr"   "totexp"   "private"  "retire"   ""   "white"    "hisp"
[12] "marry"    "northe"   "mwest"    "south"    ""   ""   "msa"      ""   "injury"   "priolist" ""
[23] "omc"      "hmo"      "mnc"      "ratio"    "posexp"   "suppins"  "hvgg"     "hfp"      ""  "hins"     "hdem"
