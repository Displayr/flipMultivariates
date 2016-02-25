library(foreign)
cola <- read.spss("C:/q/Install/Examples/colas.sav",
                  to.data.frame = TRUE)

levels(cola$d2) <- enc2utf8(levels(cola$d2))
levels(cola$q7) <- enc2utf8(levels(cola$q7))
# levels(cola$d2) <- sub("\u2019", "'", levels(cola$d2))
# levels(cola$q7) <- sub("\u2019", "'", levels(cola$q7))

devtools::use_data(cola, internal = FALSE, overwrite = TRUE)
