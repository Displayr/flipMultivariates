library(foreign)
cola <- read.spss("C:/q/Install/Examples/colas.sav",
                  to.data.frame = TRUE)
devtools::use_data(cola, internal = FALSE, overwrite = TRUE)
