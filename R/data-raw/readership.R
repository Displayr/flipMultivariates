readership = matrix(c(5,18,19,12,3,7,46,29,40,7,2,20,39,49,16), 5,
    dimnames = list(
    "Level of Education" = c("Some primary", "Primary completed", "Some secondary", "Secondary completed", "Some tertiary"),
    "Category of Readership" = c("Glance", "Fairly thorough", "Very thorough")))
devtools::use_data(readership, internal = FALSE, overwrite = TRUE)

CorrespondenceAnalysis(readership)
CorrespondenceAnalysis(readership, interactive = FALSE)
CorrespondenceAnalysis(readership, interactive = TRUE)

library(ca)
ca(readership)
summary(ca(readership))





"Symmetric", "Row Principal", "Column Principal",

scaleCorrespondenceAnalysis(ca(readership))

My paper  = ca package = SPSS
Standard Coordinates = Standard Output of Ca
Principal CA = "symmetric" = SPSS "Principal"
             =  "rowprincipal" = "Row Principal"


z = ca(readership)
caNormalization(z, "Principal")
caNormalization(z, "Column principal")
caNormalization(z, "Row principal")
caNormalization(z, "Symmetrical (½)")

library(scatterD3)
scatterD3(x = mtcars$wt, y = mtcars$mpg, point_size = 15, point_opacity = 0.5, fixed = TRUE,  col_var = rep(c("Group 1", "Group 2"), 16), col_lab = "Groups")

