readership = matrix(c(5,18,19,12,3,7,46,29,40,7,2,20,39,49,16), 5,
    dimnames = list(
    "Level of Education" = c("Some primary", "Primary completed", "Some secondary", "Secondary completed", "Some tertiary"),
    "Category of Readership" = c("Glance", "Fairly thorough", "Very thorough")))
devtools::use_data(readership, internal = FALSE, overwrite = TRUE)

library(ca)
ca(readership)
summary(ca(readership))

"Symmetric", "Row Principal", "Column Principal",

scaleCorrespondenceAnalysis(ca(readership))

My paper  = ca package = SPSS
Standard Coordinates = Standard Output of Ca
Principal CA = "symmetric" = SPSS "Principal"
             =  "rowprincipal" = "Row Principal"


scaleCorrespondenceAnalysis <- function(obj,
    map = c("symmetric", "rowprincipal", "colprincipal", "symbiplot", "rowgab", "colgab", "rowgreen", "colgreen"))
{   # Modified from ca::plot.ca.
    K <- dim(obj$rowcoord)[2]
    I <- dim(obj$rowcoord)[1]
    J <- dim(obj$colcoord)[1]
    svF <- matrix(rep(obj$sv[1:K], I), I, K, byrow = TRUE)
    svG <- matrix(rep(obj$sv[1:K], J), J, K, byrow = TRUE)
    rpc <- obj$rowcoord * svF
    cpc <- obj$colcoord * svG
    symrpc <- obj$rowcoord * sqrt(svF)
    symcpc <- obj$colcoord * sqrt(svG)
    mt <- c("symmetric", "rowprincipal", "colprincipal", "symbiplot",
        "rowgab", "colgab", "rowgreen", "colgreen")
    mti <- 1:length(mt)
    mtlut <- list(symmetric = list(x = rpc, y = cpc), rowprincipal = list(x = rpc,
        y = obj$colcoord), colprincipal = list(x = obj$rowcoord,
        y = cpc), symbiplot = list(x = symrpc, y = symcpc), rowgab = list(x = rpc,
        y = obj$colcoord * obj$colmass), colgab = list(x = obj$rowcoord *
        obj$rowmass, y = cpc), rowgreen = list(x = rpc, y = obj$colcoord *
        sqrt(obj$colmass)), rowgreen = list(x = obj$rowcoord *
        sqrt(obj$rowmass), y = cpc))
    x <- mtlut[[mti[mt == map]]][[1]]
    y <- mtlut[[mti[mt == map]]][[2]]
list(y, x)}

scaleCorrespondenceAnalysis(ca(readership), map = "symmetric")
scaleCorrespondenceAnalysis(ca(readership), map = "rowprincipal")
scaleCorrespondenceAnalysis(ca(readership), map = "colprincipal")
scaleCorrespondenceAnalysis(ca(readership), map = "symbiplot")

"Principal", Row Principal, Column Principal, Symmetrical (½), None

caNormalization <- function(ca.object, method = "Principal")
{
    .normalize = function(coords, power) sweep(coords[,1:2], 2, ca.object$sv[1:2]^power, "*")
    rows <- .normalize(ca.object$rowcoord, switch(method,
        "Principal" = 1, "Row principal" = 1, "Column principal" = 0, "Symmetrical (½)" = 0.5, "None" = 0))
    columns <- .normalize(ca.object$colcoord, switch(method,
        "Principal" = 1, "Row principal" = 0, "Column principal" = 1, "Symmetrical (½)" = 0.5, "None" = 0))
    list(rows, columns)
}
z = ca(readership)
caNormalization(z, "Principal")
caNormalization(z, "Column principal")
caNormalization(z, "Row principal")
caNormalization(z, "Symmetrical (½)")
