rowNamesOrNames <- function(x)
{
    if (is.matrix(x))
        return(rownames(x))
    names(x)
}
