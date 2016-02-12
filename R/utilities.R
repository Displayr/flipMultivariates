ifThen <- function(expression, a, b) flipU::IfThen(expression, a, b)

rowNamesOrNames <- function(x)
{
    if (is.matrix(x))
        return(rownames(x))
    names(x)
}
