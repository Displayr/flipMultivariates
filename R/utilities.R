rowNamesOrNames <- function(x)
{
    if (is.matrix(x))
        return(rownames(x))
    names(x)
}

#' @importFrom flipFormat FormatAsPercent
correctPredictionsText <- function(overall.accuracy, group.labels, group.accuracies, digits = 4)
{
    paste0("Correct predictions: ", FormatAsPercent(overall.accuracy, digits), " (",
           paste0(group.labels, ": " , FormatAsPercent(group.accuracies, digits), collapse = "; "),
           ")" )
}
