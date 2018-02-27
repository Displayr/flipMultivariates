rowNamesOrNames <- function(x)
{
    if (is.matrix(x))
        return(rownames(x))
    names(x)
}

#' @importFrom flipFormat FormatAsPercent
correctPredictionsText <- function(overall.accuracy, group.labels, group.accuracies, digits = 4, out.of.bag = FALSE)
{
    prefix <- if (out.of.bag)
        "Correct predictions (based on out-of-bag sample): "
    else
        "Correct predictions: "

    paste0(prefix, FormatAsPercent(overall.accuracy, digits), " (",
           paste0(group.labels, ": " , FormatAsPercent(group.accuracies, digits), collapse = "; "),
           ")" )
}
