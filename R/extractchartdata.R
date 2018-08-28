#' @importFrom flipFormat ExtractChartData
#' @export
flipFormat::ExtractChartData

#' @export
ExtractChartData.LDA <- function(x)
{
    return(ExtractChartData(x$confusion))
}

#' @export
ExtractChartData.SupportVectorMachine <- function(x)
{
    return(ExtractChartData(x$confusion))
}

#' @export
ExtractChartData.RandomForest <- function(x)
{
    return(ExtractChartData(x$confusion))
}

#' @export
ExtractChartData.GradientBoost <- function(x)
{
    return(ExtractChartData(x$confusion))
}

#' @export
ExtractChartData.DeepLearning <- function(x)
{
    return(ExtractChartData(x$confusion))
}
