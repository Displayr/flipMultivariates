#' \code{CorrespondenceAnalysis}
#' @description Removes rows or columns from the table.
#' @param x A table.
#' @param normalization The method used to normalize the coordinates of the
#'   correspondence analysis plot (this changes the plot, but not the outputs of
#'   \code{\link[ca]{ca}} itself. The default method is \code{"Principal"},
#'   which plots the principal coordinates (i.e., the standard coordinates
#'   multiplied by the singular values). \code{"Row principal"} and \code{"Column
#'   principal"} plot the standard coordinates of the columns (rows) against the
#'   principal coordinates. Note that the plotting occurs via
#'   \code{\link{print.CorrespondenceAnalysis}}.
#' @param interactive If \code{true}, an
#'   \code{\link[flipPlots]{InteractiveLabeledScatterPlot}} is plotted.
#'   Otherwise, a \code{\link[flipPlots]{LabeledScatterPlot}} is plotted.
#' @param row.names.to.remove A vector of the row labels to remove.
#' @param column.names.to.remove A vector of the column labels to remove.
#'   variable is provided, any cases with missing values on this variable are
#'   excluded from the final data file.
#' @param ... Optional arguments for \code{\link[ca]{ca}}.
#' @export
CorrespondenceAnalysis = function(x,
                                  normalization = "Principal",
                                  interactive = FALSE,
                                  row.names.to.remove = c("NET", "Total", "SUM"),
                                  column.names.to.remove = c("NET", "Total", "SUM"),
                                  ...)
{
    x <- flipU::GetTidyTwoDimensionalArray(x, row.names.to.remove, column.names.to.remove)
    x.ca <- ca::ca(x, ...)
    x.ca$interactive <- interactive
    class(x.ca) <- c("CorrespondenceAnalysis", class(x.ca))
    x.ca$normalization <- normalization
    x.ca$x <- x
    x.ca
}


#' \code{print.CorrespondenceAnalysis}
#' @description Creates a plot displaying the correspondence analysis results.
#' @param x CorrespondenceAnalysis object.
#' @param ... further arguments passed to or from other methods.
#' @export
print.CorrespondenceAnalysis <- function(x, ...)
{
    CorrespondenceAnalysis.object <- x
    normed <- CANormalization(CorrespondenceAnalysis.object, CorrespondenceAnalysis.object$normalization)
    singular.values <- round(CorrespondenceAnalysis.object$sv^2, 6)
    variance.explained <- paste(as.character(round(100 * prop.table(singular.values), 1)), "%", sep = "")[1:2]
    column.labels <- paste("Dimension", 1:2, paste0("(", variance.explained, ")"))
    row.coordinates <- normed$row.coordinates
    column.coordinates <- normed$column.coordinates
    coords <- rbind(row.coordinates, column.coordinates)
    x <- CorrespondenceAnalysis.object$x
    group.names <- names(dimnames(x))
    if (is.null(group.names))
        group.names <- c("Rows", "Columns")
    groups <- rep(group.names, c(nrow(row.coordinates), nrow(column.coordinates)))
    if (CorrespondenceAnalysis.object$interactive)
    {
        tooltip.text <- c(flipPlots::CreateInteractiveScatterplotTooltips(x), flipPlots::CreateInteractiveScatterplotTooltips(t(x)))
        print(flipPlots::InteractiveLabeledScatterPlot(coords, column.labels = column.labels, group = groups, fixed.aspect = TRUE, tooltip.text = tooltip.text))
    }
    else
        print(flipPlots::LabeledScatterPlot(coords, column.labels = column.labels, fixed.aspect = TRUE, group = groups))
}


#' \code{CANormalization}
#' @description Normalizes the coordinates of a \code{\link[ca]{ca}} object.
#' @param ca.object The object to normalize.
#' @param normalization The method used to normalize the coordinates of the
#'   correspondence analysis plot (this changes the plot, but not the outputs of
#'   \code{\link[ca]{ca}} itself. The default method is \code{"Principal"},
#'   which plots the principal coordinates (i.e., the standard coordinates
#'   multipled by the singular values). \code{"Row principal"} and \code{"Column
#'   principal"} plot the standard coordinates of the columns (rows) against the
#'   principal coordinates. \code{"Symmetrical (\u00BD)"} plots the standard
#'   coordinates multiplied by the square root of the singular values.
#'   \code{"None"} plots the standard coordinates.
#' @export
CANormalization <- function(ca.object, normalization = "Principal")
{
    .normalize = function(coords, power) sweep(coords[,1:2], 2, ca.object$sv[1:2]^power, "*")
    rows <- .normalize(ca.object$rowcoord, switch(normalization,
        "Principal" = 1, "Row principal" = 1, "Column principal" = 0, "Symmetrical (\u00BD)" = 0.5, "None" = 0))
    columns <- .normalize(ca.object$colcoord, switch(normalization,
        "Principal" = 1, "Row principal" = 0, "Column principal" = 1, "Symmetrical (\u00BD)" = 0.5, "None" = 0))
    list(row.coordinates = rows, column.coordinates = columns)
}


