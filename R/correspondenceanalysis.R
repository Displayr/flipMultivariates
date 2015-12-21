#' \code{removeRowsAndOrColumns}
#' @description Removes rows or columns from the table.
#' @param x The data that is being analzyed.
#' @param row.names.to.remove A vector of the row labels to remove.
#' @param column.names.to.remove A vector of the column labels to remove.
#' variable is provided, any cases with missing values on this variable
#' are excluded from the final data file.
#' @param ... Optional arguments for \code{\link{mice}}.
removeRowsAndOrColumns = function(x,
        row.names.to.remove = c("NET", "Total", "SUM"),
        column.names.to.remove = c("NET", "Total", "SUM"))
{
    x[!rownames(x) %in% row.names.to.remove, !colnames(x) %in% column.names.to.remove ]
}

CorrespondenceAnalysis = function(x,
        normalization = "Principal",
        row.names.to.remove = c("NET", "Total", "SUM"),
        column.names.to.remove = c("NET", "Total", "SUM"),
        ...)
{
    x = removeRowsAndOrColumns(x)
    x.ca = ca::ca(x)
    require(scatterD3)
    scatterD3(x = c(x.ca$rowcoord[,1],x.ca$colcoord[,1]),
          y = c(x.ca$rowcoord[,2],x.ca$colcoord[,2]),
          lab = c(x.ca$rownames, x.ca$colnames),
          col_var = c(rep("Rows",length(x.ca$rownames)),
                               rep("Columns",length(x.ca$colnames))),
          xlab = "", ylab = "", col_lab = "",
          tooltip_text = createCaTooltips(x, x.ca$rownames, x.ca$colnames)
    )
}

`Correspondence analysis` = CorrespondenceAnalysis(QInputs(tableToAnalyse))
