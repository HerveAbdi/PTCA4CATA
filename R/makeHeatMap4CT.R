#=====================================================================
# A function to create a HeatMap for a contingency table
#=====================================================================
# First a function to make a ggplot heatmap#
# Function makeHeatMap4CT
#' makeHeatMap4CT makes a ggplot2 HeatMap representation of
#' a contingency table (i.e. CATA data)
#'
#' \code{makeHeatMap4CT} makes a ggplot2 HeatMap representation of
#' a contingency table (e.g., CATA data).
#' makeHeatMap4CT assumes the contingency table
#' contains only non-negative numbers.
#' @param aContingencyTable a contingency table to be plotted
#' @param colorAttributes color for the names of the columns.
#' Can be one value or a vector. If NULL (default), colors
#' are chosen using \code{prettyGraphs::prettyGraphsColorSelection()}
#' @param angle.x (defalt = 70)
#' The slope when writing the name of the variables
#' @param colorProducts color for the names of the rows.
#' Can be one value or a vector. if NULL use dark grey.
#' @param  fontSize.x (default = 10), font size for the name
#' of the variables
#' @param  fontSize.y (default = 20), font size for the name
#' of the products
#' @return a ggplot2 heatmap
#' @import ggplot2
#' @importFrom reshape melt
#' @importFrom prettyGraphs prettyGraphsColorSelection
#' prettyGraphsColors
#' @author Herve Abdi
#' @examples
#' \dontrun{
#' aHeatMap <- makeHeatMap4CT(someContingencyTable)
#' # where \code{someContingencyTable} is a contingency table
#' }
#' @export
makeHeatMap4CT <- function(aContingencyTable,
                           colorAttributes = NULL,
                           colorProducts = NULL,
                           angle.x = 70,
                           fontSize.x = 10,
                           fontSize.y = 20
){
  if (is.null(colorAttributes)){colorAttributes = 'gray40'}
  if (is.null(colorProducts)){
    colorProducts = prettyGraphs::prettyGraphsColorSelection(
      NROW(aContingencyTable)
    ) }
  # To appease CMD Checl that gives the error message
  # no visible binding for global variable ‘Descriptors'
  Products <- Descriptors <- value <- NULL
  CT.m <- reshape::melt(as.matrix(aContingencyTable))
  colnames(CT.m) <- c("Products","Descriptors","value")
  ggplot2HeatMap.2 <- ggplot(CT.m, aes(Descriptors,
                                       Products)) +
    geom_tile(aes(fill = value),
              colour = "white") + scale_fill_gradient(low = gray(1),
                                                      high = gray(0)) +
    theme_bw() +
    theme(axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    theme(
      axis.text.x = element_text(angle = angle.x, hjust = 1,
                                 color = colorAttributes,
                                 size = 10),
      axis.text.y = element_text(color = colorProducts,
                                 size = 20,face = 'bold'),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )  # + coord_fixed(ratio = 4)
  return(ggplot2HeatMap.2)
}
#=====================================================================
# End of function
#=====================================================================
