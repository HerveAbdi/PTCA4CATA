#=====================================================================
# A function to create a ggplot HeatMap for a contingency table
#=====================================================================
# First a function to make a ggplot heatmap#
# Function makeHeatMap4CT
#' makeggHeatMap4CT makes a \code{ggplot2} HeatMap representation of
#' a contingency table (i.e., CATA data).
#'
#' \code{makeggHeatMap4CT} makes a \code{ggplot2}
#' HeatMap representation of
#' a contingency table (e.g.,
#'  CATA data).
#' makeHeatMap4CT assumes the contingency table
#' contains only non-negative numbers. The rows of the contingency
#' table are observations (for a CATA task: products), the columns
#' of the contingency
#' table are variables (for a CATA task: descriptor).
#' Note 1: as a ggplot2 object, the map can be customized
#' (e.g., title,
#' x and y labels) like any ggplot2 object.
#' Note 2: this function is still somewhat experimental
#' (i.e., \code{ggplot2} is not a "natural" for heatmap).
#' @param aContingencyTable a contingency table to be plotted
#' @param colorAttributes color for the names of the columns.
#' Can be one value or a vector. If NULL (default), colors
#' are chosen using \code{prettyGraphs::prettyGraphsColorSelection()}
#' @param angle.x (default = 70)
#' The slope when writing the name of the variables.
#' @param colorProducts color for the names of the rows.
#' Can be one value or a vector. if NULL use dark grey.
#' @param  fontSize.x (default = 10), font size for the name
#' of the variables.
#' @param face.x (default = 'plain'), the face for the names
#' of the variables.
#' @param  fontSize.y (default = 15), font size for the name
#' of the products.
#' @param face.y (default = 'bold'), the face for the names
#' of the products.
#' @return a ggplot2 heatmap
#' @import ggplot2
#' @importFrom reshape melt
#' @importFrom prettyGraphs prettyGraphsColorSelection
#' prettyGraphsColors
#' @author Herve Abdi
#' @examples
#' # Use the example from the colorOfMusic data set
#' data(colorOfMusic)
#' contingencyTable <- colorOfMusic$contingencyTable
#' aHeatMap <- makeggHeatMap4CT(contingencyTable,
#'              colorProducts = colorOfMusic$colorInformation[,2])
#'
#' @export
makeggHeatMap4CT <- function(aContingencyTable,
                           colorAttributes = NULL,
                           colorProducts = NULL,
                           angle.x = 70,
                           fontSize.x = 10,
                           face.x = 'plain',
                           fontSize.y = 15,
                           face.y = 'bold'
){
  if (is.null(colorAttributes)){colorAttributes = 'gray40'}
  if (is.null(colorProducts)){
    colorProducts = prettyGraphs::prettyGraphsColorSelection(
      NROW(aContingencyTable)
    ) }
  if (is.null(rownames(aContingencyTable))){
    rownames(aContingencyTable) <- as.character(1:nrow(aContingencyTable))
  }
  if (is.null(colnames(aContingencyTable))){
    colnames(aContingencyTable) <- as.character(1:ncol(aContingencyTable))
  }
  colorAttributes = as.character(colorAttributes)
  colorProducts = as.character(colorProducts)
  # To appease CMD Check that gives the error message
  # no visible binding for global variable ‘Descriptors'
  Products <- Descriptors <- value <- NULL
  CT.m <- reshape::melt(as.matrix(aContingencyTable))
  # Added 02/01/2018 to fix the oredeer of color problem
  #
  CT.m[,1] <- factor(CT.m[,1],levels = rev(rownames(aContingencyTable)))
  CT.m[,2] <- factor(CT.m[,2],levels = colnames(aContingencyTable))

  colnames(CT.m) <- c("Products","Descriptors","value")
  ggplot2HeatMap.2 <- ggplot(CT.m, aes(Descriptors,
                                       Products)) +
    geom_tile(aes(fill = value),
              colour = "white") +
              scale_fill_gradient(low = gray(1), high = gray(0)) +
              theme_bw() +
              theme(axis.line = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank()) +
              theme(
              axis.text.x = element_text(angle = angle.x, hjust = 1,
                                 color = colorAttributes,
                                 size = fontSize.x,
                                 face = face.x),
      axis.text.y = element_text(color = rev(as.character(colorProducts)),
                                 size = fontSize.y,
                                 face = face.y),
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
