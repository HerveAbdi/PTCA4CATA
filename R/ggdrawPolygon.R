# ggdrawPolygon
# Draw a polygon with ggplot
# useful when drawing the simplex in CA
#_____________________________________________________________________
# Helper for roxygen2 ----
#  install.packages('sinew')
#  sinew::makeOxygen(ggdrawPolygon)
#
#_____________________________________________________________________


#_____________________________________________________________________
# Draw a polygon
# A ggplot routine to draw a polygon
#_____________________________________________________________________
# A routine to draw a Polygon for ggplot2
#' @title Draw a Polygon in a \code{ggplot2} based graph
#' @description  \code{ggdrawPolygon}: Draws a Polygon in a
#' \code{ggplot2}-based factorial graph (e.g., from CA or PCA).
#' @param F a set of 2-dimensional coordinates
#' @param order2draw (Default: \code{1:nrow(F)}) the order of the
#' points to draw the polygon.
#' @param color  (Default: 'darkorchid') the color of the line
#' of the polygon.
#' @param linetype (Default: \code{3}, i.e., dotted line),
#' the line type to draw the polygon.
#' @param size (Default: \code{.5}) the size of the line.
#' @param fill (Default: \code{'darkorchid'}) the color to fill-in
#' the polygon.
#' @param alpha (Default: \code{.2}) the transparency fa ctor
#' for the fill-in color of the polygon (gors from 0 completely
#' transparent to 1: opaque).
#' @param ... other stuff to be passed to \code{ggplot2::geom_polygon}
#' if needed.
#' @return a polygon to be added to a \code{ggplots2} image.
#' @details \code{ggdrawPolygon} is mostly a wraper
#' around \code{ggplot2::geom_polygon}.
#' @author Hervé Abdi
#' @seealso
#'  \code{\link[ggplot2]{geom_polygon}}
#' @rdname ggdrawPolygon
#' @export
#' @importFrom ggplot2 geom_polygon
ggdrawPolygon <- function(F,
                          order2draw = 1:nrow(F), # default
                          color = 'darkorchid',
                          linetype = 3,
                          size   = .5,
                          fill = 'darkorchid',
                          alpha = .2,
                          ...){# begin function
  F2draw <- data.frame( F[c(order2draw,order2draw[1]),],
                        row.names = NULL)
  # Use ggplot geom_polygon
  thePolygon <- ggplot2::geom_polygon(data = F2draw,
                                      linetype = linetype,
                                      linewidth = size,
                                      aes_string(colnames(F2draw)[1],
                                                 colnames(F2draw)[2]),
                                      color = color,
                                      alpha = alpha,
                                      fill = fill,...)

  return(thePolygon)
}  # End of the function ggdrawPolygon
#_____________________________________________________________________
