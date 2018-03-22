
#---------------------------------------------------------------------
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(addCircleOfCor)
#
#---------------------------------------------------------------------
#' @title Add a Circle of Correlation for a PCA-like map
#' of correlation produced by \code{createFactorMap()} et alia.
#' @description
#' Add a Circle of Correlation for a PCA-like map
#' of correlation produced by createFactorMap et alia.
#'
#' @param color  (Default: 'darkorchid') color for the circle.
#' @param alpha  (Default: 0.3) transparency for the circle,
#' should be between 0 (completely transparent) and 1
#' (no transparent).
#' @param size (Default: 1) thickness of the line of the circle.
#' @param center (Default: c(0, 0)) center of the circle.
#' @param radius (Default: 1) radius of the circle.
#' @param nPoints (Default: 100) the number of points used to
#' draw the circle.
#' @return nothing
#' @details The map should should be first created by, for example,
#' \code{createFactorMap()} (or equivalent functions from
#' \code{PTCA4CATA}), and then the circle of correlation is added
#' (see example).
#' @import ggplot2
#' @author Herve Abdi
#' @seealso createFactorMap
#' @examples
#' # Some PCA-like correlations
#' corXY <- matrix(c(.5,-.5, .1,.7, .8,.5, -.1,.9,  -.6,-.6),
#'                ncol = 2, byrow = TRUE )
#' # create a map of correlation
#' MapCor <- createFactorMap(corXY,
#'          constraints = list(minx = -1, miny = -1,
#'                             maxx = 1 , maxy = 1) )
#' # Add a circle to the base Map
#' ggMapWithCircle <- MapCor$zeMap  + addCircleOfCor()
#' # To print the map with the circle:
#' # print(ggMapWithCircle)
#' @rdname addCircleOfCor
#' @export
#=====================================================================
# Simple example here
# To be Moved to PTCA4CATA
#=====================================================================
addCircleOfCor <- function(color = 'darkorchid', # color of the circle
                           alpha = .3,
                           size  = 1,
                           center = c(0,0),
                           radius = 1,
                           nPoints = 100){
  # first an internal function
  #-------------------------------------------------------------------
  # begin circleFun here
  .circleFun <- function(center,
                        radius,
                        npoints){
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + radius * cos(tt)
    yy <- center[2] + radius * sin(tt)
    return(data.frame(x = xx, y = yy))
  } # end private circleFun
  #-------------------------------------------------------------------
  dat <- .circleFun(center, radius, npoints = nPoints)
  x <- y <- NULL # needed to appease the parser that
                 # thinks that x and y are glocal undefined variables
  aCircle <- geom_path(data = dat, mapping = aes(x = x, y = y),
                       color = color , alpha = alpha, size = size)
  return(aCircle)

} # end addCircleOfCor
#=====================================================================

