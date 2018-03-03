# A short function to compute the constraints
# for a brick of bootstrapped factor scores
#



#' Computes the x- and y-axes constraints
#' for all \code{prettyGraphs}  \code{PTCA4CATA} plotting functions
#' that display bootstrapped based confidence, tolerance, or
#' prediction intervals
#'
#' \code{minmaxHelper4Brick} Computes the x- and y-axes constraints
#' for all \code{prettyGraphs} and \code{PTCA4CATA} plotting functions
#' that displays bootstrapped based confidence, tolerance or
#' prediction intervals.  \code{minmaxHelper4Brick} transforms the
#' brick into a matrix and calls  \code{prettyGraphs::minmaxHelper}.
#'  \code{minmaxHelper4Brick} is typically used to get
#'  the dimensions for  \code{PTCA4CATA} graphics that use
#'  \code{MakeCIEllipses} or for  \code{DistatisR}) graphs such as
#'  \code{DistatisR::GraphDistatisBoot}.
#' @param aBrick a brick of bootstrapped factor scores
#'  (e.g., from \code{Boot4PTCA}).
#' @param axis1 the number of the factor for the x-axis (default = 1)
#' @param axis2 the number of the factor for the y-axis (default = 2)
#' @param expandFactor  expansion factor, if larger than 1
#' this is an expansion, equal to 1 (default) this does nothing,
#' smaller
#' than 1 this is a shrinkage.
#' @param trimPercent (default = 0) the proportion of extreme
#' values (small and large) to eiliminate.
#' @param ... other arguments that could be passed to
#' \code{prettyGraphs::minmaxHelper}.
#' @return
#' minMaxList	A list with the following values:
#' \code{minx, miny, maxx, maxy}.
#' @author Herve Abdi and Derek Beaton
#' @export
#' @importFrom prettyGraphs minmaxHelper

minmaxHelper4Brick <-function(aBrick,
                              axis1 = 1,
                              axis2 = 2,
                              expandFactor = 1,
                              trimPercent = 0,...){
  if ((trimPercent < 0 ) | (trimPercent > .45)) trimPercent = 0
  toto <- aperm(aBrick[,c(axis1,axis2),], c(1,3,2))
  dim(toto) <-  c(dim(aBrick)[[1]]*dim(aBrick)[[3]], dim(aBrick)[[2]] )
  if (trimPercent != 0){
    toto <- apply(toto,2,sort)
    nr <- nrow(toto)
    toto <- toto[ round(nr*trimPercent) : round(nr*(1-trimPercent)), ]
  }
  return(lapply(prettyGraphs::minmaxHelper(toto,
                             axis1 = axis1,
                             axis2 = axis2,
                             ...),'*',expandFactor))
}
