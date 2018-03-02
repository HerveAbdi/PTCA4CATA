# CI-Ellipse for PTCA4CATA
# Create a CI- Ellipse Plot for a CA plot
# Should work with the Asymmetric plots
#====================================================================
#====================================================================
#' \code{MakeCIEllipses}.
#' Add Confidence interval ellipses to a CA-like plots.
#'
#' \code{MakeCIEllipses}: Create the Confidence Intervals (CI) ellipses
#' for plots for the I or J sets of a CA or STATIS type of analysis
#' The results of \code{MakeCIEllipses} should be added to BaseMap
#' created, for example, by the function \code{CreateBaseMap()}
#'
#' @param data A cube of bootstraped factor scores
#' should be an I Items * K factors * nIter array
#' (e.g. as given by \code{epCA.inference.battery})
#' with: I number of items,
#'       K number of factors (at least 2)
#'       nIter number of Bootstrap samples
#' @param axis1 (default = 1): the horizontal axis
#' @param axis2 (default = 2): the vertical axise
#' @param names.of.factors
#' (default is \code{paste0('Dimension ',c(axis1,axis2)}),
#' names of the factors, needs to be a 2-components vector.
#'  if NULL \code{name.of.factors}
#'  is \code{dimnames(data)[2]}. This argument is
#'  Needed to avoid conflict when plotting
#'  these names need to be the same as the names of the
#'  data used to make the BaseMap plot (i.e., Fi/Fj/Fij).
#' @param col (default = \code{NULL})
#' a string or a vector of strings with color names
#' if \code{NULL} use \code{prettyGraphs} scheme with
#' \code{prettyGraphs::prettyGraphsColorSelection}
#' @param  centers (default = \code{NULL}):
#' if \code{NULL} centers the ellipses on their barycenter
#' to center on another canters (i.e., item factor scores)
#' provide an \eqn{I * K} data frame or matrix
#' @param  line.size (default = 1)
#' thickness of the line for the ellipses
#' @param line.type (default = 1)
#' the type of line for the ellipses
#' @param   alpha.ellipse (default = .3)
#' alpha value (i.e., transparency) for the ellipses
#' @param alpha.line    (default = .5):
#' alpha value (i.e., transparency) for the lines
#' @param   p.level (default = .95),
#' \eqn{p}-value for the CI
#' @return LeGraph.elli the ellipses to be added to main plot
#' [e.g., as created by \code{\code{CreateBaseMap()}].
#' @author Herve Abdi
#' @import ggplot2  prettyGraphs
#' @examples
#' \dontrun{
#' Fij = rbind(Fi,Fj) # Fi and Fj being factor scores say from ExPosition
#' ABaseMap     <- CreateBaseMap(Fij)
#' # Boot_I is a Bootstrap cube for the I-set
#' GraphElli <- MakeCIEllipses(Boot_I)
#' ABaseMap + GraphElli # plot the I Set CI ellipses
#' }
#' @export
#====================================================================
# function MakeCIEllipses.
MakeCIEllipses <- function(data, # A cube of Bootstrap from Boot4PTCA
                           # I * #factor * nBooistrapIterations
                           axis1 = 1, axis2 = 2, # Axes to plots
                           names.of.factors =
                             paste0('Dimension ',c(axis1,axis2)), #
                           # Needed to avoid conflict when plotting
                           # these names need to be the same as Fi/Fj/Fij
                           col = NULL,
                           # The colors for the ellipses
                           centers = NULL,  # The centers of the ellipses
                           # if null use the boostrap means
                           # should be a I * # factors
                           line.size = 1,
                           line.type = 1,
                           alpha.ellipse = .3,
                           alpha.line    = .5,
                           p.level = .95
){
  Nom2Rows  <- unlist(dimnames(data)[1])
  X <-  aperm(data,c(1,3,2)) # Flaten the cube
  if (is.null(names.of.factors)){
    names.of.factors = unlist(dimnames(data)[2])
  }
  # rm(data)  # Not needed any more
  DimBoot <- dim(data)
  dim(X) <- c(DimBoot[1]*DimBoot[3],DimBoot[2])
  rownames(X) <- rep(Nom2Rows, DimBoot[3] )
  # We need that to be compatible for ggplots2
  colnames(X) <- names.of.factors
  nItems = DimBoot[1]
  if (is.null(col)){items.colors <-
    prettyGraphs::prettyGraphsColorSelection(nItems)
  } else {items.colors <- col}
  if(length(items.colors) == 1){items.colors= rep(items.colors,nItems)}
  if(length(items.colors) != nItems){items.colors= rep(items.colors[1],nItems)}
  LeGraph.elli <- list() # initialize
  for (i in  1:nItems){
    X2plot <- as.data.frame(X[row.names(X)==Nom2Rows[i],])
    if (!is.null(centers)){
      # Stuff to finish. HA /02/01/2016
      # Recenter the ellipses on specific centers
      truc <- as.data.frame(sweep(sweep( as.matrix(X2plot),
                                         2, colMeans(X2plot) ), 2,
                                  -as.matrix(centers[i,])))
      X2plot <- truc # recentered
    }
    #df_ell <- data.frame()

    elli <- ggplot2::stat_ellipse(data = X2plot[,c(axis1,axis2)],
                         ggplot2::aes(color=alpha(items.colors[i],alpha.line )),
                         show.legend = FALSE, geom = 'polygon',# center = c(0,0),
                         fill = ggplot2::alpha(items.colors[i],
                                               alpha.ellipse),
                         type = 't',level = p.level,color = items.colors[i],
                         size=line.size, linetype=line.type)
    LeGraph.elli[[i]] <-  elli
  }
  return(LeGraph.elli)
} # End of function MakeCIEllipses
#====================================================================
