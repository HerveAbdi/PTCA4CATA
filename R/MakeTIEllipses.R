# Make Tolerance Ellipses / Hull
# Hervé Abdi
# September 22, 2017
# Current Version: September 30 2017.
# For inclusion in PTCA4CATA
#
#=====================================================================
#====================================================================
# function MakeToleranceIntervals
#====================================================================
# function MakeToleranceIntervals
#' @title \code{MakeToleranceIntervals}.
#' Add Tolerance interval hulls or
#' ellipses to factor score plots
#' (e.g., PCA, CA).
#'
#' @description  \code{MakeToleranceIntervals}:
#'  Creates the ToleranceIntervals (CI) convex hulls or ellipses
#' for plots for the I or J sets of a CA, PCA,  MFA,
#' STATIS, etc. type of analysis
#' The results (Hull or Ellipses)
#' of \code{MakeToleranceIntervals}
#' should be added
#' to the BaseMap
#' created (for example) by the function
#'  \code{CreateBaseMap()}.
#' @param data A set of I*L factor scores
#' (as obtained, for example, from \code{ExPosition::epCA})
#' @param design an I by 1 factor giving the group membership
#' of the Ith observations.
#' @param axis1 = 1 the horizontal axis
#' @param axis2 = 2 the vertical axise
#' @param names.of.factors = 'Dimension',
#' names of the factors.
#'  if NULL   name.of.factors is dimnames(data)[2],
#'  if dimnames(data)[2] is NULL, the dimensions
#'  will be labelled "Dimension". This paramter is needed
#'  to avoid conflict when plotting
#'  these names as they must be the same as the names of the
#'  data used to make the BaseMap plot (i.e., Fi/Fj/Fij)
#' @param col = NULL
#' a string or a vector of strings with color names
#' if NULL use prettyGraphs scheme with
#' \code{prettyGraphs::prettyGraphsColorSelection}
#' @param  centers = NULL
#' if NULL centers the ellipses on their barycenter
#' to center on another center (i.e., item factor scores)
#' provide an I * K data frame or matrix.
#' @param  line.size = 1
#' thickness of the line for the ellipses
#' @param line.type = 1
#' the type of line for the ellipses
#' @param   alpha.ellipse = .3
#' alpha value (transparency) for the ellipses.
#' @param alpha.line    = .5,
#' alpha value (transparency) for the lines.
#' @param   p.level = .66
#' p value for the TI
#' @param type (Default = \code{'hull'})
#' type of interval can be c('ellipse','hull')
#' @return LeGraph.elli a graph with convex hulls
#' or ellipse to be added to the base map as created
#' by the function  \code{CreateBaseMap()}.
#' @author Herve Abdi
#' @import ggplot2  prettyGraphs
#' @examples
#' \dontrun{
#' Fij = rbind(Fi,Fj)
#' # Fi and Fj being factor scores say from ExPosition
#' ABaseMap <- CreateBaseMap(Fij)
#' # Here design.matrix is design matrix describing K groups in Fi
#' GraphHull <- MakeToleranceIntervals(Fi,design.matrix)
#' print(ABaseMap + GraphHull)  # plot the K TI ellipses
#' }
#' @rdname MakeToleranceIntervals
#' @export
MakeToleranceIntervals <- function(data, # A set of Factor Scores
                           design, # A design factor
                           # I * #factor * nBooistrapIterations
                           axis1 = 1, axis2 = 2, # Axes to plots
                           names.of.factors = 'Dimension',
                           # colnames(Fij),
                           # Needed to avoid conflict when plotting
                           # these names need to be the same
                           #       as Fi/Fj/Fij
                           col = NULL,
                           # The colors for the ellipses
                           centers = NULL,
                           # The centers of the ellipses
                           # if null use the boostrap means
                           # should be a I * # factors
                           line.size = 1,
                           line.type = 1,
                           alpha.ellipse = .3,
                           alpha.line    = .5,
                           p.level = .95,
                           type = 'hull' # 'hull' or 'ellipse'
){ design   <- factor(design)
  Nom2Rows  <- levels(design)
  X <-  data[,c(axis1,axis2)]
  if(length(design) != NROW(X)){
    stop('Length of Design should be equal to nrow(Data)')
  }
  if (is.null(names.of.factors)){
    names.of.factors = unlist(dimnames(X)[2])
  }
  if (is.null(names.of.factors)){
    names.of.factors = paste0('Dimension', c(axis1,axis2))
  }
  # rm(data)  # Not needed any more
  # DimBoot <- dim(data)
  # dim(X) <- c(DimBoot[1]*DimBoot[3],DimBoot[2])
  # rownames(X) <- rep(Nom2Rows, DimBoot[3] )
  # We need that to be compatible for ggplots2
  colnames(X) <- names.of.factors
  nItems = length(Nom2Rows)
  if (is.null(col)){items.colors <-
    prettyGraphs::prettyGraphsColorSelection(nItems)
  } else {items.colors <- col}
  if(length(items.colors) == 1){items.colors =
                                    rep(items.colors,nItems)}
  if(length(items.colors) != nItems){items.colors =
                                      rep(items.colors[1],nItems)}
  LeGraph.elli <- list() # initialize
  for (i in  1:nItems){
    X2plot <- as.data.frame(X[design==Nom2Rows[i],])
    if (!is.null(centers)){
      # Stuff to finish. HA /02/01/2016
      # Recenter the ellipses on specific centers
      truc <- as.data.frame(sweep(sweep( as.matrix(X2plot),
                                    2, colMeans(X2plot) ), 2,
                                    -as.matrix(centers[i,])))
      X2plot <- truc # recentered
    }
    #df_ell <- data.frame()
    if (tolower(type) == 'ellipse'){# Plot ellipses statellipse
    elli <- ggplot2::stat_ellipse(data = X2plot[,c(axis1,axis2)],
                          ggplot2::aes(color=alpha(
                                  items.colors[i],alpha.line )),
                          show.legend = FALSE,
                          geom = 'polygon',# center = c(0,0),
                              fill = ggplot2::alpha(items.colors[i],
                                              alpha.ellipse),
                          type = 't',
                          na.rm = TRUE,
                          level = p.level,
                          color = items.colors[i],
                                  size=line.size,
                          linetype=line.type)
    } else {# Plot convex hulls with ggConvexHull
      row2keep <-  complete.cases(X2plot[,c(axis1,axis2)])
      X.non.na <- X2plot[row2keep,c(axis1,axis2)]
      elli <- ggConvexHull(data = X.non.na,
                           x_axis = 1,
                           y_axis = 2,
                           percentage = p.level,
                           col.line = items.colors[i],
                           alpha.line = alpha.line,
                           line.size = line.size,
                           col.hull = items.colors[i],
                           alpha.hull = alpha.ellipse,
                           names.of.factors = names.of.factors
                            )
    }
    LeGraph.elli[[i]] <-  elli
  }
  return(LeGraph.elli)
} # End of function MakeToleranceIntervals
#=====================================================================
#
#=====================================================================
#' @title Create a peeled convex hull for a set of points
#' described by 2 variables.
#'
#' @description \code{peelZeHull}
#' computes a peeled convex hull for a set of points
#' described by 2 variables. Function taken from
#' the function \code{ExPosition::peeledHull} which, in turn,
#' got its inspiration from Michael Greenacre's package \code{CA}.
#' The original code can be found at
#' \url{http://carme-n.org/?sec=code2}.
#' @param data_matrix an item by variables data,frame or matrix.
#' @param x_axis column number for the first variable.
#' \code{Default = 1}
#' @param y_axis column number for the second variable.
#' \code{Default = 2}
#' @param percentage proportion of the
#' convexHull to keep, Default: .66
#' @references for a reference
#' for the code for peeling the
#' convex hull see:  Greenacre, M. J. (2007).
#' Correspondence Analysis in Practice. Chapman and Hall.
#' @return a dataframe with
#'  the coordinates of the peeled convex hull.
#' @author Herve Abdi
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname peelZeHull
#' @export
peelZeHull <- function (data_matrix,
                        x_axis = 1,
                        y_axis = 2,
                        percentage = .66){
  nsim <- length(data_matrix[, x_axis])
  data_matrix <- data_matrix[, c(x_axis, y_axis)]
  repeat {
    hpts <- chull(data_matrix)
    npts <- nrow(data_matrix[-hpts,])
    if ((npts/nsim < percentage) || is.null(npts)) {
      break
    }
    data_matrix <- data_matrix[-hpts, ]
  }
  zeVertices <- chull(data_matrix)
  peeledHull <- as.data.frame(data_matrix[zeVertices,])
  return(peeledHull)
}

# End of function PeelzeHull
#---------------------------------------------------------------------
# function ggConvexHull. Draw a convex Huell with ggplot
#
#---------------------------------------------------------------------
#' @title ggConvexHull use ggplot2 to plot
#' a peeled convex hull for a set of points
#' described by 2 variables.
#'
#' @description \code{ggConvexHull}
#' computes a peeled convex hull for a set of points
#' described by 2 variables and plot it.
#' Uses function \code{PeelZeHull}
#' derived from
#' the function \code{ExPosition::peeledHull} which, in turn,
#' got inspiration from Michael Greenacre package \code{CA}.
#' The original code can be found at
#' \url{http://carme-n.org/?sec=code2}.
#' @param data an item by variables data,frame or matrix.
#' @param x_axis column number for the first variable.
#' \code{Default = 1}
#' @param y_axis column number for the second variable.
#' \code{Default = 2}
#' @param percentage proportion of the
#' convexHull to keep, Default: .66
#' @param col.line = the color of the line for the hill.
#' Default is 'darkorchid4'.
#' @param alpha.line  = 1,
#' alpha value (transparency) for the lines,
#' takes values between 0 (completely transparent) and
#' 1 (no transparent).
#' @param  line.size = 1,
#' thickness of the line for the ellipses.
#' @param  line.type = 1 (solid line)
#' the type of line (values from 0 to 6).
#' @param col.hull  =  'darkorchid',
#' the color to fill the hull.
#' @param  alpha.hull = .4
#' alpha value (transparency) for the hull.
#' @param names.of.factors = "Dimension",
#' names of the factors.
#'  if NULL   name.of.factors is dimnames(data)[2],
#'  if dimnames(data)[2] is NULL, the dimensions
#'  will be labelled "Dimension".
#'  This parameter is
#'  needed to avoid conflict when plotting
#'  these names as they need to be the same as the names of the
#'  data used to make the BaseMap plot (i.e., Fi/Fj/Fij).
#' @author Herve Abdi
#' @return a ggplot2 object
#'  to draw a convex hull
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @rdname ggConvexHull
#'
ggConvexHull <- function(data,
                          x_axis = 1,
                          y_axis = 2,
                          percentage = .66,
                          col.line = 'darkorchid4',
                          alpha.line    = 1,
                          # alpha value (transparency) for the lines.
                          line.size = 1,
                          # thickness of the line for the Hull
                          line.type = 1,
                          # the type of line for the Hull
                          col.hull  =  'darkorchid',
                          alpha.hull = .4,
                          # alpha value (transparency) for the Hull.
                          names.of.factors = 'Dimension'
                          # names of the factors:  needed by ggplot
){# Start ggConvexHull
  X <-  data[,c(x_axis,y_axis)]
  if (is.null(names.of.factors)){
    names.of.factors = unlist(dimnames(X)[2])
  }
  if (is.null(names.of.factors)){
    names.of.factors = paste0('Dimension', c(1,2))
  }
  peeledHull <- peelZeHull(X, percentage =  percentage)
  ggHull     <-  ggplot2::geom_polygon(data = peeledHull,
                                linetype = line.type,
                                size = line.size,
                                aes_string(colnames(X)[1],
                                          colnames(X)[2]) ,
                                color = ggplot2::alpha(col.line,
                                         alpha =  alpha.line),
                                alpha = alpha.hull,
                                fill = col.hull)  #+
  #geom_line( line.type = line.type,
  #            line.size = line.size)
  return(ggHull)
} # End of function ggConvexHull
#=====================================================================

