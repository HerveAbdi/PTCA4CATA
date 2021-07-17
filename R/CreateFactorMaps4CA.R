# Preamble ----
#********************************************************************
#_____________________________________________________________________
# function for creating Factor Based Map
# createBaseMap
# createFactorMap
# map4DotsAndLabels
#
# Revision October/21/2017. in createFactorMap
# fix problem with axis different from 1 and 2
#

#_____________________________________________________________________
# function: createBaseMap
# createBaseMap ----
#_____________________________________________________________________
#' create a base map for CA type graphs with ggplot2
#'
#' \code{createBaseMap}: Create a ggplot2 basemap for CA type graphs.
#' The final maps are created by using overlays.
#' A map CA is created with first the baseMap and
#' then adding a text/dot map.
#' @seealso \code{\link{createFactorMapIJ}}
#' @param data the factor scores to plot
#' @param constraints a list with minx miny maxx maxy
#' typically obtained from  \code{prettyGraphs::minmaxHelper()}.
#' If \code{NULL}
#' (default) it  is computed with the function
#'  \code{prettyGraphs::minmaxHelper()}.
#' @param col.axes color for the axes, default is
#' \code{'darkorchid'}.
#' @param alpha.axes alpha parametere (transparency)
#' for the axes, default is .2.
#' @param width.axes the width of the axes, default is 1.1.
#' @param col.background the color theme of the background,
#' default is \code{adjustcolor('lavender', alpha.f = .2)}.
#' @param title a main title, default is \code{NULL} (no title)
#' @return a basemap
#' @import prettyGraphs ggplot2 grDevices
#' @export
#' @author Hervé Abdi
#' @section Important_Note: When creating multiple layers graphs,
#' because of the way \code{ggplot2} create graphs, all the
#' the matrices/dataframe should all the have the same column names
#' [e.g., \code{colnames()} equal to
#' \code{c("Dimension 1", "Dimension 2")}].
#' When it is not the case, some strange and cryptic
#' error may be produced
#' (e.g., \code{"cannot find Dimension"}).
#' @examples \dontrun{
#' aBaseMap <- createBaseMap(Fi)
#' # with Fi being a map of factor scores
#' }
createBaseMap <- function(data,
                          constraints = NULL,
                          col.axes = 'darkorchid',
                          alpha.axes = .2,
                          width.axes = 1.1,
                          col.background =
                             adjustcolor('lavender',
                                        alpha.f = .2),
                          title = NULL){
  if (is.null(col.background)){col.background = 'transparent'}
  data <- as.data.frame(data)
  if (is.null(constraints)) {
    # lapply(prettyGraphs::minmaxHelper(Fj[,3:4]),'*',1.1)
    constraints <- lapply(prettyGraphs::minmaxHelper(data),'*',1.1)
  }
  V1 = as.name(colnames(data)[1])
  V2 = as.name(colnames(data)[2])
  # Create a Background Map
  LeG_b <- ggplot(data = data,
                  # aes(data[,1],data[2])
                  aes_string(V1, V2)
  ) +
    coord_fixed(ratio = 1)
  LeG_b = LeG_b +
    xlim(constraints$minx,constraints$maxx) +
    ylim(constraints$miny,constraints$maxy)
  LeG_b <- LeG_b + geom_hline(yintercept = 0, size = width.axes,
                              color =  col.axes,alpha = alpha.axes) +
    geom_vline(xintercept = 0, size = width.axes ,
               color = col.axes,alpha = alpha.axes) +
    theme(panel.background = element_rect(fill = col.background,
                                          color = col.axes))
  if (!is.null(title))LeG_b <- LeG_b + ggtitle(title)
  return(LeG_b)
}
# End of function createBaseMap

#_____________________________________________________________________
# function: createFactorMap
#_____________________________________________________________________
# createFactorMap ----
# create the base plot for factor type plots
# gives a baseMap and one map for dots and one map for labels
#_____________________________________________________________________
#' @title  create the base plot maps for CA type
#' graphs with \code{ggplot2}.
#'
#' @description \code{createFactorMap}:
#' Creates the \code{ggplot2} basic factor maps for CA type graphs.
#' The final maps are created by using overlays on top of the base map.
#'
#' A map for CA is created with the baseMap to which
#' is added text/dot map.
#' \code{createFactorMap} calls the functions \code{map4DotsAndLabels}
#' and \code{createBaseMap}.
#'
#' @seealso  \code{\link{map4DotsAndLabels}}
#' \code{\link{createBaseMap}}
#' .
#' @param  X the factor scores to plot.
#' @param axis1 the column of X used for the horizontal axis
#' of the plot. Default = 1.
#' @param axis2 the column of X used for the vertical axis
#' of the plot. Default = 2.
#' @param  constraints a list with \code{minx miny maxx maxy},
#' typically obtained from \code{ExPosition}. If \code{NULL}
#' (default) it is computed with the function
#'  \code{prettyGraphs::minmaxHelper()}.
#' @param title A title for the graph. Default is \code{NULL}.
#' @param  col.points the color of the points/dots.
#' Can be one color or a vector of colors. If a vector, it needs
#' to have exactly the number of items to be plotted.
#' Default = \code{'blueviolet'}.
#' @param alpha.points (default = .5), the alpha
#'  (transparency) for the points, should be
#'  between 1 (opaque) and 0
#'  (completely transparent).
#' @param   display.points  if  \code{TRUE} (Default)
#' create the map for the points.
#' @param pch the character for the points,
#' Default is 19 (circles).
#' @param  cex size of the dots. Default = 2.5
#' @param display.labels if \code{TRUE} (Default),
#' create the
#' map for the labels.
#' @param  col.labels the color of the labels
#' Can be one color or a vector of colors. If a vector it needs
#' to have exactly the number of items to be plotted.
#' Default =  \code{'darkorchid'}.
#' @param alpha.labels (default = 1), the alpha
#'  (transparency) for the points, should be
#'  between 1 (no transparency) and 0
#'  (completely transparent).
#' @param text.cex = 4, font size for labels.
#' @param font.face  (Default = \code{'bold'}) font for labels.
#' @param font.family (Default =  \code{'sans'})
#'   font family for labels.
#' @param col.axes color for the axes, default is
#' \code{'darkorchid'}.
#' @param alpha.axes alpha parameter (transparency)
#' for the axes, default is .2.
#' @param width.axes the width of the axes, default is 1.1.
#' @param col.background the color theme of the background,
#' default is \code{adjustcolor('lavender', alpha.f = .2)}.
#' @param force \code{(default = 1)}.
#' How much \code{ggrepel} repels the label
#' @param segment.size \code{(default = 0)}
#'  size of segment line for \code{ggrpel}.
#' @param ... stuff to be passed to other functions.
#' @return a list with 6 elements:
#' \enumerate{
#' \item \code{zeMap}: The Complete Map (background Dots and Labels);
#' \item  \code{zeMap_background}: The Background;
#' \item  \code{zeMap_dots:}: The dots;
#' \item  \code{zeMap_text:}: The Labels;
#' \item  \code{factorScores:}:  The factor scores; and
#' \item  \code{constraints:}: The list of the contraints'
#' }
#; NB class = \code{'createFactorMap'}.
#' @section Important_Note: When creating multiple layers graphs,
#' because of the way \code{ggplot2} create graphs, all
#' the matrices/dataframe should all the have the  same column names
#' [e.g., \code{colnames()} equal to
#' \code{c("Dimension 1", "Dimension 2")}].
#' When it is not the case, some strange and cryptic
#' error may be produced
#' (e.g., \code{"cannot find Dimension"}).
#' @import prettyGraphs grDevices
#' @export
# @examples \dontrun{}
#' @author Hervé Abdi
createFactorMap <- function(X,
                            axis1 = 1, axis2 = 2,
                            constraints = NULL,
                            # list with minx miny maxx maxy
                            # as obtained, e.g.,
                            # from minmacHelper from prettyGraphs
                            title = NULL,
                            col.points = 'blueviolet',
                            alpha.points = .5,
                            display.points = TRUE,
                            pch = 19, # Character for the dots
                            cex = 2.5, # size of the dots
                            display.labels = TRUE,
                            col.labels = 'darkorchid4',
                            alpha.labels = 1,
                            text.cex = 4, # font size for labels
                            font.face =   'bold', # font for labels
                            font.family = 'sans' ,
                            # font family for labels
                            col.axes = 'darkorchid',
                            # color for the axes
                            alpha.axes = .2,
                            # tranparency for the axes
                            width.axes = 1.1,
                            # width of the axes
                            col.background =
                              adjustcolor('lavender',
                                          alpha.f = .2),
                            force = 1, # for ggrepel
                            segment.size = 0, # size of segment line
                            # background color
                            ... # more stuff
){  #col.labels <- ggplot2::alpha(col.labels, alpha = alpha.labels)
    #col.points <- ggplot2::alpha(col.points, alpha = alpha.points)
  #
  if (is.null(title)){title = ''}
  #
  # give a name to the Dimension of Fi is there is none
  if (is.null(colnames(X))){
    colnames(X) <- paste0('Dimension ',1:ncol(X))
  }
  # G = as.data.frame(X[,c(axis1,axis2)])
  G = as.data.frame(X)
  #V1 = as.name(colnames(G)[1])
  #V2 = as.name(colnames(G)[2])
  # NB default uses  minmaxHelper from prettyPlots
  if (is.null(constraints)){
    constraints <-
      lapply(prettyGraphs::minmaxHelper(G[,c(axis1,axis2)]),'*',1.1)
  }
  #
  #___________________________________________________________________
  # NB geom_text_repel is from ggrepel
  LeG_b <- createBaseMap(data = G[,c(axis1,axis2)],
                         constraints = constraints,
                         col.axes = col.axes, #  'darkorchid',
                         alpha.axes = alpha.axes,
                         width.axes = width.axes,
                         col.background =   col.background,
                         title = title)

  LesG_dl <- map4DotsAndLabels(
    data = G[,c(axis1,axis2)],
    axis1 = 1,
    axis2 = 2,
    display.points = display.points,
    col.points = col.points,
    alpha.points = alpha.points,
    pch = pch,
    # Character for the points
    cex = cex,
    # size of the points
    display.labels = display.labels,
    col.labels = col.labels,
    alpha.labels = alpha.labels,
    text.cex = text.cex,
    # font size for labels
    font.face =   font.face, #'bold',
    # font for labels
    font.family = font.family ,'sans',
    # font family for labels
    # below are ggrepel values
    force = force, # labels' repulsion for ggrepel
    segment.size = segment.size,
    # segment width for ggrepel
    nudge_x = 0,
    nudge_y = 0 # from ggrepel
    # nudge value for starting point for
    #  labels (ggrepel)
    , ... # in case we need more stuff for ggrepel
  )


  LeG = LeG_b
  if (display.points){LeG = LeG + LesG_dl$leG.points}
  if (display.labels){LeG = LeG + LesG_dl$leG.labels}

  return.list <- structure(list(zeMap = LeG,
                                zeMap_background = LeG_b,
                                zeMap_dots = LesG_dl$leG.points,
                                zeMap_text = LesG_dl$leG.labels,
                          factorScores = G, constraints = constraints),
                           class = 'createFactorMap')


  return(return.list)
  #
} # End of Function
#_____________________________________________________________________

#' Change the print function for \code{createFactorMap}
#'
#'  Change the print function for objsects of the
#'  class \code{createFactorMap}
#'
#' @param x a list: output of \code{createFactorMap}
#' @param ... everything else for the functions
#' @author Herve Abdi
#' @export
print.createFactorMap <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\nBasic Factor Maps (with ggplot2) \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$zeMap             ", "A standard map with background, points, and Labels")
  cat("\n$zeMap_background  ", "The background map")
  cat("\n$zeMap_dots        ", "The points map")
  cat("\n$zeMap_text        ", "The labels map")
  cat("\n$factorScores      ", "The factor scores (coordinates)")
  cat("\n$constraints       ", "map constraints (a list with minx miny maxx maxy)")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.createNormedFactors
#_____________________________________________________________________

#_____________________________________________________________________
# function: map4DotsAndLabels ----
#_____________________________________________________________________
#' Create \code{ggplot2} factorial maps for dots and labels.
#'NB needs a base map to work correctly
#'
#'  \code{map4DotsAndLabels}:
#' Creates \code{ggplot2} factorial map for dots and labels.
#'NB: needs a base map to work correctly.
#' create the base plot maps for CA type graphs with \code{ggplot2}
#'
#' @param data the factor scores to plot
#' @param axis1 the column of X used for the horizontal axis
#' of the plot. \code{Default 1}.
#' @param axis2 the column of \code{X} used for the vertical axis
#' of the plot. \code{Default 2}.
#' @param   display.points  if  \code{TRUE} (Default)
#' create the map for the points.
#' @param  col.points the color of the points/dots.
#' Can be one color or a vector of colors. If a vector it needs
#' to have exactly the number of items to be plotted.
#' \code{Default = 'blueviolet'}.
#' @param alpha.points \code{(default = .5)}, the alpha
#' (transparency) for the points, should be
#' between 1 (no transparency) and 0
#' (completely transparent).
#' @param pch the character for the points,
#' Default is 19 (Circles).
#' @param  cex size of the dots. Default = 2.5
#' @param display.labels if \code{TRUE} (default), create the
#' map for the labels.
#' @param  col.labels the color of the labels
#' Can be one color or a vector of colors. If a vector it needs
#' to have exactly the number of items to be plotted.
#' Default =  \code{'darkorchid'}.
#' @param alpha.labels (default = 1), the alpha
#'  (transparency) for the labels, should be
#'  between 1 (no transparency) and 0
#'  (completely transparent).
#' @param force = 1. How much \code{ggrepel} repels the labels.
#' @param segment.size = 0,
#'  size of segment line for \code{ggrpel}.
#' @param text.cex = 4, font size for labels.
#' @param font.face (default = \code{'bold'}) font for labels.
#' @param font.family (default = \code{'sans'}),
#'   font family for labels.
#' @param nudge_x (default = 0). From \code{ggrepel},
#' nudge value for starting point for
#'  labels: x dimension.
#' @param nudge_y (default = 0). From \code{ggrepel},
#' nudge value for starting point for
#'  labels: y dimension.
#'  @param alpha.point (default = .5), the alpha
#'  (transparency) for the points, should be
#'  between 1 (no transparency) and 0
#'  (completely transparent).
#' @param ... everything else to pass to the functions
#' @return a list with 6-elements:
#' \enumerate{
#' \item \code{zeMap}: The Complete Map (background Dots and Labels);
#' \item \code{zeMap_background}: The Background map;
#' \item \code{zeMap_dots:} The dots;
#' \item \code{zeMap_text:} The Labels;
#' \item \code{factorScores:}  The factor scores; and
#' \item \code{constraints:} The list of the contraints
#' (could be used to generate other graphs
#'  with the same scaling factors).
#'  }
#; NB class = 'createFactorMap'
#' @section Important_Note: When creating multiple layers graphs,
#' because of the way \code{ggplot2} create graphs, all
#' the matrices/dataframe should all the have the  same column names
#' [e.g., \code{colnames()} equal to
#' \code{c("Dimension 1", "Dimension 2")}].
#' When it is not the case, some strange and cryptic
#' error may be produced
#' (e.g., \code{"cannot find Dimension"}).
#' @import ggplot2 ggrepel
#' @export
#' @author Herve Abdi
#' @rdname map4DotsAndLabels
#' @seealso \code{\link{createFactorMap}} \code{\link{createBaseMap}}
map4DotsAndLabels <- function(data,
                              axis1 = 1, axis2 = 2,
                              display.points = TRUE,
                              col.points = 'blueviolet',
                              alpha.points = .5,
                              pch = 19, # Character for the points
                              cex = 2.5, # size of the points
                              display.labels = TRUE,
                              col.labels = 'darkorchid4',
                              alpha.labels = 1,
                              force = 1,
                              segment.size = 0,
                              # segment width for ggrepel
                              text.cex = 4,
                              # font size for labels
                              font.face =   'bold',
                              # font for labels
                              font.family = 'sans',
                              # font family for labels
                              nudge_x = 0, nudge_y = 0, # from ggrepel
                              # nudge value for starting point for
                              #  labels
                              ... # for ggrepel in case
){# function map4DotsAndLabels  start here
  data <- as.data.frame(data) # make sure that we a df
  V1 = as.name(colnames(data)[1])
  V2 = as.name(colnames(data)[2])
  # Create the Dot-map
  LeG_dot = NULL
  if (display.points){
    LeG_dot =
      # Next describe the points
      ggplot2::geom_point(
        data = data,
        aes_string(V1, V2),
        shape = pch,
        size = cex,
        color = col.points,
        alpha = alpha.points# ,alpha = .5
      )
  }
  # Create the text-map
  LeG_text = NULL
  if (display.labels){
    LeG_text =  # The text
      ggrepel::geom_text_repel(
        label = rownames(data),
        data = data,
        aes_string(V1, V2),
        #aes(G[,1],G[,2]),
        segment.size = segment.size,
        size = text.cex,
        fontface = font.face,
        family = font.family,
        color = col.labels,
        alpha = alpha.labels,
        nudge_x = nudge_x,
        nudge_y = nudge_y
      )
  }
  return(list(leG.points = LeG_dot, leG.labels = LeG_text))
} # End of function.
# function map4DotsAndLabels ends  here
#_____________________________________________________________________
#_____________________________________________________________________

#_____________________________________________________________________
# function: createFactorMapIJ
#_____________________________________________________________________
#' Create
#' ggplot2 factorial maps for CA-type of maps dots and labels.
#'
#'  \code{createFactorMapIJ}: Creates
#'  \code{ggplot2} factorial maps for dots and labels.
#'NB needs a base map to work correctly.
#'
#' @param  Fi the \eqn{I}-set factor scores to plot
#' @param  Fj the \eqn{J}-set factor scores to plot
#' @param axis1 the column of X used for the horizontal axis
#' of the plot. Default 1.
#' @param axis2 the column of X used for the vertical axis
#' of the plot. Default 2.
#' @param constraints a list with minx miny maxx maxy
#' typically obtained from  \code{prettyGraphs::minmaxHelper()}.
#' If \code{NULL}
#' (default) it  is computed with the function
#'  \code{prettyGraphs::minmaxHelper()}.
#' @param title a title for the plots.
#' @param  col.points.i the color of the points/dots for the \eqn{I}-set.
#' Can be one color or a vector of colors. If a vector it needs
#' to have exactly the number of items to be plotted.
#' Default = \code{'blueviolet'}.
#' @param alpha.points.i (default = .5), the alpha
#'  (transparency) for the points, should be
#'  between 1 (no transparency) and 0
#'  (completely transparent).
#' @param pch.i the character for the points for the \eqn{I}-set.
#' Default is 19 (Circles).
#' @param  cex.i size of the dots for the \eqn{I}-set. Default = 2.5
#' @param  col.labels.i the color of the labels for
#' the \eqn{I}-set.
#' Can be one color or a vector of colors. If a vector, it needs
#' to have exactly the number of items to be plotted.
#' Default =  \code{'darkorchid'}.
#' @param alpha.labels.i (default = 1), the alpha
#'  (transparency) for the \eqn{I}-labels, should be
#'  between 1 (no transparency) and 0
#'  (completely transparent).
#' @param text.cex.i = 4,  font size for labels for the I-set.
#' @param segment.size.i = 0, # size of segment
#' @param font.face.i (Default = \code{'bold'},
#' font for labels for the \eqn{I}-set.
#' @param font.family.i (Default = \code{'sans'})
#'  font family for the \eqn{I}-set.
#' @param force.i (Default = 1). How much ggrepel repels the labels
#' for the  \eqn{I}-set.
#' @param  col.points.j the color of the points/dots
#' for the \eqn{J}-set.
#' Can be one color or a vector of colors. If a vector it needs
#' to have exactly the number of items to be plotted.
#' Default = \code{'darkolivegreen4'}.
#' @param alpha.points.j (default = .5), the alpha
#'  (transparency) for the points, should be
#'  between 1 (no transparency) and 0
#'  (completely transparent).
#' @param pch.j the character for the points for the \code{J}-set.
#' Default is 18 (Diamonds).
#' @param  cex.j size of the dots for the \eqn{J}-set. Default = 2.5
#' @param segment.size.j = 0, # size of segment for \eqn{J}-set
#' @param  col.labels.j the color of the labels for
#' the \eqn{J}-set.
#' Can be one color or a vector of colors. If a vector, it needs
#' to have exactly the number of items to be plotted.
#' Default =  \code{'darkolivegreen'}.
#' @param alpha.labels.j (default = 1), the alpha
#'  (transparency) for the \eqn{J}-labels, should be
#'  between 1 (no transparency) and 0
#'  (completely transparent).
#' @param text.cex.j (Default = 4),  font size for labels for the J-set.
#' @param font.face.j (Default = \code{'bold'}),
#' font for labels for the \eqn{J}-set.
#' @param font.family.j (Default = \code{'sans'})
#'  font family for the \eqn{J}-set.
#' @param force.j = 1. How much ggrepel repels
#' the labels for the \eqn{J}-set.
#' @param col.axes (Default = \code{'darkorchid'})
#' color for the axes
#' @param    alpha.axes (Default = .2) transoarency factors for the
#' color of the axes.
#' @param  width.axes (Default = 1.1) the width of the axes
#' @param col.background [Default =
#'  \code{adjustcolor('lavender', alpha.f = .2)}]
#'  The color of the background.
#' @param nudge_x = 0. From \code{ggrepel},
#' nudge value for starting point for
#'  labels: x dimension.
#' @param nudge_y = 0. From \code{ggrepel},
#' nudge value for starting point for
#'  labels: y dimension.
#' @param ... everything else for the functions
#' @return a list with
#'  1) \code{zeMap:} The Complete Map (background Dots and Labels);
#'  2) \code{zeMap_background:} The background;
#' 3) \code{zeMap_dots:} The dots;
#' 4) \code{zeMap_text:} The labels;
#' 5) \code{factorScores}:  The factor scores; and
#' 6) \code{constraints:} The list of the contraints.
#; NB class = 'createFactorMap'
#' @section Important_Note: When creating multiple layers graphs,
#' because of the way \code{ggplot2} creates graphs, all the
#' the matrices/dataframe should all the have the  same column names
#' [e.g., \code{colnames()} equal to c("Dimension 1", "Dimension 2")].
#' When it is not the case, some strange and cryptic
#' error may be produced
#' (e.g., "cannot find Dimension").
#' @import prettyGraphs
#' @export
# @examples \dontrun{}
#' @author Herve Abdi
#' @seealso createFactorMaps createbaseMap
# Function
# createFactorMapIJ
# createFactorMapIJ
# create the base plot for factor type plots
# give a baseMap and one map for dots and one map for labels
#
createFactorMapIJ <- function(Fi,Fj,
                              axis1 = 1, axis2 = 2,
                              constraints = NULL,
                              # list with minx miny maxx maxy
                              # as obtained, e.g.,
                              # from minmacHelper from prettyGraphs
                              title = NULL,
                              # The I-set
                              col.points.i = 'blueviolet',
                              alpha.points.i = .5,
                              # display.points.i = TRUE,
                              pch.i = 19, # Character for the dots
                              cex.i = 2.5, # size of the dots
                              segment.size.i = 0, # size of segment
                              # display.labels.i = TRUE,
                              col.labels.i = 'darkorchid4',
                              alpha.labels.i = 1,
                              text.cex.i = 4, # font size for labels
                              font.face.i =   'bold', # font for labels
                              font.family.i = 'sans' ,
                              force.i = 1 , # force repel factor for ggrepel
                              # The J-set
                              col.points.j = 'darkolivegreen4',
                              #display.points.j = TRUE,
                              alpha.points.j = .5,
                              pch.j = 18, # Character for the dots
                              cex.j = 2.5, # size of the dots
                              segment.size.j = 0, # size of segment
                              #display.labels.j = TRUE,
                              col.labels.j = 'darkolivegreen',
                              alpha.labels.j = .5,
                              text.cex.j = 4, # font size for labels
                              font.face.j =   'bold', # font for labels
                              font.family.j = 'sans' ,
                              force.j  = 1 , # force repel factor for ggrepel
                              # font family for labels
                              col.axes = 'darkorchid',
                              # color for the axes
                              alpha.axes = .2,
                              # transparency for the axes
                              width.axes = 1.1,
                              # width of the axes
                              col.background =
                                adjustcolor('lavender',
                                            alpha.f = .2),
                              # background color
                              nudge_x = 0,
                              nudge_y = 0,
                              ... # more stuff
){
  #
  if (is.null(title)){title = ''}
  # Use ggplot  from ggplot2
  #
  Fi <-  as.data.frame( Fi[,c(axis1,axis2)])
  Fj <-  as.data.frame( Fj[,c(axis1,axis2)])
  # give a name to the Dimension of Fi is there is none
  if (is.null(colnames(Fi))){
    colnames(Fi) <- paste0('Dimension ',1:ncol(Fi))
    colnames(Fj) <- paste0('Dimension ',1:ncol(Fj))
  }
  # G = as.data.frame(Fi[,c(axis1,axis2)])
  #V1 = as.name(colnames(G)[1])
  #V2 = as.name(colnames(G)[2])
  # NB default uses  minmaxHelper from prettyPlots
  if (is.null(constraints)){
    constraints <-
      lapply(prettyGraphs::minmaxHelper(Fi,Fj),'*',1.1)
  }
  #___________________________________________________________________
  # First the basemao
  LeG_b <- createBaseMap(data = rbind(Fi,Fj),
                         constraints = constraints,
                         col.axes = col.axes, #  'darkorchid',
                         alpha.axes = alpha.axes,
                         width.axes = width.axes,
                         col.background =   col.background,
                         title = title)
  # dots and labels map
  LesGi_dl <- map4DotsAndLabels(
    data = Fi,
    axis1 = 1,
    axis2 = 2,
    # display.points = TRUE,
    col.points = col.points.i,
    alpha.points.i = alpha.points.i,
    pch = pch.i,
    # Character for the points
    cex = cex.i,
    # size of the points
    #display.labels = display.labels,
    col.labels = col.labels.i,
    alpha.labels.i = alpha.labels.i,
    text.cex = text.cex.i,
    # font size for labels
    font.face =   font.face.i, #'bold',
    # font for labels
    font.family = font.family.i , # 'sans',
    # font family for labels
    # below are ggrepel values
    force = force.i, # labels' repulsion for ggrepel
    segment.size = segment.size.i,
    # segment width for ggrepel
    nudge_x = nudge_x  ,
    nudge_y = nudge_y # from ggrepel
    # nudge value for starting point for
    #  labels (ggrepel)
    , ... # in case we need more stuff for ggrepel
  )


  LesGj_dl <- map4DotsAndLabels(
    data = Fj,
    axis1 = 1,
    axis2 = 2,
    #display.points   = display.points,
    col.points = col.points.j,
    alpha.points.j = alpha.points.j,
    pch = pch.j,
    # Character for the points
    cex = cex.j,
    # size of the points
    #display.labels = display.labels,
    col.labels = col.labels.j,
    alpha.points.j = alpha.points.j,
    text.cex = text.cex.j,
    # font size for labels
    font.face =   font.face.j, #'bold',
    # font for labels
    font.family = font.family.j ,# 'sans',
    # font family for labels
    # below are ggrepel values
    force = force.j, # labels' repulsion for ggrepel
    segment.size = segment.size.j,
    # segment width for ggrepel
    nudge_x = nudge_x  ,
    nudge_y = nudge_y # from ggrepel
    # nudge value for starting point for
    #  labels (ggrepel)
    , ... # in case we need more stuff for ggrepel
  )

  return.list = structure(list(baseMap = LeG_b,
                               I_labels = LesGi_dl$leG.labels,
                               I_points = LesGi_dl$leG.points,
                               J_labels = LesGj_dl$leG.labels,
                               J_points = LesGj_dl$leG.points),
                          class = 'createFactorMapIJ')

  return(return.list)

} # End of Function
#

#_____________________________________________________________________
# Change the print function for
# the createFactorMapIJ environment
#
#' Change the print function for createFactorMapIJ
#'
#'  Change the print function for createFactorMapIJ
#'
#' @param x a list: output of createFactorMapIJ
#' @param ... everything else for the functions
#' @author Herve Abdi
#' @export
print.createFactorMapIJ <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\nThe base map and 4 Sub-Maps for Correspondence Analysis  \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$baseMap   ", "The Base Map to which the subMaps are added")
  cat("\n$I_labels  ", "I-Set (row) Labels")
  cat("\n$I_points  ", "I-Set (row) Points")
  cat("\n$J_labels  ", "J-Set (col) Labels")
  cat("\n$J_points  ", "J-Set (col) Points")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.createFactorMapIJ
#_____________________________________________________________________

#_____________________________________________________________________
#' @title Creates all the partial ggplot2 maps for CA
#' with all standard variants of normalization for factor scores.
#'
#' @description  \code{createAllMaps4CA}: uses
#'  \code{ggplot2} to
#'  1) create all the partial maps for CA
#' with the different types of normalizations: Asymmetric,
#' Symmetric, True-Barycentric, Biplots, and \code{SPSS}
#'  pseudo Biplots.
#' 2)
#'   to creates \code{ggplot2} factorial maps for dots and labels;
#' 3) to create the base plot maps for CA type graphs, and 4)
#' to creates maps for the \eqn{I} (rows) and the
#'\eqn{J} (column) -sets.
#'
#' @param  allNormedFactors,
#' A list with all the
#' factor scores normed
#' typically from \code{createAllNormedFactors}.
#' @param axis1 the column of X used for the horizontal axis
#' of the plot. Default =  1.
#' @param axis2 the column of X used for the vertical axis
#' of the plot. Default = 2.
#' @param title A main title (default is \code{NULL}).
#' @param  col.points.i the color of the points/dots
#' for the \eqn{I}-set.
#' Can be one color or a vector of colors. If a vector, it needs
#' to have exactly the number of items to be plotted.
#' Default = \code{'blueviolet'}.
#' @param alpha.points.i (default = .5), the alpha
#'  (transparency) for the points, should be
#'  between 1 (opaque) and 0
#'  (completely transparent).
#' @param pch.i the character for the points for the \eqn{I}-set.
#' Default is 19 (Circles).
#' @param  cex.i size of the dots for the \eqn{I}-set.
#'  Default = 2.5
#' @param  col.labels.i the color of the labels for
#' the \eqn{I}-set.
#' Can be one color or a vector of colors. If a vector, it needs
#' to have exactly the number of items to be plotted.
#' Default =  \code{'darkorchid'}.
#' @param alpha.labels.i (default = 1), the alpha
#'  (transparency) for the labels, should be
#'  between 1 (opaque) and 0
#'  (completely transparent).
#' @param text.cex.i = 4,  font size for labels for the I-set.
#' @param segment.size.i = 0, # size of segment
#' @param font.face.i (Default = \code{'bold'})
#' font for labels for the \eqn{I}-set.
#' @param font.family.i (Default = \code{'sans'})
#'  font family for the \eqn{I}-set.
#' @param force.i (Default = 1)
#' How much \code{ggrepel} repels the labels
#' for the  \eqn{I}-set.
#' @param  col.points.j
#' the color of the points/dots for the \eqn{J}-set.
#' Can be one color or a vector of colors. If a vector, it needs
#' to have exactly the number of items to be plotted.
#' Default = \code{'darkolivegreen4'}.
#' @param alpha.points.j (Default = .5), the alpha
#'  (transparency) for the \eqn{J}-points, should be
#'  between 1 (opaque) and 0
#'  (completely transparent).
#' @param pch.j the character for the points for the \eqn{J}-set.
#' Default is 18 (Diamonds).
#' @param  cex.j size of the dots for the \eqn{J}-set.
#' Default = 2.5
#' @param segment.size.j = 0, # size of segment for \eqn{J}-set
#' @param  col.labels.j the color of the labels for
#' the \eqn{J}-set.
#' Can be one color or a vector of colors. If a vector, it needs
#' to have exactly the number of items to be plotted.
#' Default =  \code{'darkolivegreen'}.
#' @param alpha.labels.j (default = 1), the alpha
#'  (transparency) for the \eqn{J}-labels, should be
#'  between 1 (opaque) and 0
#'  (completely transparent).
#' @param text.cex.j (Default = 4)
#' font size for labels for the \eqn{J}-set.
#' @param font.face.j (Default = \code{'bold'}
#' font for the labels for the \eqn{J}-set.
#' @param font.family.j (Default = \code{'sans'})
#'  font family for the \eqn{J}-set.
#' @param force.j (Default = 1). How much \code{ggrepel} repels
#' the labels for the \eqn{J}-set.
#' @param col.axes (Default = \code{'darkorchid'})
#' color for the axes.
#' @param    alpha.axes
#' (Default = .2) transparency factor for the
#' color of the axes.
#' @param  width.axes (Default = 1.1) the width of the axes
#' @param col.background
#' [Default =  \code{adjustcolor('lavender', alpha.f = .2)}],
#' The color of the background.
#' @param ... eveythings else for the functions.
#' @return a list with
#' \code{baseMap_S:} The Background (Symmetric)
#' \code{I_labels_S}  The labels I-set (Symmetric).
#' \code{I_points_S}  The dots I-set (Symmetric).
#' \code{J_labels_S}  The labels J-set (Symmetric).
#'  \code{J_points_S} The dots J-set(Symmetric).
#'  \code{baseMap_A} The Background (Asymmetric)
#'   \code{I_labels_A}  The labels I-set (Asymmetric).
#'\code{I_points_A} The dots I-set (Asymmetric).
#'   \code{J_labels_A}  The labels J-set (Asymmetric).
#'   \code{J_points_A}  The dots J-set (Asymmetric).
#'  \code{baseMap_B} The Background (Barycentric).
#'   \code{I_labels_B} The labels I-set (Barycentric).
#'   \code{I_points_B}  The dots I-set (Barycentric).
#'   \code{J_labels_B} The labels J-set (Barycentric).
#'  \code{J_points_B}  The dots J-set (Barycentric).
#; NB class = 'createAllMaps4CA'
#' @details The final maps are built
#' by combining the elementary maps.
#' For example, a map with the \eqn{J}-set being asymmetric
#' and the \eqn{I}-set being Symmetric would be made
#' by creating the \code{Ja_Is} map as
#' \code{Ja_Is <- baseMap_A + J_labels_A + I_labels_S}.
#'
#' Note than, in general, the \code{Asymmetric} is larger than the
#' \code{Symmetric} map which is in turn larger than the
#' \code{Barycentric} map.
#' @section Important_Note:
#' When creating multiple layers graphs,
#' because of the way \code{ggplot2} create graphs,
#' all
#' the matrices/dataframe should all the have
#' the  same column names
#' [e.g., \code{colnames()}
#' equal to c("Dimension 1", "Dimension 2")].
#' When this is not the case, some strange and cryptic
#' errors may be produced
#' (e.g., "cannot find Dimension").
#' @export
# @examples \dontrun{}
#' @author Hervé Abdi
#' @seealso createFactorMaps createbaseMap
createAllMaps4CA <- function(allNormedFactors,
                             # A list with all the
                             # factor scores normed
                             # typically from createAllNormedFactors
                             axis1 = 1, axis2 = 2,
                             # defaultnames for the graphs
                             # constraints = NULL,
                             # list with minx miny maxx maxy
                             # as obtained, e.g.,
                             # from minmacHelper from prettyGraphs
                             title = NULL,
                             # The I-set
                             col.points.i = 'blueviolet',
                             #display.points.i = TRUE,
                             alpha.points.i = .5,
                             pch.i = 19, # Character for the dots
                             # 19 is circle
                             cex.i = 2.5, # size of the dots
                             segment.size.i = 0, # size of segment
                             #display.labels.i = TRUE,
                             col.labels.i = 'darkorchid4',
                             alpha.labels.i = 1, # alapha for text
                             text.cex.i = 4, # font size for labels
                             font.face.i =   'bold', # font for labels
                             font.family.i = 'sans' ,
                             force.i = 1 , # force repel factor for ggrepel
                             # The J-set
                             col.points.j = 'darkolivegreen4',
                             alpha.points.j = .5,
                             #display.points.j = TRUE,
                             pch.j = 18, # Character for the dots
                             # 18 is diamond
                             cex.j = 2.5, # size of the dots
                             segment.size.j = 0, # size of segment
                             #display.labels.j = TRUE,
                             col.labels.j = 'darkolivegreen',
                             alpha.labels.j = 1,
                             text.cex.j = 4, # font size for labels
                             font.face.j =   'bold', # font for labels
                             font.family.j = 'sans' ,
                             # font family for labels
                             force.j  = 1 , # force repel factor for ggrepel
                             col.axes = 'darkorchid',
                             # color for the axes
                             alpha.axes = .2,
                             # tranparency for the axes
                             width.axes = 1.1,
                             # width of the axes
                             col.background =
                               adjustcolor('lavender',
                                           alpha.f = .2),
                             # background color
                             ... # more stuff
){
  # Start with symmetric
  #
  Fi_S <- allNormedFactors$Fi[,c(axis1,axis2)]
  Fj_S <- allNormedFactors$Fj[,c(axis1,axis2)]
  # constraints_S <-  minmaxHelper(Fi_S, Fj_S)
  Maps_S <-
    createFactorMapIJ(Fi_S,Fj_S,
                      axis1 = axis1, axis2 = axis2,
                      constraints = NULL,
                      # list with minx miny maxx maxy
                      # as obtained, e.g.,
                      # from minmacHelper from prettyGraphs
                      title = title,
                      # The I-set
                      col.points.i = col.points.i,
                      alpha.points.i = alpha.points.i,
                      #display.points.i = TRUE,
                      pch.i = pch.i, # Character for the dots
                      cex.i = cex.i, # size of the dots
                      #display.labels.i = TRUE,
                      col.labels.i =  col.labels.i,
                      alpha.labels.i =  alpha.labels.i,
                      text.cex.i = text.cex.i, # font size for labels
                      font.face.i = font.face.i  , # font for labels
                      font.family.i =   font.family.i,
                      # The J-set
                      col.points.j = col.points.j,
                      alpha.points.j = alpha.points.j,
                      #display.points.j = TRUE,
                      pch.j = pch.j, # Character for the dots
                      cex.j = cex.j, # size of the dots
                      #display.labels.j = TRUE,
                      col.labels.j = col.labels.j,
                      alpha.labels.j =  alpha.labels.j,
                      text.cex.j = text.cex.j, # font size for labels
                      font.face.j =   font.face.j, # font for labels
                      font.family.j =  font.family.j ,
                      # font family for labels
                      col.axes = col.axes,
                      # color for the axes
                      alpha.axes = alpha.axes,
                      # tranparency for the axes
                      width.axes =  width.axes,
                      # width of the axes
                      col.background = col.background ,
                      # background color
                      ... # more stuff
    )
  # Asymetric Maps
  #
  Fi_A <- allNormedFactors$Fi_A[,c(axis1,axis2)]
  Fj_A <- allNormedFactors$Fj_A[,c(axis1,axis2)]
  # constraints_S <-  minmaxHelper(Fi_S, Fj_S)
  Maps_A <-
    createFactorMapIJ(Fi_A,Fj_A,
                      axis1 = axis1, axis2 = axis2,
                      constraints = NULL,
                      # list with minx miny maxx maxy
                      # as obtained, e.g.,
                      # from minmacHelper from prettyGraphs
                      title = title,
                      # The I-set
                      col.points.i = col.points.i,
                      alpha.points.i = alpha.points.i,
                      #display.points.i = TRUE,
                      pch.i = pch.i, # Character for the dots
                      cex.i = cex.i, # size of the dots
                      #display.labels.i = TRUE,
                      col.labels.i =  col.labels.i,
                      alpha.labels.i = alpha.labels.i ,
                      text.cex.i = text.cex.i, # font size for labels
                      font.face.i = font.face.i  , # font for labels
                      font.family.i =   font.family.i,
                      # The J-set
                      col.points.j = col.points.j,
                      alpha.points.j = alpha.points.j,
                      #display.points.j = TRUE,
                      pch.j = pch.j, # Character for the dots
                      cex.j = cex.j, # size of the dots
                      #display.labels.j = TRUE,
                      col.labels.j = col.labels.j,
                      alpha.labels.j = alpha.labels.j,
                      text.cex.j = text.cex.j, # font size for labels
                      font.face.j =   font.face.j, # font for labels
                      font.family.j =  font.family.j ,
                      # font family for labels
                      col.axes = col.axes,
                      # color for the axes
                      alpha.axes = alpha.axes,
                      # tranparency for the axes
                      width.axes =  width.axes,
                      # width of the axes
                      col.background = col.background ,
                      # background color
                      ... # more stuff
    )

  # True Barycentric Maps
  #
  Fi_B <- allNormedFactors$Fi_B[,c(axis1,axis2)]
  Fj_B <- allNormedFactors$Fj_B[,c(axis1,axis2)]
  # constraints_S <-  minmaxHelper(Fi_S, Fj_S)
  Maps_B <-
    createFactorMapIJ(Fi_B,Fj_B,
                      axis1 = axis1, axis2 = axis2,
                      constraints = NULL,
                      # list with minx miny maxx maxy
                      # as obtained, e.g.,
                      # from minmacHelper from prettyGraphs
                      title = title,
                      # The I-set
                      col.points.i = col.points.i,
                      alpha.points.i = alpha.points.i,
                      #display.points.i = TRUE,
                      pch.i = pch.i, # Character for the dots
                      cex.i = cex.i, # size of the dots
                      #display.labels.i = TRUE,
                      col.labels.i =  col.labels.i,
                      alpha.labels.i = alpha.labels.i,
                      text.cex.i = text.cex.i, # font size for labels
                      font.face.i = font.face.i  , # font for labels
                      font.family.i =   font.family.i,
                      # The J-set
                      col.points.j = col.points.j,
                      alpha.points.j = alpha.points.j,
                      #display.points.j = TRUE,
                      pch.j = pch.j, # Character for the dots
                      cex.j = cex.j, # size of the dots
                      #display.labels.j = TRUE,
                      col.labels.j = col.labels.j,
                      alpha.labels.j = alpha.labels.j,
                      text.cex.j = text.cex.j, # font size for labels
                      font.face.j =   font.face.j, # font for labels
                      font.family.j =  font.family.j ,
                      # font family for labels
                      col.axes = col.axes,
                      # color for the axes
                      alpha.axes = alpha.axes,
                      # tranparency for the axes
                      width.axes =  width.axes,
                      # width of the axes
                      col.background = col.background ,
                      # background color
                      ... # more stuff
    )


  return.list = structure(list(baseMap_S =  Maps_S$baseMap,
                               I_labels_S = Maps_S$I_labels,
                               I_points_S = Maps_S$I_points,
                               J_labels_S = Maps_S$J_labels,
                               J_points_S = Maps_S$J_points,
                               baseMap_A =  Maps_A$baseMap,
                               I_labels_A = Maps_A$I_labels,
                               I_points_A = Maps_A$I_points,
                               J_labels_A = Maps_A$J_labels,
                               J_points_A = Maps_A$J_points,
                               baseMap_B =  Maps_B$baseMap,
                               I_labels_B = Maps_B$I_labels,
                               I_points_B = Maps_B$I_points,
                               J_labels_B = Maps_B$J_labels,
                               J_points_B = Maps_B$J_points
  ),
  class = 'createAllMaps4CA')

  return(return.list)



}

#_____________________________________________________________________
# Change the print function for
# the createAllMaps4CA environment
#
#' Change the print function for createAllMaps4CA
#'
#' Change the print function for createAllMaps4CA
#'
#' @param x a list: output of createAllMaps4CA
#' @param ... everything else for the functions
#' @author Herve Abdi
#' @export
print.createAllMaps4CA <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\nBasemaps and Point and Labels maps for Correspondence Analysis  ")
  cat("\nThree Normalizations: Symmetric, Asymmetric, and True Barycentric ")
  cat("\nOne Mais is Basemap + dots and / or Labels \n ")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n Symmetric Maps (Inertia = Lambda)  \n")
  cat(rep("-", ndash), sep = "")
  cat("\n$baseMap_S   ", "The Base Map to which the subMaps are added")
  cat("\n$I_labels_S  ", "I-Set (row) Labels")
  cat("\n$I_points_S  ", "I-Set (row) Points")
  cat("\n$J_labels_S  ", "J-Set (col) Labels")
  cat("\n$J_points_S  ", "J-Set (col) Points")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n Asymmetric Maps (Inertia = Unity)  \n")
  cat(rep("-", ndash), sep = "")
  cat("\n$baseMap_A   ", "The Base Map to which the subMaps are added")
  cat("\n$I_labels_A  ", "I-Set (row) Labels")
  cat("\n$I_points_A  ", "I-Set (row) Points")
  cat("\n$J_labels_A  ", "J-Set (col) Labels")
  cat("\n$J_points_A  ", "J-Set (col) Points")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n True Barycentric Maps (Inertia = Lambda^2)  \n")
  cat(rep("-", ndash), sep = "")
  cat("\n$baseMap_B   ", "The Base Map to which the subMaps are added")
  cat("\n$I_labels_B  ", "I-Set (row) Labels")
  cat("\n$I_points_B  ", "I-Set (row) Points")
  cat("\n$J_labels_B  ", "J-Set (col) Labels")
  cat("\n$J_points_B  ", "J-Set (col) Points")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.createAllMaps4CA
#_____________________________________________________________________


