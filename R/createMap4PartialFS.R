# File for development of plotPartial
# Create aggplot version of
#  DistatisR::GraphDistatisPartial

# A new minmaxHelper4Partial function

#=====================================================================
# Function minmaxHelper4Partial
#---------------------------------------------------------------------
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(minmaxHelper4Partial)
#
#---------------------------------------------------------------------
#=====================================================================
#Need to make that as a separate function
# Get the constraints for plotting
#' @title
#' Computes the x- and y- axis constraints for
#' graphs combininig a dataframe and a cube of partial factor sscores.
#' Used
#' for all \code{prettyGraphs} and \code{prettyGraph}-like
#' (e.d. \code{PTCA4CATA}) functions.
#'
#' @description
#' \code{minmaxHelper4Partial}:
#' Computes the x- and y- axis constraints for
#' graphs combininig a dataframe and a cube of partial factor sscores.
#' Used
#' for all \code{prettyGraphs} and \code{prettyGraph}-like
#' (e.d. \code{PTCA4CATA}) functions. Factors scores and
#' partial factor scores should be describing the same set of
#' \eqn{I} observations.
#'
#' @param FactorScores An \eqn{I} observations by \eqn{J}
#' dimensions  data frame od factor scores
#' @param partialFactorScores An \eqn{I} observations by \eqn{J}
#' dimensions by \eqn{K} block array of partial factor scores.
#' @param axis1 (Default: 1) the number of the first dimension
#' of interest.
#' @param axis2 (Default: 2) the number of the second dimension
#' of interest.
#' @param expansionFactor  (Default: 1.1) a multiplicative factor
#' for the list (when larger than 1 this is an expansion,
#' smaller than 1 this is a shrinkage).
#' @return A list with \code{minx, miny, maxx, maxy} that can be
#' used by various graphic functions from, for example,
#' \code{prettyGraphs}, \code{DistatisR}, or \code{PTCA4CATA}.
#' @examples
#' \dontrun{
#' testminmax <- minmaxHelper4Partial(someScores, somePartialScores )
#' }
#' @rdname minmaxHelper4Partial
#' @author Herve Abdi & Derek Beaton
#' @export
minmaxHelper4Partial <-  function(FactorScores, partialFactorScores,
                                  axis1 = 1, axis2 = 2,
                                  expansionFactor = 1.1){

  # nK <- dim(partialFS)[[3]]
  toto <- partialFactorScores[,c(axis1,axis2),]
  lesMin <- apply(t(apply(toto,c(2,3),min)),2,min)
  lesMax <- apply(t(apply(toto,c(2,3),max)),2,max)
  tutu <- FactorScores[,c(axis1,axis2)]
  lesMin <- apply(rbind(lesMin,tutu),2,min)
  lesMax <- apply(rbind(lesMax,tutu),2,max)
  #
  constraint4PFS <- lapply(
    list(minx = lesMin[1],
         miny = lesMin[2],
         maxx = lesMax[1],
         maxy = lesMax[2]
    ),'*', expansionFactor)
  return(constraint4PFS)
} # end of function minmaxHelper4Partial
#=====================================================================
#


#=====================================================================
# function createPartialFactorScoresMap
#=====================================================================
#---------------------------------------------------------------------
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(createPartialFactorScoresMap)
#
#---------------------------------------------------------------------
# function createPartialFactorScoresMap
# Parameters here
#' @title Create a \code{ggplot2} map for partial factor scores
#' (for \code{PTCA4CATA}, or\code{distatis}) to be added to
#' the main map of the factor scores.
#'
#' @description \code{createPartialFactorScoresMap}:
#' Creates a map for partial factor scores for blocks of data
#' (e.g., for \code{PTCA4CATA} or \code{distatis}) to be added to
#' the main map of the factor scores (created for example
#' by \code{createFactorMap}).
#' The partial factor scores are colored by Blocks and by Items
#' (i.e. observations).
#' The data are 1) an \eqn{I} Items by \eqn{J} factors/dimensions
#' data set storing the main factor scores
#' (\code{factorScores}), and  2)
#' an \eqn{I} Items by \eqn{J} factors/dimensions by
#' \eqn{K} Blocks storing the partial factor scores.
#' The resulting map connects the partial factor scores to their
#' respective main factor scores.
#' @param factorScores an \eqn{I} Items by \eqn{J}
#' factors/dimensions
#' data set storing the main factor scores.
#' @param partialFactorScores an
#' \eqn{I} Items by \eqn{J} factors/dimensions by
#' \eqn{K} Blocks storing the partial factor scores.
#' Needs to have numbers of Items as \code{factorScores}.
#' @param axis1 (Default: 1) the horizontal dimension of the map.
#' @param axis2 (Default: 2) the vertical dimension of the map.
#' @param colors4Items (Default: \code{NULL})
#' color name(s) for the Items,
#' can be one element or a \eqn{I} by 1 vector of color names.
#' If \code{NULL} (defaults), \code{prettyGraphsColorSelection()}
#' is used to select the colors.
#' @param colors4Blocks
#' (Default: \code{NULL})
#' color name(s) for the Blocks,
#' can be one element or a \eqn{K} by 1 vector of color names.
#' If \code{NULL} (defaults), \code{prettyGraphsColorSelection()}
#' is used to select the colors.
#' @param names4Partial a vector for (preferably short)
#' names of the Blocks. If
#' \code{NULL} (default), the names are obtained from
#' the third dimension of \code{partialFactorScores};
#' if the third dimension of \code{partialFactorScores}
#' is \code{NULL}, then block are names from 1 to \eqn{K}.
#' @param alpha.lines (Default: 0.5) transparency parameter
#'  for the lines, should be between 0 (complety transparent)
#'  and 1 (no transparent).
#' @param size.lines (Default: 0.75) thickness of the lines
#' connecting the partial factor scores to their main scores.
#' @param type.lines (Default: 1) the type (from 1 to 6)
#'  of the lines
#' connecting the partial factor scores to their main scores.
#' @param arrow.length (Default: 0) the length in cm. of the
#' arrow point toward the factor scores.
#' @param alpha.points (Default: 0.7)
#'  transparency parameter
#'  for the points, should be between 0 (complety transparent)
#'  and 1 (no transparent).
#' @param shape.points (Default: 23) the pch for the shape
#' (from 1 to 25). Default (23) is the diamond.
#' @param size.points (Default: 1) the size of the points.
#' @param alpha.labels (Default: 0.7)
#'  transparency parameter
#'  for the labels, should be between 0 (complety transparent)
#'  and 1 (no transparent).
#' @param font.labels (Default: 'plain') the font for the
#' labels, can be \code{"plain", "bold", "italic"}
#' @param family.labels (Default: 'sans') the family of the
#' fontn used, this set depends upon the set of font installed,
#' the three save choices (always installed) are
#' \code{'sans','serif','mono'}.
#' @param size.labels (Default: 2) the size of the labels.
#' @return a list with
#' 1)  \code{$mapColByItems}:  the Partial Factor
#' Score Map Colored by Items;
#' 2)
#'\code{$mapColByBlocks}: the Partial Factor
#'Score Map Colored by Blocks;
#'3) \code{$linesColByItems}: the Lines Colored by Items;
#'4) \code{$pointsColByItems}: Points Colored by Items;
#'5) \code{$labelsColByItems}: the Labels Colored by Items;
#'6) \code{$linesColByBlocks}:  the Lines Colored by Blocks;
#'7) \code{$pointsColByBlocks}: the Points Colored by Blocks;
#'and 8) #'\code{$labelsColByBlocks}:  the Labels Colored by Blocks.
#' @details The maps show the connections from the
#' partial factor scores to the main factor scores
#' (which are baycentric to their respective partial factor scores).
#' The map obtained should be added to a main map (of the
#' main factor scores) that should be created prior to
#' adding the map of the partial factor scores.
#' Because the partial factor scores have a larger variance
#' than the main factor scores, the dimension (i.e.
#' the \code{constraints}) of the
#' map should be computed (with \code{minmaxHelper4Partial()})
#' before creating the main of the main factor scores
#' (see example).
#' @section Important_Note: When creating multiple layers graphs
#' because of the way \code{ggplot2} create graphs all the
#' the matrices/dataframe should all the have the  same column names
#' [e.g., \code{colnames()} equal to c("Dimension 1", "Dimension 2")].
#' When it is not the case, some strange and cryptic
#' error may be produced
#' (e.g., "cannot find Dimension").
#' @examples
#' \dontrun{
#' # fS are factor scores and pFS are partial factor scores
#' # first get the constraints
#' constraint4PFS <- minmaxHelper4Partial(fS, pFS)
#' # Get the background map
#' BaseMap.FS <- createFactorMap(X = fS ,
#'                               constraints = constraint4PFS)
#' # Create the map of the partial factor scores
#' map4pFS <- createPartialFactorScoresMap(fS, pFS)
#' map_FS_pFS <- BaseMap.FS + map4pFS
#' # To print the Map:
#' print(map_FS_pFS )
#' }
#' @seealso minmaxHelper4Partial createFactorMap DistatisR
#' @rdname createPartialFactorScoresMap
#' @author Herve Abdi
#' @export
createPartialFactorScoresMap <- function(
factorScores,         # = FS
partialFactorScores,  # <- PartialFS
axis1 = 1,
axis2 = 2,
colors4Items = NULL,  # as.vector(color4Products) # default is NULL
colors4Blocks = NULL,  #
names4Partial = NULL, #  c('5','C') # Short names NULL
alpha.lines  = .5,
size.lines   = .75,
type.lines = 1,
arrow.length = 0,
alpha.points = .7,
shape.points = 23,
size.points  = 1,
alpha.labels = .7,
font.labels = "plain",
family.labels = 'sans',
size.labels  = 2){# Begin function createPartialFactorScoresMap
#---------------------------------------------------------------------
# function to start here
#
# Check the dimensionality
nI <- nrow(factorScores)
if (nI != dim(partialFactorScores)[[1]]){
  stop('incompatible number of rows between FS and PartialFS')
}
# Keep only the important subset
fS  <- factorScores[,c(axis1,axis2)]
pFS <- partialFactorScores[,c(axis1,axis2),]
# get the defaults correct
if (is.null(arrow.length)){ arrow.length = 0 }
nK <- dim(pFS)[[3]]
if (is.null(dimnames(pFS)[[3]])){dimnames(pFS)[[3]] <- 1:nK}
if (is.null(names4Partial)){names4Partial = dimnames(pFS)[[3]] }
if (is.null(colors4Blocks)){# Color4Blocks by default
  colors4Blocks <- prettyGraphsColorSelection(nK)}
if (is.null(colors4Items)){# Color4Blocks by default
  colors4Items <- prettyGraphsColorSelection(nI)}

# check null colnames
if (is.null(colnames(fS))){
    colnames(fS) <- paste0('Dimension ', c(axis1,axis2))
} # end if

if (is.null(names4Partial)){names.part = dimnames(pFS)[[3]] }
if (is.null(names4Partial)){names.part =  seq(1:dim(pFS)[[3]]) }

#---------------------------------------------------------------------
# First create the map with dots and labels
# Arrow and line below
leGraph.lines.partial.i <- list()
leGraph.points.partial.i  <- list()
leGraph.names.partial.i <- list()
leGraph.lines.partial.b <- list()
leGraph.points.partial.b  <- list()
leGraph.names.partial.b <- list()
for (i in  1:nI){
  pFSi <- as.data.frame(t(pFS[i,1:2,]))

  zePoints <- geom_point(data = pFSi,
                         color = colors4Items[i],
                         alpha = alpha.points,
                         shape = shape.points,
                         size  = size.points)
  zeLabels <- geom_text_repel(data = pFSi,,
                              color = colors4Items[i],
                              label = names4Partial,
                              alpha = alpha.labels,
                              size  = size.labels,
                              fontface = font.labels,
                              family = family.labels,
                              force = 3,
                              segment.size = 0)
  zeLines <- suppressWarnings(annotate("segment",
            x = fS[i,1], y = fS[i,2],
            xend = pFS[i,1,],
            yend = pFS[i,2,],
            color = colors4Items[i],
            alpha = alpha.lines,
            size = size.lines,
            linetype = type.lines,
            arrow =  arrow(length = unit(arrow.length, "cm"))))
  leGraph.lines.partial.i[[i]]  <- zeLines
  leGraph.points.partial.i[[i]] <- zePoints
  leGraph.names.partial.i[[i]] <- zeLabels
  zePoints <- geom_point(data = pFSi,
                         color = colors4Blocks,
                         alpha = alpha.points,
                         shape = shape.points,
                         size  = size.points)
  zeLabels <- geom_text_repel(data = pFSi,,
                              color = colors4Blocks,
                              label = names4Partial,
                              alpha = alpha.labels,
                              size  = size.labels,
                              fontface = font.labels,
                              family = family.labels,
                              force = 3,
                              segment.size = 0)
  zeLines <- suppressWarnings(annotate("segment",
                    x = fS[i,1], y = fS[i,2],
                    xend = pFS[i,1,],
                    yend = pFS[i,2,],
                    color = colors4Blocks,
                    alpha = alpha.lines,
                    size = size.lines,
                    linetype = type.lines,
              arrow =  arrow(length = unit(arrow.length, "cm"))))
  leGraph.lines.partial.b[[i]]  <- zeLines
  leGraph.points.partial.b[[i]] <- zePoints
  leGraph.names.partial.b[[i]]  <- zeLabels
  } # end loop in I

return.list <- structure(
  list(mapColByItems = list(leGraph.lines.partial.i,
        leGraph.points.partial.i, leGraph.names.partial.i) ,
       mapColByBlocks = list(leGraph.lines.partial.b,
        leGraph.points.partial.b, leGraph.names.partial.b),
       linesColByItems  = leGraph.lines.partial.i,
       pointsColByItems = leGraph.points.partial.i,
       labelsColByItems = leGraph.names.partial.i,
       linesColByBlocks  = leGraph.lines.partial.b,
       pointsColByBlocks = leGraph.points.partial.b,
       labelsColByBlocks = leGraph.names.partial.b
  ), class = "map4pfs")
return(return.list)
}
# End of function here
#--------------------------------------------------------------------
#---------------------------------------------------------------------

#---------------------------------------------------------------------
# Print function for class "map4pfs"
#*********************************************************************
#---------------------------------------------------------------------
#' Change the print function for the class \code{map4pfs}
#' (e.g., output from \code{createPartialFactorScoresMap})
#'
#' Change the print function for the class \code{map4pfs}
#' (e.g., output from \code{createPartialFactorScoresMap}).
#'
#' @param x a list objects of the class {map4pfs}
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.map4pfs <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: Partial Factor Scores Maps and their Components (to add to a Map) \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$mapColByItems     ", "Partial Factor Score Map Colored by Items.")
  cat("\n$mapColByBlocks    ", "Partial Factor Score Map Colored by Blocks.")
  cat("\n$linesColByItems   ", "Lines Colored by Items.")
  cat("\n$pointsColByItems  ", "Points Colored by Items.")
  cat("\n$labelsColByItems  ", "Labels Colored by Items.")
  cat("\n$linesColByBlocks  ", "Lines Colored by Blocks.")
  cat("\n$pointsColByBlocks ", "Points Colored by Blocks.")
  cat("\n$labelsColByBlocks ", "Labels Colored by Blocks.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.cubeOfCovDis
#---------------------------------------------------------------------
