# Get group means on factor scores
#_____________________________________________________________________
# Inferences
# getMeans ----
#' @title  get statistics (e.g., \code{means})
#'  of groups of observations.
#' @description \code{getMeans}: compute statistics
#' (e.g., \code{means}) by groups of observations stored in an
#' observations * variables
#' data-frame or matrix.
#' @param G a data frame observations * variables.
#' @param factor a factor vector for the groups of observations
#' @param FUN (\code{default = mean}) the statistics to be computed.
#' @return a data frame with the group means as rows and the
#' variables as columns.
#' @author Hervé Abdi
#' @examples
#' toto = matrix(round(runif(12)*10), nrow = 6, ncol = 2)
#' getMeans(toto, factor(c(1,1,1,2,2,2)), median) # median by groups
#' @rdname getMeans
#' @seealso \code{\link{MakeCIEllipses}}
#' @export
getMeans <- function(G, factor, FUN = mean){
  # A function to get the mean by groups
  groupMean.tmp  <- aggregate(G, list(factor), FUN)
  groupMean <- groupMean.tmp[,2:ncol(groupMean.tmp )]
  rownames(groupMean) <- groupMean.tmp[,1]
  return(groupMean)
} # end of getMeans
#_____________________________________________________________________
