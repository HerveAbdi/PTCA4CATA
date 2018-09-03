# Function fuzzyRanking
# How to recode ratings by doubling the rating scales for CA
# Hervé Abdi: September 2, 2018
#
#  Preambule for fuzzyRanking----

#' @title recode rating data (e.g., Lickert type scale)
#' into a fuzzy code creating 2 columns
#' \emph{per} variable (a.k.a. doublings, or thermometer code).
#'
#' @description
#' \code{fuzzyRanking}:
#' recode a rank or Lickert type scale
#' into a fuzzy code (a.k.a. doublings, or thermometer coding).
#' \code{fuzzyRanking}: is used to recode rating scale data
#' prior to perform Multiplle Correspondence Analysis on
#' rating scale (e.g., Lickert scale).
#' @param X an \eqn{I} by \eqn{J} matrix where
#' the rows are object or stimuli and the columns are variables.
#' At the intersection of a row and columns is the rating
#' for the object in the row for the variable in the column.
#' @param min (default = \code{NULL})
#' the minimum value(s) for the scale.
#' if \code{NULL} (default), the min is computed per column,
#' if \code{min} is a scalar it is used for all column,
#' if \code{min} is a vector of length \eqn{J} is is used per column.
#' @param max (default = NULL)
#' (default = \code{NULL}) the maximum value(s) for the scale.
#' if \code{NULL} (default), the max is computed per column,
#' if \code{max} is a scalar it is used for all column,
#' if \code{max} is a vector of length \eqn{J} is is used per column.
#' @param neg.mark (default = '.neg') the suffix for the negative
#' side of the scale.
#' @param pos.mark (default = '.pos')
#'  the suffix for the positive
#' side of the scale.
#' @details
#'Each column is recoded as two columns: The first one expresses
#'the distance to the negative pole of the scale and the second one
#'expresses the distance to the positive pole. For example for a 5 point
#'Lickert scale going from 1 to 5, a value of 2 will be recoded as
#'\eqn{d.neg} = 1/4 and \eqn{d.pos} = 3/4.
#'Note that \eqn{d.neg} and \eqn{d.pos} always sum to 1.
#'
#'If \code{X} has no column names, the dimension will be named
#' \code{V1} to \code{VJ}.
#' @return a data frame (or a matrix)
#' @author Hervé Abdi
#' @rdname fuzzyRanking
#' @references
#'
#' Greenacre M. (2017). \emph{Correspondence Analysis in Practice}.
#' Boca Raton: CRC Press. pp 201-208.
#'
#' @examples
#' Y <- matrix(c(1,5,2,4,1,4,2,2), nrow = 4)
#' fuzzy.Y <- fuzzyRanking(Y)
#' @export
fuzzyRanking <- function(X,
                         min = NULL, max = NULL,
                         neg.mark = '.neg',
                         pos.mark = '.pos'){
  # create the min and max
  nJ <- NCOL(X)
  if (is.null(colnames(X))){colnames(X) = paste0('V',1:nJ)}
  if (is.null(min)){min <- apply(X,2,min)}
  if (length(min) == 1){min <- rep(min, nJ)}
  if (length(min) != nJ){stop('Length of min incompatible with ncol(X)')}
  if (is.null(max)){max <- apply(X,2,max)}
  if (length(max) == 1){max <- rep(max, nJ)}
  if (length(max) != nJ){stop('Length of max incompatible with ncol(X)')}
  range <- max - min
  d2Min <-  t(apply(t(X), 2, '-', min))
  pos.X <-  t(apply(t(d2Min), 2, '/', range))
  neg.X <- 1 - pos.X
  colnames(pos.X) <- paste0(colnames(X),pos.mark)
  colnames(neg.X) <- paste0(colnames(X),neg.mark)
  fuzzy.X <- cbind(neg.X,pos.X)
  return(fuzzy.X)
}
# End of  \code{fuzzyRanking}

