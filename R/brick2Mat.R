# Two useful functions to go from brick to matrix and back

# brick2Mat -------------------------------------------
# ___________________________________________________________________
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(brick2Mat)
#
# Preambule brick2Mat ------------------------------------------------
# ____________________________________________________________________
#' @title Unfold a brick of data to a matrix
#' @description \code{brick2Mat}:
#' Unfold an \eqn{I} by \eqn{J} by \eqn{K}
#' brick of data to an
#' (\eqn{I}*\eqn{K}) by \eqn{J} matrix that will inherit
#' the dimension names from the brick of data.
#' @param aBrick an \eqn{I} by \eqn{J} by \eqn{K}
#' brick of data.
#' @param names4Rows  (Default: \code{NULL}) a \eqn{I}*\eqn{K}
#' vector for the row names of the output matrix.
#' When \code{NULL}, the row names are obtained by concatenation
#' of the \code{dimnames(brick2mat)[[1]]} and
#' \code{dimnames(brick2mat)[[3]]}.
#' @return an
#' (\eqn{I}*\eqn{K}) by \eqn{J} matrix.
#' @author Herve Abdi
#' @rdname brick2Mat
#' @export
brick2Mat <- function(aBrick, names4Rows = NULL){
  aMat <- aBrick
  aMat <- aperm(aMat, c(1,3,2))
  dim(aMat) <- c(dim(aBrick)[[1]]*dim(aBrick)[[3]],
                 dim(aBrick)[[2]])
  if (is.null(names4Rows)){ # create row names by multiplication
    n1 <- dim(aBrick)[[1]]
    n3 <- dim(aBrick)[[3]]
    d1 <- dimnames(aBrick)[[1]]
    d3 <- dimnames(aBrick)[[3]]
    names4Rows <- paste0(rep(d1, n3 ), '-', rep(d3, each = n1))
  }
  rownames(aMat) <- names4Rows
  colnames(aMat) <- dimnames(aBrick)[[2]]
  return(aMat)
} # End of brick2Mat ----
#____________________________________________________________________


# mat2Brick  ----
# ___________________________________________________________________
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(mat2Brick)
#
# Preambule mat2Brick ------------------------------------------------
# ____________________________________________________________________

#' @title tranform a matrix into a Brick of Data
#'
#' @description
#' \code{mat2Brick}:
#' Unfold an (\eqn{I}*\eqn{K}) by \eqn{J} data matrix into
#' an \eqn{I} by \eqn{J} by \eqn{K}
#' brick of data (i.e., array).
#' @param aMat an (\eqn{I}*\eqn{K}) by \eqn{J}
#'  data matrix.
#' @param nBlocks number of blocks (i.e., 3rd dimension of
#' the output array). No default.
#' @param names4Rows  (Default: \code{NULL}) a \eqn{I}
#' element
#' vector for the row names of the output array.
#' When \code{NULL}, the row names are
#' created as {I}-1 to  I-\eqn{I}.
#' @param names4Blocks
#' (Default: \code{NULL}) a \eqn{K}
#' element
#' vector for the names of  3rd-dimension of the output array.
#' When \code{NULL}, the row names are
#' created  as \eqn{B}-1 to \eqn{B}-K.
#' @return
#' an \eqn{I} by \eqn{J} by \eqn{K}
#' brick of data (i.e., array).
#' @rdname mat2Brick
#' @export
mat2Brick <- function(aMat, nBlocks,
                      names4Rows = NULL, names4Blocks = NULL){
  nI.all <- nrow(aMat)
  nI <- nI.all / nBlocks
  aCube <- aMat
  dim(aCube) <- c(nI, nBlocks, ncol(aMat))
  aCube <- aperm(aCube,c(1,3,2))
  dimnames(aCube)[[2]] <- dimnames(aMat)[[2]]
  if (is.null(names4Rows)){names4Rows = paste0('I-',1:nI)}
  if (is.null(names4Blocks)){names4Blocks = paste0('B-',1:nBlocks)}
  dimnames(aCube)[[1]] <- names4Rows
  dimnames(aCube)[[3]] <- names4Blocks
  return(aCube)
}
