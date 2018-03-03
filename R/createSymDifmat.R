# Functions for computing distances and similarities
#  between 0/1 matrices. Particulary suitable for Check 1 data
#  createSymDifmat
# on the 3rd dimension (K) of a CA cube
# Created March 3.
# Current Version Mrach 4. 2018. Herve Abdi
#---------------------------------------------------------------------
# Helper for roxygen2
# sinew::makeOxygen(naem of a function)


#=====================================================================
# function createSymDist4PTCA
#
#---------------------------------------------------------------------
# Helper for roxygen2
# sinew::makeOxygen(createSymDist4PTCA)
#
#---------------------------------------------------------------------
#' @title Create a matrix of
#' the symmetric difference distance / cross-products
#' for the 3rd dimension of a brick of non-negative numbers
#' (i.e., a brick of data for a BeTA or a CATA test).
#'
#' @description \code{createSymDist4PTCA}
#' creates a matrix of the
#' symmetric difference distance / cross-products
#' between a set of \eqn{K} matrices stored as
#' the 3rd dimension of an \eqn{I*J*K} brick of non-negative numbers
#' (i.e., a brick of data for a BeTA/CATA test).
#' The symmetric difference distance between two (0/1) matrices
#' \eqn{X} and \eqn{Y} is computed as
#' \eqn{sum(abs(X - Y))}.  The \eqn{K*K} distance matrix is
#' converted using double centering  into a cross-product matrix
#' that can be analyzed by
#' an eigen-decomposition to provide a STATIS-like
#' scalar-product/RV-map.
#' @param dataCube  an \eqn{I*J*K} brick of non-negative numbers
#' (i.e., a brick of data for a CATA test)
#' @return return.list A list with
#' \code{Distance}:
#' a \eqn{K*K} (symmetric difference) distance  matrix
#' \code{CrossProduct}:
#' a \eqn{K*K} cross-product matrix obtained by double centering
#' the distance matrix
#' (the cross-product can be eigen-decomposed to give an MDS).
#' @details Note that the symmetric difference distance is meaningful
#' only between 0/1 matrices.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # with aCubeOfCATAData being an I*J*K array
#'  DistanceMat <- createSymDist4PTCA(aCubeOfCATAData)
#'  }
#' }
#' @author Herve Abdi
#' @rdname createSymDist4PTCA
#' @export
createSymDist4PTCA <- function(dataCube){
   lesDims <- dim(dataCube)
   if (length(lesDims) != 3){'dataCube should be a 3-Way array'}
   nI <- lesDims[1]
   nJ <- lesDims[2]
   nK <- lesDims[3]
   if (is.null(dimnames(dataCube)[[3]])){# Make sure
     # that there are names for the slices
     dimnames(dataCube)[[3]] <- paste0('Slice ',1:nK)
   }
   #
   Smat <- matrix(dataCube, nrow = nK, ncol = nI*nJ, byrow = TRUE)
   # # Normalize the rows of Smat if RV is required
   # if(tolower(normalization) == 'rv'){# normalize when RV is chosen
   #  Smat <- t(apply(Smat,  1 , function(x) x / sqrt(sum(x^2)) ))
   #                                   }# end of if
      Dmat <- abs(1 - Smat) %*% t(Smat) + Smat %*% t(abs(1 - Smat))
         # Smat %*% t(Smat)
      nameOfRows <- dimnames(dataCube)[[3]]
    # Eliminate rows and Column of Slices with null rows

       rownames(Dmat) <- nameOfRows -> colnames(Dmat)
       # Cross product Matrix
       masses <- rep(1/nK, nK)
       LeM = t(kronecker(masses, t(rep(1, nK))))
       Xhi <- diag(1, nK) - LeM
       Cmat <- -0.5 * sqrt(LeM) * Xhi %*% Dmat %*% t(Xhi) * sqrt(t(LeM))

       return.list <- structure(
         list( Distance = Dmat,
               CrossProducts = Cmat),
         class = 'SymDif')


    return(return.list)
} # End of function createSymDi4PTCA
#=====================================================================
#=====================================================================
#*********************************************************************
# ********************************************************************
# ********************************************************************
#' Change the print function for object of class \code{SymDif}
#' (from function \code{createSymDi4PTCA})
#'
#'  Change the print function for objects of class \code{SymDif}.
#'  (from function \code{createSymDi4PTCA})
#' @param x a list: object of class \code{SymDif},
#'   output of function: \code{createSymDi4PTCA}.
#' @param ... everything else for the function
#' @author Herve Abdi
#' @export
print.SymDif <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Symmetric Difference Distance Matrix & CrossProduct \n")
  #cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$Distance     ", "The Symmetric Difference Distance Matrix")
  cat("\n$CrossProduct ", "The CrossProduct Matrix (from Distance)")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.bootRatios
