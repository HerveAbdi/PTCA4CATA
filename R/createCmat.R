# Functions for computing similarities:
# CA.SfromX createCmat4PTCA
# on the 3rd dimension (K) of a CA cube
#
#---------------------------------------------------------------------
# Helper for roxygen2
# sinew::makeOxygen(CA.SfromX)
#
#---------------------------------------------------------------------
#' @title Compute the cross-product matrix for CA from
#' a contingency table
#' @description \code{CA.SfromX} computes
#' the I*I row cross-product matrix S for CA from
#' a I * J contingency table X.
#' S is computed from the mean centered
#' row profiles, with Abdi and Bera (2014, 2018) notation:
#' S = M^{1/2} * (R - c1') * W * (R - c1')'  M^{1/2}.
#' The eigen-decomposition of S gives the eigen-values
#' of the CA of Xm the factor scores can aslo be obtained
#' from the eigen vectors after an appropriate normalization.
#' Columns with 0 sum are ignored.
#' @param X a data matrix with non-zero elements
#' @param center = TRUE. if TRUE center the matrix.
#' when center = FALSE, the first eigenvalue of S is equal to 1
#' and the first eigenvector is equal to r^(1/2), the other
#' eigenvectors and eigenvalues are then the same are tose of the
#' centered S.
#' @return S  (the cross-product matrix)
#' @details see Abdi and Bera, (2018) for details.
#' @references Abdi H. & Bera, M. (in Press, 2018).
#' Correspondence analysis. In R. Alhajj and J. Rokne (Eds.),
#' Encyclopedia of Social Networks and Mining (2nd Edition).
#' New York: Springer Verlag
#' @examples
#' \dontrun{
#' if(interactive()){
#' data(authors,package = 'ExPosition')
#' X <- as.matrix(authors$ca$data)
#' S <- CA.SfromX(X)
#'  }
#' }
#' @rdname CA.SfromX
#' @author Herve Abdi
#' @export
CA.SfromX <-function(X, center = TRUE){
  # Function for PTCA4CATA. 10/01/2017.
  X <- X[, which(colSums(X) !=0)]
  Z <- X / sum(X)
  m <- rowSums(Z)
  c <- colSums(Z)
  w <- 1/c
  nI <- nrow(Z)
  nJ <- ncol(Z)
  R <- matrix(1/m, nI,nJ) * Z
  if (center){R = R - matrix(c, nI, nJ,byrow = TRUE ) }
  m12Rw12 <- matrix(m^(1/2),  nrow = nI, ncol = nJ ) *
                   R *
             matrix(w^(1/2), nrow = nI, ncol = nJ, byrow = TRUE )
  RRt <-  m12Rw12 %*% t(m12Rw12)
  return(RRt)
}
#=====================================================================
#=====================================================================



#=====================================================================
# function createCmat4PTCA
#
#---------------------------------------------------------------------
# Helper for roxygen2
# sinew::makeOxygen(createCmat4PTCA)
#
#---------------------------------------------------------------------
#' @title 
#' Creates a matrix of cross-products or RV coefficients
#' for the 3rd dimension of a brick of non-negative numbers
#' (i.e., a brick of data for a CATA test).
#' @description \code{createCmat4PTCA}
#' creates a matrix of cross-product (i.e., scalar products
#' between two matrices)
#' or RV coefficients
#' for the 3rd dimension of an \eqn{I*J*K} brick of non-negative numbers
#' (i.e., a brick of data for a CATA test).
#' The coefficients
#' are computed from the I row-profiles (observations) and stored
#' in a \eqn{K*K} semi-positive definite matrix that can be analyzed by
#' an eigen-decomposition to provide a STATIS-like
#' scalar-product/RV-map.
#' @param dataCube  an \eqn{I*J*K} brick of non-negative numbers
#' (i.e., a brick of data for a CATA test)
#' @param normalization Type of normalization
#' can be 'cp' ( cross-product, Default) or 'Rv' (for the Rv coefficient)
#' @return A K*K cross-product or Rv Matrix depending upon
#' the value of the parameter \code{normalization}.
#' @details Each of the K slices of the I*J*K brick
#' of data is first transformed into an I*I S (for a CA analysis)
#' matrix using the
#' function \code{PTCA4CATA::CA.SfromX}
#'  (Empty columns are eliminated before computing the matrix S).
#'  This creates an I*I*K brick of S matrices which is then used to
#'  compute the I*I scalar-product/Rv matrix
#'  that measures the similarity
#'  between all slices of \code{dataCube}.
#'  Note: that this matrix can be used
#'  in a STATIS approach to re-weight the
#'  slices of the \code{dataCube}.
#'  Note: that the rows of each slice are supposed to have
#'  at least one non-zero entry.
#'  Slices with zero rows are eliminated and a warning
#'  message is issued
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # with aCubeOfCATAData being an I*J*K array
#'  Cmat <- createCmat4PTCA(aCubeOfCATAData)
#'  }
#' }
#' @author Herve Abdi
#' @rdname createCmat4PTCA
#' @export
createCmat4PTCA <- function(dataCube, normalization = 'cp'){
   lesDims <- dim(dataCube)
   if (length(lesDims) != 3){'dataCube should be a 3-Way array'}
   nI <- lesDims[1]
   # nJ <- lesDims[2]
   nK <- lesDims[3]
   if (is.null(dimnames(dataCube)[[3]])){# Make sure
     # that there are names for the slices
     dimnames(dataCube)[[3]] <- paste0('Slice ',1:nK)
                }
   # Initialize the Cube of Cross-Products
   Scube = array(NA, dim = c(nI,nI,nK))
   # Compute the Scube with an ugly loopk
  Obs2Clean <- NULL
  for (k in 1:nK){# loop on k
   zeSlice <- dataCube[,,k]
   if  (!(sum( rowSums(zeSlice ) == 0 ) > 0) ) {
       Scube[,,k] <- CA.SfromX(zeSlice) }else{ # empty rows
         warning(paste0('Observation ', k,
                  ' has empty rows and has been eliminated'))
         Obs2Clean <- c(Obs2Clean,k)
       }
       } # end of loop on k
   #
   # Check for NaN
   # is.nan(SCube)
   # Unfold the Cube
   Smat <- matrix(Scube, nrow = nK, ncol = nI*nI, byrow = TRUE)
   # Normalize the rows of Smat if RV is required
   if(tolower(normalization) == 'rv'){# normalize when RV is chosen
    Smat <- t(apply(Smat,  1 , function(x) x / sqrt(sum(x^2)) ))
                                     }# end of if
      Cmat <- Smat %*% t(Smat)
      nameOfRows <- dimnames(dataCube)[[3]]
    # Eliminate rows and Column of Slices with null rows
    if(!is.null(Obs2Clean)){
       Cmat <- Cmat[-Obs2Clean,-Obs2Clean]
       nameOfRows <- nameOfRows[-Obs2Clean]
       }
       rownames(Cmat) <- nameOfRows -> colnames(Cmat)
    return(Cmat)
} # End of function createCmat4PTCA
#=====================================================================
#=====================================================================
