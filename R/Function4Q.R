#
# Functions for package PTCA4CATA
# Here the functions are for Cochran Q
#
# Work in progress. Hervé Abdi
# Started October 16, 2016.
# Last update  October 16, 2016.
#
# --------------------------------------------------------------------
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890
# --------------------------------------------------------------------
# 4. Taylor made functions from  CATA_Example4Bangkok.Rmd
# --------------------------------------------------------------------

#' Cochran's Q test for a slice of a cube
#'
#'  Cochran's Q test for a slice of a cube.
#'  Used as a subfunction of Q4CATA.Cube
#'  Needs the \code{coin} library.
#'  The data are stored in a matrix
#' that represents the choices of
#' J participants (col) for I products (row)
#' about one variable
#' with
#'     rows as products
#'     columns as assessors
#' x(i,j) = 1 if p(i) was chosen by a(j), 0 if not.
#'
#' @param Data4Q a slice of CATA cube
#' @importFrom coin symmetry_test statistic pvalue
#' @return a vector with the value of \code{chi2}
#'  and \code{pvalue}. The value for the chi2
#' @author Herve Abdi
#' @export
#'

Q4CATA.Slice <- function(Data4Q){
  # Cochran Q test
  # The data are stored in a matrix
  # that represents the choices of
  # J participants (col) for I products (row)
  # about one variable
  # with
  #     rows as products
  #     columns as assessors
  # \eqn{x(i,j) =} 1 if \eqn{p(i)} was chosen by \eqn{a(j)}, 0 if not
  # NB: require(coin)
  # Make Data4Q a Vector to use long form
  as.vec <- as.vector(Data4Q)
  nP <- nrow(Data4Q)
  nS <- ncol(Data4Q)
  F4Products   <- factor(rep(seq(1:nP),nS ))
  F4Participant <- factor((1:nS)%x%rep(1,nP))
  # Use symmetry_test from library(coin)
  Res4Q <- coin::symmetry_test(as.vec ~ F4Products |
                          F4Participant,
                        data = data.frame(as.vec,
                                F4Products,F4Participant),
                        teststat = "quad")
  pvalue <- coin::pvalue(Res4Q)
  chi2   <- coin::statistic(Res4Q)
  # Qres = list(p = pvalue, chi2 = chi2)
  Qres <- c(chi2, pvalue)
  return(Qres)
} # End of function Q4CATASlice
#
# ------------------------------------------------------------------

# Q4CATA.Cube
#' Compute Cochran's Q
#' for a Cube of CATA Data
#'
#' Compute Cochran's Q
#' for a Cube of CATA Data
#' (as created, e.g., by \code{readCATAfromXL})
#' I: Rows   are Products
#' J: Columns are Variables
#' K: Third Dimension is Assessors.
#' x_{i,j,k} = 1 means
#' Assessor k chosed Variable j for Product i
#' NB. uses function \code{Q4CATA.Slice}
#'
#' @param ZeCube A cube of 0/1 CATA data
#' @return a 4 * J matrix.
#' Row 1 gives the value of chi2
#' Row 2 gives the uncorrected p-value
#' Row 3 gives the  Sidak corrected (for multiple
#' comparisons)  p-value
#' Row 4 gives the  Bonferroni corrected (for multiple
#' comparisons)  p-value
#' @author Herve Abdi
#  #' @import coin # do not seem to need it
#' @export
#
Q4CATA.Cube <- function(ZeCube){
  # Compute Cochran's Q
  # for a Cube of CATA Data
  # Rows   are Products
  # Colums are Variables
  # Third Dimension are Assessors
  # 1 mean Assessor chose Variable for Product
  res4Q.12 <- apply(ZeCube,2,Q4CATA.Slice)
  #aPF   = 1 - (1 - alpha)^(1/nVars)
  nC = ncol(res4Q.12) # How many attributes
  pS <- 1 - (1 - res4Q.12[2,])^nC # Sidak correction for multiple tests
  pB <-  res4Q.12[2,]*nC # Bonferroni correction for multiple tests
  # pB can get larger than 1 so all p > 1 are set to 1
  pB[pB >  1] <- 1
  res4Q <- matrix(0,4,nC)
  rownames(res4Q) <- c('chi2','p','Sidak p(FW)','Bonferroni p(FW)')
  colnames(res4Q) <- colnames(res4Q.12)
  res4Q[1:2,] <- res4Q.12
  res4Q[3,] <- pS
  res4Q[4,] <- pB
  return(res4Q)
} # End of function Q4CATA.Cube
## -------------------------------------------------------------------
