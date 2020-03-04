#_____________________________________________________________________
# Functions for package PTCA4CATA
# Here the functions are for Cochran Q
#  functions in this file: Q4CATA.Slice Q4CATA.Cube
#
#
# Work in progress. Hervé Abdi
# Started October 16, 2016.
# Last update  October 16, 2016.
#
#_____________________________________________________________________
#_____________________________________________________________________
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890
#_____________________________________________________________________
# 4. Taylor made functions from  CATA_Example4Bangkok.Rmd
#_____________________________________________________________________

# Preamble Q4CATA.Slice ----
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
#' \eqn{x(i,j) =} 1 if \eqn{p(i)} was chosen by \eqn{a(j)}, 0 if not.
#'
#' @param Data4Q a slice of CATA cube
#' @importFrom coin symmetry_test statistic pvalue
#' @return a vector with the value of \code{chi2}
#'  and \code{pvalue}.
#' @author Hervé Abdi
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
  pval <- coin::pvalue(Res4Q)
  chi2   <- coin::statistic(Res4Q)
  # Qres = list(p = pvalue, chi2 = chi2)
  Qres <- c(chi2, pval)
  return(Qres)
} # End of function Q4CATASlice
#
#_____________________________________________________________________
## Preamble Q4CATA.Cube ----
# Q4CATA.Cube
#' @title Compute Cochran's \eqn{Q}
#' for a Cube of CATA Data.
#'
#' @description Compute Cochran's \eqn{Q}
#' for a Cube of CATA Data
#' (as created, e.g., by \code{readCATAfromXL}):
#' \code{I}: Rows   are Products,
#' \code{J}: Columns are Variables,
#' \code{K}: Third Dimension are Assessors.
#' \eqn{x_{i,j,k} =} 1 means
#' Assessor \eqn{k} chose Variable \eqn{j} for Product \eqn{i}.
#' NB. uses function \code{Q4CATA.Slice}.
#'
#' @param ZeCube A cube of 0/1 CATA data
#' @return a 4 * \eqn{J} matrix.
#' Row 1 gives the value of chi2
#' Row 2 gives the uncorrected p-value
#' Row 3 gives the  Sidak corrected (for multiple
#' comparisons)  \eqn{p}-value
#' Row 4 gives the  Bonferroni corrected (for multiple
#' comparisons)  \eqn{p}-value
#' @author Hervé Abdi
#  #' @import coin # do not seem to need it
#' @export
#
Q4CATA.Cube <- function(ZeCube){
  # Compute Cochran's Q
  # for a Cube of CATA Data
  # Rows   are Products
  # Columns are Variables
  # Third Dimension are Assessors
  # 1 mean Assessor chose Variable for Product
  res4Q.12 <- apply(ZeCube,2,Q4CATA.Slice)
  #aPF   = 1 - (1 - alpha)^(1 / nVars)
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
##_____________________________________________________________________

