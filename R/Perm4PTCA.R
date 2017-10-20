#
# Work File For the correct permutation test for CATA
#
# Contains functions: eig4CA

# function to give the eigenvalues of a CA matrix
#'  computes the eigenvalues for
#' correspondence analysis (CA)
#'
#' eig4CA computes the eigenvalues for
#' correspondence analysis (CA). Needs a matrix
#' of non-negative numbers. The trivial eigenvalue
#' equals to 1, is not computed.
#' NB for computational efficiency,
#'  the matrix diagonalized is NOT the
#' matrix diagonalized in CA but it has the
#' same eigenvalues.
#' NB:  rows and columns with 0 sums have an inverse set to 0
#' to avoid infinities.
#' @param X a matrix of non-negative numbers
#' @author Herve Abdi
#' # @export
eig4CA <- function(X){# give the eigenvalues of a CA matrix
  # NB the matrix diagonalized is NOT the
  # matrix diagonalized in CA but it has the
  # same eigenvalues.
  # Author Herve Abdi. October 31, 2016.

  # first make sure that we always
  # diagonalize the smaller matrix
  if (dim(X)[1] > dim(X)[2]){ X <- t(X)}

  P  <- X / sum(X)
  # Get Masses etc.
  r = rowSums(P)
  c = colSums(P)
  Pcent <- P - as.matrix(r)%*%c
  r_12 <- 1/sqrt(r)
  r_12[r==0] <- 0 # fix problem with divide by 0
  c_12 <- 1/sqrt(c)
  c_12[c==0] <- 0 # fix problem with divide by 0
  # Two ways of avoiding multiplication by diag matrices
  # so 
  # Y <- diag(r_12) %*% Pcent %*% diag(c_12)
  # 1. a la repmat ...
  #nI <- NROW(P)
  #nJ <- NCOL(P)
  #M12 <- matrix(r_12, nrow = nI, ncol = nJ  )
  #W12 <- matrix(c_12, nrow = nI, ncol = nJ, byrow = TRUE)
  #Y <- as.matrix(M12 * Pcent * W12)
  # 2. Equivalent (marginaly faster)
  #
   Y <- t(t( r_12 * Pcent) * c_12)
  #S = Y %*% t(Y)
  eig4CA <- eigen( Y %*% t(Y),
                   symmetric = TRUE,
                   only.values = TRUE)$values
  # Make sure that only positive eigenvalues are kept
  eig4CA <- eig4CA[eig4CA >= 0]
  return(eig4CA)
} # End eig4CA
#==============================================================================

#' truc4ptca: a private function
#'
#' truc4ptca a private function: create a permuted version of a data cube
#' from a PTCA analysis. Used by the function \code{perm4PTCA}.
#' @param aCube A cube of PTCA Data.
#' @param longueur the number of eigenvalues of the
#' data table
#' (default is rank of the data table -1).
#' @param permType the type of permutation used can be
#' "byRows', 'byCols', or 'byMat
#' @author Herve Abdi
truc4ptca <- function(aCube,
                  longueur = min(dim(aCube)[1:2])-1,
                  permType = permType){
  # Do the permutation for a CATA matrix
  # default permutation is byRows
  # other possible values is byCols or  byMat
  # nN = length(X)
  #valP   <- rep(0, longueur)
  #resvp <- eig4CA( apply(X,2,sample ))
  # 'byRows' is the default if a incorrect option is chosen
  if (!(permType %in% c('byRows','byCols','byMat'))) permType <- 'byRows'
  # Select the type of permutation
  if (permType == 'byMat'){
    # permute the colums and then the rows when byMat
    cubeRand <- apply(aCube,c(2,3),sample )
    cubeRand <- aperm(apply(cubeRand,c(1,3),sample ),
                      perm=c(2,1,3))
  }
  if ( permType == 'byRows'){# the Rows
    cubeRand <- aperm(apply(aCube,c(1,3),sample ),
                      perm=c(2,1,3))
  }
  if ( permType == 'byCols'){#The Colums
    cubeRand <-  apply(aCube,c(2,3),sample )
  }
  # get the sum of the cube to get a random matrix
  Xrand <- apply(cubeRand,c(1,2),sum)
  # Clean empty columns and empty rows if any
  if (any(rowSums(Xrand)==0)){Xrand <-  Xrand[-which(rowSums(Xrand)==0),]}
  if (any(colSums(Xrand)==0)){Xrand <-  Xrand[,-which(colSums(Xrand)==0)]}
  # Call eig4CA
  vpCA  <-   eig4CA(Xrand)
  valP <-     rep(0, longueur)
  valP[1:length(vpCA)] <- vpCA
  return(valP)
} # end of truc4ptca
#==============================================================================
# function perm4PTCA
#
#--------------------------------------------------------------------
#' perm4PTCA Permutation test for the inertia and eugenvalues for PTCA
#'
#' Permutation for PTCA4CATA.
#' Computes an omnibus permutation test and
#' specific tests for the eigenvalues when
#' performing a PTCA analysis on Check All That Apply
#' Data. Input is a Cube of data (products*Descriptors*Assessors).
#' Three different permutation schemes
#' are currently available (see paramater
#' \code{permType}).
#' @param aCube an I*J*K (i.e., products*Descriptors*Assessors)
#' of CATA Data.
#' @param nIter (Default = 1000). Number of Iterations
#' (i.e. number of permuted samples computed).
#' @param permType what type of permutation is used
#' if 'byRows' (default) the data are permuted within
#' the rows (i.e., products) for each assessor.
#' if 'byCols'  the data are permuted within
#' the columns (i.e., descriptors) for each assessor.
#' if 'byMat' the data are permuted within
#' the whole data matrix (i.e., products*descriptors)
#' for each assessor.
#' @param compact if TRUE return
#' (Default) only p-values for omnibus test
#' @param Malinvaud (default = TRUE) return the results
#' of the Malinvaud-Saporta chi square test
#' @return a list with
#' \code{fixedInertia}: the CA-inertia of the data matrix;
#' \code{fixedEigenvalues}: the CA-eigenvalues of
#' the data matrix;
#' \code{pOmnibus}: the probability associated
#' to the inertia.
#' If \code{compact} is \code{FALSE}, return also
#' \code{permInertia}:
#' an \code{nIter} * 1 vector containing the
#' permutated inertia;
#' \code{pEigenvalues}: The probabilites
#' associated to each eigenvalue;
#' If \code{compact} is is \code{FALSE}, return also
#' \code{permEigenvalues}: an
#' \code{nIter} * \code{L} matrix giving
#' the permuted eigenvalues.
#' @author Herve Abdi
#' @export


perm4PTCA <- function(aCube,
                      nIter = 1000,
                      permType = 'byRows' , # 'byCols', 'byMat'
                      compact = FALSE,
                      Malinvaud = TRUE
){# function perm4PTCA stars here
  # 'byRows' is the default if a incorrect option is chosen
  if (!(permType %in% c('byRows','byCols','byMat'))) permType <- 'byRows'
  # First get the fixed effect model
  Xfixed <-  apply(aCube,c(1,2),sum)
  maxRank <- min(dim(Xfixed)) - 1
  fixedEigenvalues <- rep(0,maxRank)
  fixedEV <- eig4CA(Xfixed)
  # Make sure that the length fit
  if (length(fixedEV) > maxRank){
    fixedEigenvalues <- fixedEV[1:maxRank]
  }
  if (length(fixedEV) == maxRank){fixedEigenvalues <- fixedEV}
  if (length(fixedEV) < maxRank){
    fixedEigenvalues[1:length(fixedEV)] <- fixedEV
  }
  fixedInertia <- sum(fixedEigenvalues)
  # The random permutations below
  # Initialize
  permInertia     <- rep(NA,nIter)
  permEigenvalues <- matrix(NA, nrow = nIter, ncol = maxRank)
  #
  # Use replicate
  # with a private function?
  laLongueur <- maxRank + 1 # to fix rounding error for ev
  permEigenvalues <- replicate(nIter,
                               truc4ptca(aCube = aCube,
                                          laLongueur,permType) )
  permEigenvalues <- t(permEigenvalues[1:maxRank,])
  # Done without a loop!
  permInertia = rowSums(permEigenvalues)
  #
  pOmnibus = sum(permInertia > fixedInertia) / nIter
  if (pOmnibus == 0) pOmnibus <- 1/nIter # no 0
  pEigenvalues <- rowSums( t(permEigenvalues) >
                             (fixedEigenvalues)) / nIter
  pEigenvalues[pEigenvalues == 0 ] <- 1/nIter
  return.list <- structure(
    list(fixedInertia = fixedInertia,
         fixedEigenvalues = fixedEigenvalues,
         pOmnibus = pOmnibus,
         pEigenvalues = pEigenvalues
    ),
    class = 'perm4ptca')
  if (!compact){
    return.list$permInertia =  permInertia
    return.list$permEigenvalues = permEigenvalues
  }
  if (Malinvaud){
    res4Malin <- return.list
    if (compact){
      res4Malin$permInertia =  permInertia
      res4Malin$permEigenvalues = permEigenvalues
               }
    res.malin <- Malinvaud4ptca(Data =  Xfixed,
                                # The original Data Table
                                res.perm4ptca  = res4Malin,
                                # the permuted eigenvalues
                                # as a niteration * rank of X matrix
                                ndigit4print = 4
                                # how many digits for printing
    )
    return.list$MalinvaudQ <- res.malin
  }
  return(return.list)
}  # End of function perm4PTCA
#===============================================================================
# *******************************************************************************
#' Change the print function for perm4ptca class
#'
#'  Change the print function for perm4ptca class
#'  objects
#'  (output of Perm4PTCA)
#'
#' @param x a list: output of perm4ptca
#' @param ... everything else for the functions
#' @author Herve Abdi
#' @export
print.perm4ptca <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Results of Permutation Tests for PTCA analysis (CATA data)  \n")
  cat(" for Omnibus Inertia and Eigenvalues \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ fixedInertia     ", "the Inertia of the contingency table (CT)")
  cat("\n$ fixedEigenvalues ", "an L*1 vector of the eigenvalues of CT")
  cat("\n$ pOmnibus         ", "the probablity associated to the Inertia")
  cat("\n$ pEigenvalues     ", "an L*1 vector of p for the eigenvalues of CT")
  cat("\n$ permInertia      ", "vector of the permuted Inertia for CT")
  cat("\n$ permEigenvalues  ", "matrix of the permuted eigenvalues of CT")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n$ MalinvaudQ        ", "result of Malinvaud-Saporta Q-test")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.perm4ptca
#------------------------------------------------------------------------------
#==============================================================================
# NewMalivaudTest4CA
# a compact version of Malinvaud-Saparta Test.
# Hervé Abdi. November 6, 2016. Revised as part of PTCA4CATA
# June 27, 2017.
#-------------------------------------------------------------------------------
# MalinvaudQ4CA
#===============================================================================
#' computes the Malinvaud-Saporta test for
#' significance in correspondence analysis
#'                   with asymptotically
#'                   (and, if provided permutation)
#'                    derived p-values
#'
#'
#' Compute the Malinvaud-Saporta test for significance
#' in Correspondence Analysis. Provides asymptotic
#' Chis-square based p-values, and if a set
#' of permuted eigenvalues is provided
#' (obtained, e.g., from \code{Perm4MultCA} or
#' from \code{perm4PTCA})
#' provides _p_-values based on permutation tests.
#'References:
#' Malinvaud, E. (1987). Data Analysis in applied socio-economic statistics
#' with special considerations of correspondence analysis.
#' Marketing Science Conference Proceedings, HEC-ISA, Jouy-en-Josas.
#' Also cited in Saporta (2011).
#' Probabilité et Analyse des Données (3rd Ed).
#'    Technip, Paris. p. 209.
#'@param Data the contingency table used for CA
#'@param res.perm4ptca The results from perm4PTCA
#' that gives the eigenvalues obtained from a permutation test
#' @param ndigit4print (Default = 4) number of digits
#' to use to print the results
#' @author Herve Abdi
#' @return: A dataframe with the results of the test
#' @export

# Rewrite for coherence with PTCA4CATA
Malinvaud4ptca  <-   function(Data, # The original data table
                             res.perm4ptca,
                             # the permuted eigenvalues
                             # as a niteration * rank of X matrix
                             ndigit4print = 4
                             # how many digits for printing
){# Function begins here
  # References:
  # Malinvaud, E. (1987). Data Analysis in applied socio-economic statistics
  # with special considerations of correspondence analysis.
  # Marketing Science Conference Proceedings, HEC-ISA, Jouy-en-Josas.
  # Also cited in Saporta (2011).
  # Probabilité et Analyse des Données (3rd Ed).
  #    Technip, Paris. p. 209.

  Val.P = res.perm4ptca$fixedEigenvalues
  #
  N.pp = sum(Data)
  # Compute eigenvalues
  nL <- length(Val.P) # how many eigenvalues
  nI <- nrow(Data)
  nJ <- ncol(Data)
  Q     <- N.pp * cumsum(Val.P[nL:1])[nL:1]
  Q.nu  <- (nI - 1:nL)*(nJ - 1:nL)
  # Get the values from Chi Square
  pQ = 1 - pchisq(Q,Q.nu)
  # Add NA to make clear that the last
  #     dimension is not tested
  Le.Q = c(Q,NA)
  Le.pQ = c(pQ,NA)
  Le.Q.nu = c(Q.nu,0)
  #
  NamesOf.Q = c('Malinvaud-Saporta Test. Ho: Omnibus', paste0('Dim-',seq(1:nL)))
  names(Le.Q)    <- NamesOf.Q
  names(Le.pQ)   <- NamesOf.Q
  names(Le.Q.nu) <- NamesOf.Q
  #
  # if we have the permuted eigenvalues
  # use them to derive the p-values
  permutedEigenValues <-  res.perm4ptca$permEigenvalues
  if (!is.null(permutedEigenValues)){# If LesEigPerm exist get p-values
    Q.perm     <-  N.pp*t(apply(permutedEigenValues[,nL:1],1,cumsum) )[,nL:1]
    #
    Logical.Q.perm =  t(t(Q.perm) >  (Q))
    pQ.perm = colMeans(Logical.Q.perm)
    pQ.perm[pQ.perm == 0] = 1 / nrow(Q.perm)
    Le.pQ.perm = c(pQ.perm,NA)
    # return the table
    Malinvaud.Q = data.frame(matrix(c(
      c(round(c(sum(Val.P),Val.P),digits=ndigit4print) ),
      round(Le.Q,      digits=ndigit4print),
      round(Le.pQ,     digits=ndigit4print),
      round(Le.Q.nu,   digits=ndigit4print),
      round(Le.pQ.perm,digits=ndigit4print)),
      nrow = 5, byrow=TRUE))
    colnames(Malinvaud.Q) = NamesOf.Q
    rownames(Malinvaud.Q) = c('Inertia / sum lambda',
                              'Chi2',
                              'p-Chi2','df',
                              'p-perm')
  } else {
    Malinvaud.Q = data.frame(matrix(c(
      c(round(c(sum(Val.P),Val.P),digits=ndigit4print) ),
      round(Le.Q,      digits=ndigit4print),
      round(Le.pQ,     digits=ndigit4print),
      round(Le.Q.nu,   digits=ndigit4print)
      # ,round(Le.pQ.perm,digit=ndigit4print)
    ),
    nrow = 4, byrow=TRUE))
    colnames(Malinvaud.Q) = NamesOf.Q
    rownames(Malinvaud.Q) = c('Inertia / sum lambda',
                              'Chi2',
                              'p-Chi2','df'
                              #,'p-perm'
    )
  }
  return(Malinvaud.Q)
} # End of function MalinvaudQ4CA here
#
#==============================================================================
