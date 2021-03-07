# Préambule -----
#Functions for CA Block (columns or rows) Projections
# Part of PTCA4CATA.
# Created July 8, 2017. Hervé Abdi
# Current major version: February /27/ 2021
# Revisited: March 2021. HA
# Last revision March/07/2021. HA
#________________________________________________
#________________________________________________
#
# Function partialProj4CA ----
#' Compute blocks (of columns or rows)
#' partial projections for a Correspondence Analysis.
#'
#' \code{partialProj4CA} computes blocks (of columns or rows)
#' partial projections for a Correspondence Analysis (CA).
#' Blocks are non-overlapping sets of of columns or rows
#' of a data table analyzed with
#' CA (as performed with \code{epCA} from \code{ExPosition}).
#' \code{partialProj4CA} gives
#' the partial projection for the blocks.
#' These projections are barycentric
#' because the barycenters of the
#' partial projections are equal to the factor scores
#' for the whole table.
#' @details
#' \emph{Current version does not handle blocks with only one
#' column (or row)}. This problem is due to the way R handles
#' vectors vs matrices and is likely to be fixed
#' in the (soon to come)
#'  next version.
#' @param resCA the results of the (CA) analysis
#' from \code{epCA},
#' for example \code{reFromCA <- epCA(X)}.
#' @param code4Blocks a vector indicating
#' which columns (or rows) belong
#' to what block  (i.e.,
#' the of columns or rows of the same
#' block have the same level for
#' \code{code4Block}): Needs to be of length equal to the
#' number of variables (resp. rows) of the analysis.
#' @param rowBlocks = \code{FALSE} (default).
#' When \code{TRUE},
#' \code{partialProj4CA} runs the analysis
#' on blocks of rows instead of blocks of columns and
#' exchange the roles of the of columns and rows.
#' @return a list with (1) \code{Fk}:
#' an \eqn{I*L*K} array of the partial projections
#' for the \code{L} factors (from \code{epCA})
#' of the \eqn{K} blocks, for the \eqn{I} rows
#' (if \code{rowBlock} is \code{FALSE} for the \eqn{J}
#' columns if
#' \code{rowBlock} is \code{TRUE});
#'  (2) \code{Ctrk} an \eqn{I*L} (resp. \eqn{J*L})
#' matrix of the "relative"
#' block contributions [for a given component
#' the relative contributions sum to 1];
#' (3) \code{absCtrk} an \eqn{I*L} (resp  \eqn{I*L})
#'  matrix of the "absolute"
#' block contributions [for a given component
#' the absolute contributions sum to the eigenvalue
#' for this component];
#' (4) \code{bk} a \eqn{K*}1 vector storing the weights for the blocks,
#' (5) \code{resRV} a list with
#' (a) a matrix storing the \eqn{RV} coefficients
#' between the blocks and, if the package \code{FactoMineR} is
#' installed, (b) the \eqn{p}-value for the eqn{RV}-coefficient (as
#' computed with \code{FactoMineR::coeffRV}).
#' @details In CA, the (barycentric) partial projections
#'  are obtained by rewriting the "reconstitution" formula.
#'  @references
#'  Escofier, B. (1980). Analyse factorielle
#'  de très grands tableaux
#'  par division en sous-tableaux.
#'  In Diday \emph{et al.}: \emph{Data Analysis and
#'  Informatics}. Amsterdam: North-Holland. pp 277-284.
#' @examples
#' \dontrun{
#' # Get the data/CA function from Exposition
#'  library(ExPosition)
#'  data(authors, package = 'ExPosition')
#'  X <- (authors$ca$data) # the data
#'  zeBlocks <- as.factor(c(1,1,2,2,3,3)) # 3 blocks
#'  resCA <- epCA(X, graphs = FALSE) # CA of X
#'  resPart <- partialProj4CA(resCA, zeBlocks, rowBlocks = TRUE)
#' }
#' @author Hervé Abdi
#' @export
#'
#'
partialProj4CA <- function(resCA, #output from ExPosition::epCA
                           code4Blocks, # a J*1 factor vector for blocks
                           rowBlocks = FALSE
                           # flip the roles of rows and columns
){
  #resCA <- ResCATACA  # The results of epCA
  #code4Blocks <- NewCode4Attributes # what variable belongs to what block
  code4Blocks <- as.factor(code4Blocks) # in case it is not a factor
  # Two local functions. maybe moved later on
  # How to avoid multiplying by 0.
  # Can be seen as a variation of repmat!
  leftdiagMult <-function(aVec,aMat){# begin leftdiagMult
    resmult <-  matrix(aVec,
                       nrow = nrow(aMat),
                       ncol = ncol(aMat) ) * aMat
  } # end of leftdiagMult
  rightdiagMult <-function(aMat,aVec){# begin rightdiagMult
    resmult <-  matrix(aVec,
                       nrow = nrow(aMat),
                       ncol = ncol(aMat), byrow = TRUE ) * aMat
  } # end of rightdiagMult
  if (rowBlocks){# get the dual
    Fi <-  resCA$ExPosition.Data$fj
    wj <-  1 / resCA$ExPosition.Data$M
    Q <- leftdiagMult(resCA$ExPosition.Data$M,
                      resCA$ExPosition.Data$pdq$p)
    X <- (rightdiagMult( leftdiagMult(
                             resCA$ExPosition.Data$W,
                             t(resCA$ExPosition.Data$X)),
                             resCA$ExPosition.Data$M))
    cj <- resCA$ExPosition.Data$ci
  } else {
    Fi <- resCA$ExPosition.Data$fi
    wj <- resCA$ExPosition.Data$W
    X  <- resCA$ExPosition.Data$X
    Q  <- resCA$ExPosition.Data$pdq$q
    cj <- resCA$ExPosition.Data$cj
  }
  # function starts here
  nBlocks = nlevels(code4Blocks)
  Fk = array(0,dim = c(nrow(Fi), ncol(Fi), nBlocks))
  dimnames(Fk)[[1]] =  rownames(Fi)
  dimnames(Fk)[[2]] =  paste0('F.',seq(1,ncol(Fi)))
  dimnames(Fk)[[3]] =  levels(code4Blocks)
  bk = rep(0,nBlocks)
  names(bk) <- levels(code4Blocks)
  Ctrk <- matrix(0,nrow = nBlocks,ncol = NCOL(Fi))
  rownames(Ctrk) <- levels(code4Blocks)
  colnames(Ctrk) <- dimnames(Fk)[[2]]
  # Horrible Loop to find the Partial Factor Scores
   wj <- 1 / wj # New addon HA
  for (k in 1:nBlocks){# Begin k loop
    BlockVar = code4Blocks == levels(code4Blocks)[k]
    bk[k]  <-  sum(wj[BlockVar]) # try 2
    Xk =  X[, BlockVar]
    Qk =  Q[BlockVar,]
    Wk = diag(wj[BlockVar]) # old
    Wk = diag(1 / wj[BlockVar])  # new
    Fk[,,k]  = (1/bk[k]) * Xk%*%Wk%*%Qk
    # Compute the contributions for the Blocks
    Ctrk[k,] = colSums(cj[BlockVar,])
  } # End k loop
  # Absolute contributions
  absCtrk <-t(resCA$ExPosition.Data$eigs * as.data.frame(t(Ctrk)))
  # RV Coefficient
  resRV <- RV4Brick(Fk)
  return.list  <- structure(list(Fk = Fk,
                                 bk = bk,
                                 Ctrk = Ctrk,
                                 absCtrk = absCtrk,
                                 resRV = resRV #,
                                 #Q = Q,
                                 #X = X
  ),
  class = 'partialProj')
  #
  return(return.list)
} # End of function partialProj4CA
#________________________________________________
# Print function for class partialProj
#________________________________________________
#' Change the print function for partialProj.
#'
#'  Change the print function
#'  for partialProj class
#'  objects
#'  (e.g.,output of partialProj4CA).
#'
#' @param x a list: output of perm4ptca
#' @param ... everything else for the functions
#' @author Herve Abdi
#' @export
print.partialProj <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Results of Partial (i.e., Block) projections for a CA ")
  cat("\n I: # observations, J: # variables, K: # blocks  \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$Fk      ", "a I*L*K brick of partial (block) factor scores")
  cat("\n$bk      ", "a K*1 vector of the blocks' weights")
  cat("\n$Ctrk    ", "a K*L matrix of relative block contributions (sum to 1)")
  cat("\n$absCtrk ", "a K*L matrix of absolute block contributions (sum to lambda)")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n$resRV   ", "a list with the matrix of RV coefficients (and p) between blocks")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.partialProj
#________________________________________________
#________________________________________________
# RV2Mat -----
# A quick and dirty RV function
#' RV2Mat
#' Compute the RV coefficient between two matrices
#' with the same number of rows.
#'
#'
#' \code{RV2Mat}
#' Compute the RV coefficient between two matrices
#' with the same number of rows.
#' \code{RV2Mat}
#' first computes the cross-product matrix.
#' With X and Y matrices, RV is equal to
#' <XX',YY'> /sqrt(<XX'*XX'>*<YY'*YY'>),
#' with <> being the scalar product.
#' RV works like a squared correlation coefficient.
#' @param m1 an I*J matrix
#' @param m2 an I*K matrix
#' @return rv The RV coefficient
#' @author Herve Abdi
RV2Mat <- function(m1,m2){
  M1 <- as.matrix(m1) %*% t(as.matrix(m1))
  M2 <- as.matrix(m2) %*% t(as.matrix(m2))
  rv <-
    sum(c(M1)*c(M2)) / sqrt(sum(M1^2)*sum(M2^2))
  return(rv)}
# End of function RV2Mat ----
#________________________________________________
# RV4Brick -----
# A function to compute the RV coefficient
# on a cube of Data
#
# compute the RV coefficients between the tables
#
#' Compute the RV coefficient between the slices of a data array.
#'
#' \code{RV4Brick} computes the RV coefficient
#' between the slices of a data array,
#' such as block projection
#' for CA in PTCA4CATA.
#' @param aBrickOfData a brick of data I*J_k*K. The RV coefficient
#' is computed between the K-slices of the brick
#' @return a list with a matrix of the RV coefficients
#' (\code{$RV}), and
#' a matrix of p-values ((\code{$pRV})).
#' @author Herve Abdi
#' @export

RV4Brick <- function(aBrickOfData){
  nBlocks <- dim(aBrickOfData)[3]
  RV <- matrix(0,nBlocks,nBlocks)
  colnames(RV) <- dimnames(aBrickOfData)[[3]]
  rownames(RV) <- dimnames(aBrickOfData)[[3]]
  #colnames(RV) <- levels(code4Blocks)
  #rownames(RV) <- levels(code4Blocks)
    pRV <- matrix(0,nBlocks,nBlocks)
    for (k1 in 1:(nBlocks-1)){
      for (k2 in (k1+1):(nBlocks)){
        lRV <-Rv( aBrickOfData[,,k1],aBrickOfData[,,k2] )
        RV[k1,k2] <- lRV$Rv
        pRV[k1,k2] <- lRV$pRv
      }
    }
    pRV <- pRV + t(pRV)
    RV <-  RV + t( RV) + diag(rep(1, ncol(RV)))
    return.list <- list(RV = RV, pRV = pRV)
  return(return.list)
}
#_________________________________________________
# Rv ----
# RV function. Hervé Abdi.
# July 14, 2017.
# Clone from HA's Matlab RV function
#
#  ERv is the expected value of RV
#  VRv is the variance
#  ZRv is the "Z-tranformed Rv"
#  pRv is the p assuming Normality
#  Ref Kazi-Aoual et al CSDA 1995
#      Schlich (1996)
# See also FactoMineR coefRV
#
#________________________________________________
#________________________________________________
# A quick and dirty RV function
#' \code{Rv}
#' Compute the RV coefficient between two matrices
#' with the same number of rows.
#'
#' \code{Rv}
#' Compute the RV coefficient between two matrices
#' with the same number of rows.
#' \code{Rv}
#' first computes the cross-product matrix.
#' With X and Y matrices, RV is equal to
#' <XX',YY'> /sqrt(<XX'*XX'>*<YY'*YY'>),
#' with <> being the scalar product.
#' Rv works like a squared correlation coefficient.
#' Depending the options chosen,
#' \code{Rv} will return the
#' \eqn{Rv} coefficient and associated \eqn{p}-values.
#' The estimation of the p-values are made following
#' Kazi-Aoual et al. (CSDA 1995).
#' See also Schlich (1996),
#' and Abdi (2010, 2007).
#' @param m1 an \eqn{I*J} matrix
#' @param m2 an \eqn{I*K} matrix
#' @param return.type what is in the "returned list."
#' \code{"compact"} (default) returns \code{Rv} and (pRv),
#' \code{"Rv"} returns \code{Rv}, \code{"full"} return
#' \code{Rv}, \code{ERv},  \code{VRv},
#'  \code{ZRv},  \code{pRv}.
#' @return a list
#' with, depending upon the option
#' chosen in \code{return.type}, some (or all) of these:
#'   \code{Rv} (RV coefficient),
#'   \code{ERv} (Expected value of Rv),
#'   \code{VRv} (Variance of Rv),
#'   \code{ZRv} (Z-score for Rv),  \code{pRv} (p-value for Rv).
#' @references
#' Abdi, H. (2007). Multiple correlation coefficient.
#'   In N.J. Salkind (Ed.):
#'   \emph{Encyclopedia of Measurement and Statistics.}
#'   Thousand Oaks (CA): Sage. pp. 648-651.
#' @references Abdi, H. (2010). Congruence:
#' Congruence coefficient,
#' RV coefficient, and Mantel Coefficient.
#'  In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.):
#'   \emph{Encyclopedia of Research Design}.
#'   Thousand Oaks (CA): Sage. pp. 222-229.
#'   @importFrom stats pnorm
#' @export
#' @author Herve Abdi
Rv <- function(m1,m2, return.type = 'compact'){
  if (nrow(m1) != nrow(m2)){
    stop('The input matrices need to have the same number of rows')
  }
  #________________________________________________
  #________________________________________________
  # First local functions
  le_beta <- function(W){
    l <- eigen(W,only.values = TRUE,symmetric = TRUE)$values
    b <- ( sum(l)^2 ) /sum(l^2)
    return.list = list(l = l, b = b)
    return(return.list)
  }
  #________________________________________________
  le_delta <- function(dd,l){
    d <- sum(dd^2) / sum(l^2)
    return(d)
  }
  #________________________________________________
  le_c <- function(n,b,d){
    c = ( (n-1)*(n*(n+1)*d-(n-1)*(b+2))) / ( (n-3)*(n-1-b))
    return(c)
  }
  # end of local functions
  #________________________________________________
  #________________________________________________
  if ((return.type != 'full') & (return.type != 'Rv')){
    return.type <- 'compact' # enforce the default
  }
  W1 <- as.matrix(m1) %*% t(as.matrix(m1))
  W2 <- as.matrix(m2) %*% t(as.matrix(m2))
  Rv <- sum(c(W1)*c(W2)) / sqrt(sum(W1^2)*sum(W2^2))
  if (return.type != 'Rv'){
    n = nrow(m1)
    b1l1 <- le_beta(W1)
    b1 <- b1l1$b
    l1 <- b1l1$l
    b2l2 <- le_beta(W2)
    b2 <- b2l2$b
    l2 <- b2l2$l
    ERv = sqrt(b1*b2) / (n-1)
    d1 = le_delta(diag(W1),l1)
    d2 = le_delta(diag(W2),l2)
    c1 = le_c(n,b1,d1)
    c2 = le_c(n,b2,d2)
    num = (n-1-b1)*(n-1-b2)*(2*n*(n-1)+(n-3)*c1*c2)
    den = (n+1)*n*((n-1)^3)*(n-2)
    VRv = num/den
    ZRv = (Rv-ERv) / sqrt(VRv)
    # NB  Proz is bilateral here
    pRv = 2*(1 - stats::pnorm(abs(ZRv)))
  }
  if (return.type == 'Rv'){
    return.list <- structure(list(Rv = Rv),
                             class = 'rvCoeff')
  }
  if (return.type == 'compact'){
    return.list <-  structure(list(Rv = Rv, pRv = pRv),
                              class = 'rvCoeff')
  }
  if (return.type == 'full'){
    return.list <-  structure(list(Rv = Rv, pRv = pRv,
                                   ERv = ERv, VRv = VRv,
                                   ZRv=ZRv),
                              class = 'rvCoeff')
  }
  return(return.list)
}
# End of function Rv
#________________________________________________
# Print function for class rvCoeff
#________________________________________________
#' Change the print function for rvCoeff.
#'
#'  Change the print function for rvCoeff class
#'  objects
#'  (e.g.,output of Rv).
#'
#' @param x a list: output of \code{rvCoeff}
#' @param ... everything else for the function
#' @author Herve Abdi
#' @export
print.rvCoeff <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Rv Coefficient between two matrices\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$Rv     ", "The Rv coefficient between X and Y")
  cat("\n$pRv    ", "p-value testing Ho: Rv = 0")
  cat("\n$ERv    ", "Expected value of Rv under the null")
  cat("\n$VRv    ", "Variance of Rv under the null")
  cat("\n$Zrv    ", "The Z-score to test the null")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.rvCoeff
#________________________________________________
# Test ----
# # Test here
# Xraw <- matrix(c(1,6,7,5,3,2,6,1,1,7,1,2,2,5,4,3,4,4),6,3,byrow=TRUE)
# Yraw <- matrix(c(2,5,7,6,4,4,4,2,5,2,1,1,7,2,1,2,3,5,6,5,3,5,4,5),6,4,
#           byrow=TRUE)
# XXt  <- Xraw %*% t(Xraw)
# YYt  <- Yraw %*% t(Yraw)
# LeRV <- Rv(Xraw,Yraw)
#
#________________________________________________
# EoF ----
#________________________________________________
