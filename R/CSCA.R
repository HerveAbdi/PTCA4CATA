#First pass at multiBlockCA -----------------------------------------
# function blockCA
# Created Hervé Abdi. April 4 2018.
# Idea:
# We have a set K I*K matrices X_k suitable for CA
# with X = mean of all X_k
# We decompose the CA of [X_1, ... X_k, .. X_K]
# as CA of [X,..., X, ...., X] and
# CA of [X_1 - X, ..., X_k - X, ..., X_K - X]
# The analysis of X is a plain CA with centers r and c
# and metric diag{1/r} and diag{1/c}
# The analysis of the matrix [X_1, ... X_k, .. X_K]
# is done with the metrics and centers of X;
# the analysis of  [X_1 - X, ..., X_k - X, ..., X_K - X]
# is uncentered with the metrics of X.

# sinew::makeOxygen(ldiag)
# rdiag & ldiag ----
# Two other local functions ----------------------------------------
# to replace left and right diag multiplication
#' @title  Left (i.e., pre) Multiply a matrix by a diagonal matrix
#'
#' @description  \code{ldiag}: Left (i.e., pre) Multiply
#' a matrix by a diagonal matrix (with only
#'  the diagonal elements being given).
#' @param y a \eqn{I} element
#' vector (of the diagonal elements of an \eqn{I} by \eqn{I} matrix)
#' @param X an \eqn{I} by \eqn{J} matrix.
#' @return an \eqn{I} by \eqn{J} matrix equal
#' to diag(\strong{y}) %*% \strong{X}.
#' @author Hervé Abdi
#' @rdname ldiag
#' @seealso \code{\link{rdiag}}
#' @examples
#' \dontrun{
#'  Z = ldiag(y, X)
#'  }
#' @export
ldiag <- function(y,X){
  nR <- length(y)
  nC <- ncol(X)
  return(matrix(y, nrow = nR, ncol = nC, byrow = FALSE) * X)
}
#_____________________________________________________________________
# rdiag preamble ----
#'@title  right (i.e., post) Multiply a matrix by a diagonal matrix
#'
#' @description \code{rdiag}: right (i.e., post) Multiply
#' a matrix by a diagonal matrix (with only
#'  the diagonal elements being given).
#' @param y a \eqn{J} element
#' vector (of the diagonal elements of a \eqn{J} by \eqn{J} matrix)
#' @param X an \eqn{I} by \eqn{J} matrix.
#' @return an \eqn{I} by \eqn{J} matrix equal to
#' \strong{X} %*% diag(\strong{y}).
#' @author Hervé Abdi
#' @seealso \code{\link{ldiag}}
#' @rdname rdiag
#' @examples
#' \dontrun{
#'  Z = rdiag(X,y)
#'  }
#' @export
rdiag <- function(X,y){
  nC <- length(y)
  nR <- nrow(X)
  return(X * matrix(y, nrow = nR, ncol = nC, byrow = TRUE))
}
# __________________________________________________________________

# Partial Projection Here -------------------------------------------
# ___________________________________________________________________
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(blockProj)
#
# Preambule blockProj ------------------------------------------------
# ___________________________________________________________________
#' @title Compute the partial projections for blocks in
#' 3-way Correspondence Analysis (as in block-PTCA)
#' @description \code{blockProj}:
#' Computes the partial projections for blocks in
#' 3-way Correspondence Analysis (e.g., as in block-PTCA).
#' Compute the projection for blocks on one set (e.g., rows) from
#' the reconstitution formula from the other set (e.g., columns).
#'
#' @param data An \eqn{I} * \eqn{J} data matrix structured in
#' \eqn{K} blocks of rows, each described by the same
#' \eqn{J} variables.
#' @param b.weights the weights for the \eqn{K} blocks.
#' Can be a \eqn{K} by 1 vector or a scalar if all blocks
#' have same weight.
#' @param nI the number of rows of the blocks,
#' Can be a \eqn{K} by 1 vector or a scalar if all blocks
#' have same number of rows.
#' @param FS An \eqn{I} * \eqn{L} matrix (with
#' \eqn{L}: number of factors of the analysis) storing
#' the factor scores for the \eqn{I}-set obtained from
#'  correspondence analysis.
#' @param sv The singular values obtained from the
#' analysis of the whole data table.
#' @param data.metric (a \eqn{J} by 1 vector) stores
#' the metric for the columns of \code{X}. When
#' \code{NULL} (default) \code{blockProj} uses
#' the standard transition formula and first transform the
#' data into colum profiles. When the CA was not a standard CA
#' (i.e., different centers or different metrics),
#' \code{data.metric} is used to normalize the data matrix
#' prior to the barycentric projection.
#' @return a \eqn{J} (variables) by \eqn{L} (factors)
#' by \eqn{K} (blocks) array storing the \eqn{K} "slices"
#' of partial factor scores.
#' @details This function is used when the original data table is
#' made of blocks of rows (resp columns) all described by the
#' same columns (resp. rows). In the row case, the rows
#' are clustered in blocks of size
#' \eqn{I_}1, ... ,\eqn{I_k}, ... \eqn{I_K}
#' (with sum \eqn{I_K} = \eqn{I}) all described by \eqn{J} variables
#' (i.e., columns). \code{blockProj} computes the variables
#' (i.e., the \eqn{J}-set) partial projections
#' of the \eqn{K} blocks of rows. The partial projections
#' are barycentric to the whole set of projections
#' (i.e., the barycenters off all \eqn{K}-blocks is
#' equal to the factors for the whole matrix.
#' The projections are obtained from the standard reconstitution
#' formula generalized to the case of blocks with each block
#' having a \eqn{b}-weight
#' (with \eqn{b_k} > 0, and sum \eqn{b_k} = 1).
#' When the data are obtained from a standard CA, the
#' parameter \code{data.metric} does not need to be used.
#' When the data are obtained from an unconventional CA
#' (i.e., as performed  with the decomposition
#' in common and specific factors), \code{data.metric}
#' gives the metric needed; for example,
#' for the decomposition
#' in common and specific factors, the specific analysis
#' isperformed with the same meetric aas the common analysis.
#' @references Escofier, B. (1983).
#' Analyse de la difference entre deux mesures definies
#' sur le produit de deux memes ensembles.
#' \emph{Les Cahiers de l'Analyse des Donnees, 8}, 325-329.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso genCA
#' @rdname blockProj
#' @author Herve Abdi
#' @export

blockProj <- function(data, # the data to project
                      # (nI*nK) * nJ
                      b.weights, # the weights for the blocks
                      # can be a scalar or a vector
                      nI, # size of the blocks
                      # can be a scalar or a vector
                      FS, # the factor scores on the other side
                      # (nI*nK) * nF [nF: # of factors]
                      sv, # The singular values
                      data.metric = NULL

){
  nJ <- ncol(data)
  nInK <- nrow(data)
  if (nrow(FS) != nInK){stop('data and FS should have same number or rows')}
  nF <- ncol(FS) # number of factors
  if (length(nI) == 1){# nI is a  scalar
    nK <- nInK / nI
    if(nK*nI != nInK){stop('Incompatible values of nI and size of data')}
    nI <- rep(nI,nK)
  } else {
    if(sum(nI) != nInK){stop('Incompatible value of nIs and size of data')}
  } # end if else
  if (is.null(data.metric)){ # standard approach
  C.profiles <-  (apply(data, 2, function(x){ z <- x / sum(x)} ) )
           } else {# centers are provided
  C.profiles <- rdiag(data,data.metric)
           } # end of profile
  block.G <- array(NA, c(nJ,nF,nK)) # initialize array
  # a horrible loop
  debut <- 1 # initialize
  for (k in 1:nK){
    fin   <- sum(nI[1:k])
    lindex <- debut:fin
    block.G[,,k] <- (1/b.weights[k]) * t(C.profiles[lindex,]) %*%
      FS[lindex,] %*% diag(1/sv[1:nF])
    debut <- fin + 1
  }
  dimnames(block.G)[[1]] <- colnames(data)
  dimnames(block.G)[[2]] <- colnames(FS)
  dimnames(block.G)[[3]] <- names(b.weights)
  return(block.G)
} # End of function blockProj
# End of blockProj ---------------------------------------------------
# ____________________________________________________________________
# ____________________________________________________________________
# Function genCA to generalize CA to the case of
# different weight schemas and centering.
# Needed to accommadate the analysis of the difference table.
# ____________________________________________________________________
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(genCA)
#
#Preambule genCA -----------------------------------------------------
# ____________________________________________________________________
# A genCA with choice of the masses and weights.
#' Generalized Correspondence Analysis with choice of
#' row and column metrics and choice of row and column centers
#'
#' \code{genCA}: Implements
#' Generalized Correspondence Analysis with choice of
#' row and column metrics and choice of row and column centers.
#'
#' @details The current version is not optimized for
#' computational efficiency and uses the
#' general SVD function \code{ExPosition::genPDQ()}.
#'
#' @param X an \eqn{I} by \eqn{J} data table suitable for
#' correspondence analysis.
#' @param nfact (Default: 3) number of factors to keep.
#' @param normalize.X  (Default: \code{TRUE}), when
#' \code{TRUE} normalize \code{X} to a sum of 1 (as in standard CA).
#' @param r.metric (Default: \code{NULL}),
#' an eqn{I} by 1 vector of the row-metric.
#' when \code{NULL}
#' use the standard row-metric from CA
#' i.e., the  inverse of the sum of the rows of
#' normalized X).
#' @param c.metric
#' (Default: \code{NULL}),
#' a \eqn{J} by 1 vector of the columns-metric,
#' when \code{NULL}
#' use the standard column-metric from CA
#' i.e., the  inverse of the sum of the columns of
#' normalized X).
#' @param r.center
#' (Default: \code{NULL}),
#' an \eqn{I} by 1 vector of the row-center.
#' when \code{NULL}
#' use the standard row-center from CA
#' i.e., the sum of the rows of
#' normalized X).
#' @param c.center
#' (Default: \code{NULL}),
#' a \eqn{J} by 1 vector of the row-center.
#' when \code{NULL}
#' use the standard column-center from CA
#' i.e., the sum of the colums of
#' normalized X).
#' @return A list with 1) $\code{fi}: the row factor scores, 2)
#' 2) $\code{fj}: the columns factor scores.
#' @examples
#' # Use the colorOfMusic Example from PTCA4CATA
#' data(colorOfMusic)
#' resCA <- genCA(colorOfMusic$contingencyTable)
#' # gives a standard CA of these data
#' @author Herve Abdi
#' @rdname genCA
#' @export
#' @importFrom ExPosition genPDQ

# function starts here
genCA <- function(X, nfact = 3,
                   normalize.X = TRUE,
                   r.metric = NULL,
                   c.metric = NULL,
                   r.center = NULL,
                   c.center = NULL
){
  # __________________________________________________________________
  if (normalize.X){
    xpp <- sum(X)
    X = X/xpp }
  # xpp <- sum(X)
  if (is.null(r.metric)) r.metric <- 1/rowSums(X)
  if (is.null(c.metric)) c.metric <- 1/colSums(X)
  if (is.null(r.center)) r.center <-  rowSums(X)
  if (is.null(c.center)) c.center <-  colSums(X)
  if (length(r.center) == 1) r.center <-  rep(r.center,nrow(X))
  if (length(c.center) == 1) c.center <-  rep(c.center,ncol(X))
  X <- X - r.center %*% t(c.center)
  gSVD_X <-  genPDQ(X, M = r.metric, W = c.metric)
  ev.X   <- gSVD_X$Dv^2
  In.X   <- sum(ev.X)
  tau.X  <-  round(100*ev.X/In.X)
  #F.X    <- diag(r.metric) %*% gSVD_X$p %*% gSVD_X$Dd
  #G.X    <- diag(c.metric) %*% gSVD_X$q %*% gSVD_X$Dd
  F.X    <- rdiag(ldiag(r.metric, gSVD_X$p), gSVD_X$Dv)
  G.X    <- rdiag(ldiag(c.metric, gSVD_X$q), gSVD_X$Dv)
  if (length(ev.X) > nfact){
    F.X <- F.X[,1:nfact]
    G.X <- G.X[,1:nfact]
    } # end if
  colnames(F.X) <- paste0('Dimension ',1:ncol(F.X)) -> colnames(G.X)
  rownames(F.X) <- rownames(X)
  rownames(G.X) <- colnames(X)
  return.list <- structure( list(
    fi = F.X, fj = G.X,
    Dv = gSVD_X$Dv,
    eigs = ev.X,
    tau = tau.X,
    Inertia = In.X
  ), class = 'genCA')
  return(return.list)
} # end of GenCA
# end of genCA -------------------------------------------------------
# ____________________________________________________________________
# ____________________________________________________________________
# Print function for 'genCA' ----
#*********************************************************************
# ____________________________________________________________________
#' Change the print function for objects of the class
#' \code{'genCA'} (e.g., from function
#' \code{genCA})
#'
#' Change the print function for objects of the class
#' \code{'genCA'} (e.g., from function
#' \code{genCA}).
#'
#' @param x a list:
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.genCA <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list. Output of genCA (CA with choice of metrics and centers)")
  cat("\n", rep("-", ndash), sep = "")
  cat("\n$fi      ", "An I*L matrix of row factor scores ")
  cat("\n$fj      ", "A  J*L matrix of column factor scores ")
  cat("\n$Dv      ", "The vector of the singular values")
  cat("\n$eigs    ", "The vector of the eigen values")
  cat("\n$Inertia ", "The total Inertia (sum of eigen values)")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.genCA ----
# ____________________________________________________________________

# ____________________________________________________________________

# Start Common and Specific --------------------------
# Still need a good name
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(CSCA)
# ____________________________________________________________________
# function to start here
# CSCA ----
# Parameters of the function here ------------------------------------
#' @title Common and Specific Correspondence Analysis
#' (CSCA)
#' of a set of \eqn{K} matched matrices, each of order \eqn{I*J}.
#'
#' @description \code{CSCA}: implements
#' Common and Specific Correspondence Analysis
#' of a set of  \eqn{K}
#'  matched matrices, each of order \eqn{I*J}.
#'
#' @param brickOfMat an \eqn{I} items by \eqn{J} descriptors by
#' \eqn{K} blocks (e.g., matrices) suitable
#' for correspondence analysis (i.e., all non-negative elements).
#' @param nfact (Default = 3) number of factors to keep.
#' @param b (Default = \code{NULL})
#' a \eqn{K} elements weight vector for the matrices
#' (should all be positive and sum to 1). When \code{NULL}
#' \code{CSCA} computes the weights as the sum of each matrix
#' divided by the grand total. Note that it is general better
#' to have pre-normalized the matrices for CSCA
#' (e.g., with \code{normBrick4PTCA}),
#' so that all matrices have the same weight.
#' @return A list with
#' 1) \code{allMatrices.resCA}: results for the analysis of
#' the whole set of matrices stacked on top of each other;
#' 2) \code{sumOfMatrices.resCA}:
#' results (from \code{ExPosition::epCA}) for the analysis of
#' the sum (i.e., average with CA) of all matrices;
#' 3) \code{diffOfMatrices}:
#' results for the analysis of
#' the difference of the matrices to their average (from 2);
#' 4) \code{partialProjOnSum}:
#' The projection as supplementary elements of the matrices
#' onto their average;
#' and 5) \code{RvCoefficients}:
#' the matrix of \code{Rv}-coefficient between the matrices.
#'  # projZonDif.fi
#'
#' \code{allMatrices} is a list
#' containing a)
#' \code{fi}: the \eqn{I*K} by \code{nfact} matrix of the
#' row factor scores;
#'  b) \code{fj}:
#'   the \eqn{J*}\code{nfact}{*K} array
#'    by \code{nfact} array of the
#' column factor scores;
#' c) \code{Dv}: the singular values;
#' d) \code{eigs}: the eigenvalues;
#' e) \code{tau}: the percentage of Inertia; and
#' f) \code{Inertia} the total inertia;
#'
#' \code{sumOfMatrices} is a list storing the output of
#' the plain correspondence analysis of the
#' \code{I*J} matrix of the sum of matrices as
#' analyzed by \code{ExPosition::epCA} (see help there for
#' more details).
#'
#'  \code{diffOfMatrices}
#'  is a list
#' containing a)
#' \code{fi}: the \eqn{I*K} by \code{nfact} matrix of the
#' row factor scores; b) \code{fj}:
#'   the \eqn{J*}\code{nfact}{*K} array
#'    by \code{nfact} array of the
#' column factor scores;
#'  b) \code{part.fj}: A  \eqn{J*L*K}
#' array of partial column factor scores;
#' c) \code{projZonDif.fi}:
#' An (I*K) by L matrix of  the
#' projection of the original data
#' onto the specific space (useful to explore the difference
#' induced by the original data matrices);
#' d) \code{Dv}: the singular values;
#' e) \code{eigs}: the eigenvalues;
#' f) \code{tau}: the percentage of Inertia;
#'  and
#' g) \code{Inertia} the total inertia.
#'
#' \code{partialProjOnSum} is a
#' list
#' containing a)
#' \code{fi}: the \eqn{I*K} by \code{nfact} matrix of the
#' (supplementary) row factor scores;
#' b) \code{fj}:
#'   the \eqn{I*}\code{nfact}{*K} array
#'    by \code{nfact} array of the (supplementary)
#' column factor scores.
#'
#' @details The analysis of the three matrices whose results are
#' given in
#' \code{allMatrices.resCA}, \code{sumOfMatrices.resCA}, and
#' \code{diffOfMatrices} implement an ANOVA like decomposition
#' of the Inertia such that All = Sum + Difference or
#' \strong{X}_\eqn{K}  =  \strong{G} +
#' (\strong{X}_\eqn{K} - \strong{G}), with
#' \strong{X}_\eqn{K} being the set of matrices,
#' \strong{G} being the sum matrix
#' (which in CA is the barycenter of all the matrices)
#' and (\strong{X}_\eqn{K} - \strong{G})
#' being the set of the differences of all the matrices
#' to their barycenter.
#'
#' The matrix \strong{X}_\eqn{K}
#' is obtained by stacking all the original matrices
#' on top of each other
#' (so \strong{X}_\eqn{K} is an \eqn{I*K} by \eqn{J} matrix);
#' the
#' matrix (\strong{X}_\eqn{K} - \strong{G})
#' is obtained by subtracting from each
#' matrix in \strong{X}_\eqn{K} the matrix \strong{G}
#' (so \strong{X}_\eqn{K} - \strong{G}
#' is an \eqn{I*K} by \eqn{J} matrix);
#' the matrix \strong{G}
#' is obtained as the sum of all the matrices
#' in \strong{X}_\eqn{K} (so \strong{G}
#' is a \eqn{I} by \eqn{J} matrix).
#'
#' The analysis of the matrix \strong{G} is made with
#' a standard CA program, but the correspondence analysis
#' of matrices (\strong{X}_\eqn{K} - \strong{G})
#' needs a special
#' CA program because this CA uses the row and column
#' metrics from \strong{G},
#' \strong{X}_\eqn{K} uses the same centers as \strong{G},
#' whereas the (\strong{X}_\eqn{K} - \strong{G})
#' matrix is uncentered
#' (these analyses are performed
#' by the function \code{genCA}
#' that allow specific metrics and centers).
#' For the analysis of \strong{X}_\eqn{K} and
#' (\strong{X}_\eqn{K} - \strong{G})
#' the row factor scores (\code{fi}) are
#' computed from the plain \code{genCA}
#' analysis and the column factor
#' scores (\code{fj})
#' are obtained from partial projection using the
#' correspondence analysis transition formula adapted to blocks
#' of matrices.
#'
#' Note that in a two table version
#' the partial column factor scores will be identical
#' (and identical to the overall column factor score) and
#' so in this case, the overal column factor scores can
#' be plotted.
#'
#' Note, also, that the two table version
#' of  CSCA could be obtained
#' from the analysis of the
#' [\strong{X}_1 \strong{X}_2 || \strong{X}_2 \strong{X}_1]
#' circulant matrix
#' (see Greenacre, 2003).
#' @seealso normBrick4PTCA genPCA
#' @references The ideas used here are derived from:
#'
#' 1) Escofier, B. (1983). Analyse de la différence
#' entre deux mesures définies sur le produit de deux mêmes
#' ensembles. \emph{Les Cahiers de l'Analyse des Données, 8}, 325-329.
#'
#' 2) Escofier, B., & Drouet, D. (1983). Analyse des différences
#' entre plusieurs tableaux de fréquences.
#' \emph{Les Cahiers de l'Analyse des Données, 8}, 491-499;
#'
#' 3) Benali, H., & Escofier, B. (1990).
#' Analyse factorielle lissée et analyse factorielle
#' des différences locales
#' \emph{Revue de statistique appliquée, 38}, 55-76.
#'
#' 4) Greenacre, M. (2003). Singular value decomposition of
#' matched matrices. \emph{Journal of Applied Statistics, 30},
#' 1101-1113; and
#'
#' 5)
#' Takane Y. (2014). \emph{Constrained Principal Component Analysis
#' and Related Techniques}, Boca Raton: CRC Press.
#' @importFrom ExPosition epCA
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ExPosition]{epCA}}
#' @rdname CSCA
#' @export
#' @importFrom ExPosition epCA supplementaryRows supplementaryCols
#'
CSCA <- function(brickOfMat,  # An array: The set of K matrices
                 nfact = 3,   # how many factors to keep
                 b = NULL     # in case of a priori weights
                 ){# CSCA starts here
# ____________________________________________________________________
# function starts here
# Check that the Inertia of [A | B] relative to the
# common barycenter is the same In.all
# Get needed parameter from data -------------------------------------
nI <- dim(brickOfMat)[[1]]
nJ <- dim(brickOfMat)[[2]]
nK <- dim(brickOfMat)[[3]]
x.tot <- sum(brickOfMat)
# Get the Z matrix
# x  <- rep(NA, nK)
zBrick <- brickOfMat / x.tot
# get the b-weights if not provided with
if (is.null(b)) {b <- apply(brickOfMat, 3,sum) / x.tot}
names(b) <- dimnames(brickOfMat)[[3]]
# create X
X <- apply(brickOfMat,c(1,2),sum)
# Get Z, r and c
Z <- X / x.tot
r.Z <- rowSums(Z)
c.Z <- colSums(Z)
# Centers for CA
r <- as.vector(kronecker(b,r.Z) )
c <- as.vector(c.Z)
# Unfold the bricks --------------------------------------------------
# Unfold the cube. Get the Total variance
Z.K  <- aperm(zBrick, c(1,3,2) )
dim(Z.K) <- c(nI*nK,nJ)
colnames(Z.K) <- dimnames(brickOfMat[[2]])
rNamesZ <-  paste0( rep(dimnames(brickOfMat)[[1]],nK),'-',
                    rep(dimnames(brickOfMat)[[3]], each = nI))
rownames(Z.K) <- rNamesZ
colnames(Z.K) <- dimnames(brickOfMat)[[2]]
# Create the matrix (b_k*Z) K times
b.K <- kronecker(b,rep(1,nI))
Z.long <- kronecker(rep(1,nK),Z) * matrix(b.K,nI*nK,nJ)
dimnames(Z.long) <- dimnames(Z.K)
# difference to the mean matrix: Specific components
Z.K_M <- Z.K - Z.long
# Multiple SVD Total = Common + Specific -----------------------------
# Now check the different SVD
resCA.all <- genCA(Z.K,
                   nfact = nfact,
                   normalize.X = FALSE,
                   r.metric = 1/r, c.metric = 1/c,
                   r.center =  r, c.center = c)
# # Check reconstitution formula
# #R.K      <- t(apply(Z.K, 1, function(x){ z <- x / sum(x)} ))
# C.K      <-  (apply(Z.K, 2, function(x){ z <- x / sum(x)} ) )
# test.G.K <- t(C.K) %*% resCA.all$Fi %*% diag(1/resCA.all$sv[1:3])
# G.K <- array(NA, c(nJ,nfact,nK)) # initialize array
# # a horrible loop to be functionalized
# for (k in 1:nK){
#   lindex <- (nI*(k-1) + 1) : (k*nI)
#   G.K[,,k] <- (1/b[k]) * t(C.K[lindex,]) %*%
#             resCA.all$Fi[lindex,] %*% diag(1/resCA.all$sv[1:nfact])
# }
#
# compare here with function blockProj()
#
G_K <- blockProj(Z.K, b, nI, resCA.all$fi, resCA.all$Dv)
# works cf, altCA.Z.AtopZ_B In.all
# Sum matrix Z = \sum_k b_k Z_k
# equivalent to the analysis of the sum matrix
# old code used for testing that
# resCA.Z give the same results as plain CA on the sum matrix
# resCA.Z <- genCA(Z.long,  nfact = nfact,
#            normalize.X = FALSE,
#             r.metric = 1/r, c.metric = 1/c,
#             r.center =  r, c.center = c )
# works cf. test4.AtopB In.AB
resCA.K_M <- genCA(Z.K_M, nfact = nfact,
                   normalize.X = FALSE,
                   r.metric = 1/r, c.metric = 1/c,
                   r.center =  0, c.center = 0)
# Add projection of Z onto the difference fj
# Z onto fi._ ----
projZonDif.fi  <- ldiag(1/(2*r), Z.K)  %*%
  rdiag(resCA.K_M$fj, 1/resCA.K_M$Dv[1:nfact])
# Works cf. In.A_B
# Transition formula for the difference
#test.Fj <- diag(1/c) %*% t(Z.K_M) %*%
#                resCA.K_M$Fi %*% diag(1/resCA.K_M$sv[1:nfact])
# partial Projection
test.Gk.K_M <- blockProj(Z.K_M, b, nI,
                     resCA.K_M$fi, resCA.K_M$Dv,
                     data.metric = 1/c)
# Works gives the same value a Fj.A_B from the circulant
# check with Fi.A_B and Fj.A_B
#_____________________________________________________________________
# Partial FS as Sup ----
#_____________________________________________________________________
# Create Partial Factor Scores
#_____________________________________________________________________
# First run the analysis with the whole of Z

resCA.Z_whole <- ExPosition::epCA(Z, graphs = FALSE, k = nfact)
# Get the Products / Rows supplementary projections
sup.Fi <- ExPosition::supplementaryRows(SUP.DATA = Z.K,
                            res = resCA.Z_whole)$fii[,1:nfact]
colnames(sup.Fi) <- paste0("Dimension ",1:nfact)

partial.G_K <- array(NA, c(nJ,nfact,nK))
for  (k in 1:nK){
# get the index
  lindex <- ((k - 1)*nI + 1):(k*nI)
  partial.G_K[,,k] <- ExPosition::supplementaryCols(
                          SUP.DATA = Z.K[lindex,],
                          res = resCA.Z_whole)$fjj[,1:nfact]
   }
dimnames(partial.G_K) <- dimnames(G_K)
# Rv matrix
Cmat4C5  <- createCmat4PTCA(brickOfMat, normalization = 'rv')
# Save all the Results -----------------------------------------------
# List for each components ----
allMatrices.resCA <- structure(list(
  fi = resCA.all$fi,
  fj = G_K,
  Dv = resCA.all$Dv,
  eigs = resCA.all$eigs,
  tau = resCA.all$tau,
  Inertia = resCA.all$Inertia),
  class = 'csca.block'
)
sumOfMatrices.resCA <- resCA.Z_whole$ExPosition.Data
diffOfMatrices.resCA <- structure(list(
  fi = resCA.K_M$fi,
  fj = resCA.K_M$fj,
  partial.fj = test.Gk.K_M,
  projZonDif.fi = projZonDif.fi,
  Dv = resCA.K_M$Dv,
  eigs = resCA.K_M$eigs,
  tau = resCA.K_M$tau,
  Inertia = resCA.K_M$Inertia),
  class = 'csca.block.diff'
)
partialProjOnSum.resCA <- structure(list(
  fi = sup.Fi,
  fj = partial.G_K),
  class = 'csca.block.part'
)

return.list <- structure(list(
  allMatrices.CA    = allMatrices.resCA,
  sumOfMatrices     = sumOfMatrices.resCA,
  diffOfMatrices    = diffOfMatrices.resCA,
  partialProjOnSum  = partialProjOnSum.resCA,
  RvCoefficients =  Cmat4C5),
class = 'csca')

return(return.list)
} # end of function CSCA
# ____________________________________________________________________
# end of function CSCA -----------------------------------------------
# ____________________________________________________________________


# ____________________________________________________________________
#' Change the print function for objects of the class
#' \code{csca}
#'
#' Change the print function for objects of the class
#' \code{csca}.
#'
#' @param x a list:
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.csca <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list. Output of CSCA (Common and Specific CA, from PCTA4CATA)")
  cat("\n         Correspondence Analysis of a set of K matched I*J matrices")
  cat("\n", rep("-", ndash), sep = "")
  cat("\n$allMatrices.CA   ", "List: results for all the matrices stacked.")
  cat("\n$sumOfMatrices    ", "List: results for the sum of the matrices.  ")
  cat("\n$diffOfMatrices   ", "List: results for the difference matrices.")
  cat("\n$partialProjOnSum ", "List: results for the projections as supplementary")
  cat("\n                  ", "      of the matrices onto the sum matrix.")
  cat("\n$RvCoefficients   ", "Matrix: K*K between matrices Rv-coefficient matrix.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.csca.block  ----
# ____________________________________________________________________
# ____________________________________________________________________
#' Change the print function for objects of the class
#' \code{csca.block.part}
#'
#' Change the print function for objects of the class
#' \code{csca.block.part}.
#'
#' @param x a list:
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.csca.block.part <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list. Output of CSCA (Common and Specific CA, from PCTA4CATA)")
  cat("\n         Supplementary Projections onto the Common Space")
  cat("\n", rep("-", ndash), sep = "")
  cat("\n$fi      ", "An (I*K)*L matrix of row factor scores ")
  cat("\n$fj      ", "A  J*L*K array of column factor scores ")
  #cat("\n$Dv      ", "The vector of the singular values")
  #cat("\n$eigs    ", "The vector of the eigen values")
  #cat("\n$Inertia ", "The total Inertia (sum of eigen values)")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.csca.block.part ----
# ____________________________________________________________________
# ____________________________________________________________________
#
#' Change the print function for objects of the class
#' \code{csca.block}
#'
#' Change the print function for objects of the class
#' \code{csca.block}.
#'
#' @param x a list:
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.csca.block <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list. Output of CSCA (Common and Specific CA, from PCTA4CATA)")
  cat("\n         Supplementary Projections onto the Common Space")
  cat("\n", rep("-", ndash), sep = "")
  cat("\n$fi      ", "An (I*K)*L matrix of row factor scores ")
  cat("\n$fj      ", "A  J*L*K array of column factor scores ")
  cat("\n$Dv      ", "The vector of the singular values")
  cat("\n$eigs    ", "The vector of the eigen values")
  cat("\n$Inertia ", "The total Inertia (sum of eigen values)")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.csca.block ----
# ____________________________________________________________________
# ____________________________________________________________________
# ____________________________________________________________________
#
#' Change the print function for objects of the class
#' \code{csca.block.diff}
#'
#' Change the print function for objects of the class
#' \code{csca.block.diff}.
#'
#' @param x a list:
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.csca.block.diff <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list. Output of CSCA (Common and Specific CA, from PCTA4CATA)")
  cat("\n         Analysis of the Specific Components (deviation to barycenter)")
  cat("\n", rep("-", ndash), sep = "")
  cat("\n$fi            ", "An (I*K)*L matrix of row factor scores ")
  cat("\n$fj            ", "A  J*K matrix of column factor scores ")
  cat("\n$part.fj       ", "A  J*L*K array of partial column factor scores ")
  cat("\n$projZonDif.fi ","An (I*K) by L matrix of  the ")
  cat("\n               ","   projection of the original data onto the specific space ")
  cat("\n$Dv            ", "The vector of the singular values")
  cat("\n$eigs          ", "The vector of the eigen values")
  cat("\n$Inertia       ", "The total Inertia (sum of eigen values)")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.csca.block ----
# ____________________________________________________________________
# ____________________________________________________________________

