#=====================================================================
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# A function here to norm an array
#---------------------------------------------------------------------
#'
#' \code{normBrick4PTCA} norms an \eqn{I*J*N} CATA array.
#'
#' \code{normBrick4PTCA} norms an \eqn{I*J*N} CATA array
#' (i.e., an array such that \eqn{x_{i,j,k}} >= 0),
#' by rows, by columns, by matrix
#' (i.e. by "Slice"), or by group of Participants.
#' The normalization implemented makes the sum of the normalized unit
#' (i.e., rows, columns, matrices) equal to a constant
#' (see \code{normingConstant}).
#' @param array2Norm a "to be normed" 3-D
#'  \eqn{I*J*N} CATA array
#' (i.e., an array such that x_{i,j,k} >= 0).
#' @param normingConstant (default = 1), the constant to which
#' normalization is done. For example, when the option \code{byRow}
#' is chosen and \code{normingConstant = 1},
#'  each row of the array is normalized so that its sum is equal to 1.
#' @param normalization type of normalization,
#' Current options are: \code{"byRow"} (default),
#' \code{"byCol"}, \code{"byMat"}, and \code{"byGroup"}.
#' Note that if \code{normalization = "byGroup"},
#' the parameter \code{code4Group} needs to be specified.
#' @param code4Groups (default = \code{NULL}),
#' an \eqn{N} by one factor or string vector
#' speficying to what group each matrix
#' (i.e., each of the \eqn{I*J} \eqn{N} slides) belongs.
#' Used only and required when \code{normalization = "byGroup"},
#' ignored otherwise.
#' @return a list with the \eqn{I*J*N} normalized cube. With the option
#' \code{"byGroup"}.
#' \code{normBrick4PTCA} will also return a \eqn{I*J*K} array with the
#' summed values for the groups.
#' Note that the normalizated array should be access as list.
#' So with the call \code{toto <- normBrick4PTCA(array2Norm)},
#' the returned normlized
#' array is in \code{toto$normedArray}.
#' @author Herve Abdi
#' @export
#'
normBrick4PTCA <- function(array2Norm, # A brick of CATA data
                    normingConstant = 1, # normalization constant
                    normalization = 'byRow', # Type of normalization
                    code4Groups = NULL # vector for the groups
){
  #-------------------------------------------------------------------
  # First an internal function
  # Normalization function
  norma <- function(x, normingConstant = 1 ){
    x <- x
    sx <- sum(x)
    if (sx != 0){
      x <- normingConstant*(x / sum(x))}
    return(x)
  }
  #-------------------------------------------------------------------
  CurrentNormedOptions <- c("byRow", # (default),
                            "byCol","byMat", "byGroup")
  if( (normalization == 'byGroup') & (is.null(code4Groups) )){
    stop('The option normalization = "byGroup" require a vector for code4Groups')
  }
  if (!(normalization %in% CurrentNormedOptions)){
    normalization <- CurrentNormedOptions[1] } # get default
  nI <- dim(array2Norm)[1]
  nJ <- dim(array2Norm)[2]
  nN <- dim(array2Norm)[3]
  # Norm by Row of each matrix
  if (normalization == CurrentNormedOptions[1]){
    normedArray <- aperm(  apply(array2Norm,c(1,3),
                                 norma, normingConstant  )  ,
                           c(2,1,3) )
  } # End "byRow"
  if (normalization == CurrentNormedOptions[2]){
    # Norm by Column of each matrix
    normedArray <- apply(array2Norm,c(2,3),
                         norma , normingConstant)
  } # End "byCol"
  if (normalization == CurrentNormedOptions[3]){
    # Norm by Slices (i.e., each matrix in the cube)
    normedArray  <- array(NaN,c(nI,nJ,nN))
    # Horrible loop there
    # for (n in 1:nN){
    #   normedArray.byMat[,,n] <- array2Norm[,,n] / (sum(array2Norm[,,n] ))
    # }
    # A more beautiful version sith aplly. Merci Vincent G.!
    normedArray <- array(apply(array2Norm, 3, norma, normingConstant),
                         c(nI, nJ, nN))
  } # end byMat
  if (normalization == CurrentNormedOptions[4]){
    # norm by Groups of Participants or Conditions
    Groups <- unique(code4Groups)
    nK <- length( Groups )
    normedArray.Groups  <- array(NaN,c(nI,nJ,nK))
    normedArray <- array(NaN,c(nI,nJ,nN))
    # Gaspature: A loop!
    for (k in 1:nK){
      id4Group = which(code4Groups==Groups[k] )
      mat4Group <- apply(array2Norm[,,id4Group], c(1,2),sum )
      # Normalized the Group matrix
      normedArray.Groups[,,k] <- norma(mat4Group,
                                    normingConstant = normingConstant)
      # Normalize the cube-slices to the norming constant
      normedArray[,,id4Group] <- array2Norm[,,id4Group] *
        (normingConstant / sum(mat4Group))
    } # Loop on k
  } # end of "byGroup" option
  #return results
  dimnames(normedArray)[1] <- dimnames(array2Norm)[1]
  dimnames(normedArray)[2] <- dimnames(array2Norm)[2]
  dimnames(normedArray)[3] <- dimnames(array2Norm)[3]

  return.list  <- structure(list(normedArray = normedArray),
                            class = 'normedPTCAarray')
  if (normalization == CurrentNormedOptions[4]){
    dimnames(normedArray.Groups)[1] <- dimnames(array2Norm)[1]
    dimnames(normedArray.Groups)[2] <- dimnames(array2Norm)[2]
    dimnames(normedArray.Groups)[3] <- list(Groups)
    return.list$normedArray.Groups = normedArray.Groups
    }

  return(return.list)
} # end of function normBrick4PTCA
#---------------------------------------------------------------------
#---------------------------------------------------------------------
#=====================================================================
