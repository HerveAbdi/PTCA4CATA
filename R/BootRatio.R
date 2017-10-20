# This file contains the computational routines for PTCA4CATA
# Current functions here:
# boot.ratio.test()
# Boot4PTCA()
# InertiaPermutedTables()
# InertiaTable()
# DataCheckMark2Cube()
# vec2gray()
# Created August 05, 2016 by Hervé Abdi
# Documented with roxygen2
# Uptdates. August 07. HA / October 17 / 2016.
# June 9 2017. HA

# *****************************************************************************
# The functions start below
# *****************************************************************************
# function from Derek for bootstrap ratios
#' boot.ratio.test computes bootstrap ratios
#' from a "bootstrap cube"
#'
#' boot.ratio.test computes bootstrap ratios
#' from a "bootstrap cube" (created, e.g., by
#' BOOT4PTCA)
#'
#' @author Derek Beaton & Hervé Abdi
#' @param boot.cube An I*L*B bootstrap brick
#' (typically obtained from Boot4PTCA).
#' The third dimension (B) corresponds to the
#' random factor of the
#' "to-be-bootstrapped-units"
#' (e.g., judges, participants, assessors)
#' @param critical.value The critical value for significance
#' (default = 2, which matches a \eqn{p <} .05 significance level)
#' @return A list: 1) sig.boot.ratios: A logical vector that identifies the
#' significant items. 2)  boot.ratios: the bootstrap ratios
#' 3) prob.boot.ratios: the (uncorrected) probability associated to the
#' bootstrap ratios 4) prob.boot.ratios.cor:
#' the (Sidak/Bonferonni) corrected probability associated to the
#' bootstrap ratios
#' @examples #  BR <- boot.ratio.test(BootI)
#' @export
boot.ratio.test <- function(boot.cube,critical.value=2){
  boot.cube.mean <- apply(boot.cube,c(1,2),mean)
  boot.cube.mean_repeat <- array(boot.cube.mean,dim=c(dim(boot.cube)))
  boot.cube.dev <- (boot.cube - boot.cube.mean_repeat)^2
  s.boot<-(apply(boot.cube.dev,c(1,2),mean))^(1/2)
  boot.ratios <- boot.cube.mean / s.boot
  significant.boot.ratios <- (abs(boot.ratios) > critical.value)
  prob.boot.ratios <-  dnorm(abs(boot.ratios))
  ncomp =  dim(boot.cube)[1]
  prob.boot.ratios.corr <- 1 - (1 - prob.boot.ratios)^(1/ncomp)
  rownames(boot.ratios) <- rownames(boot.cube)
  rownames(significant.boot.ratios) <- rownames(boot.cube)
  rownames(prob.boot.ratios) <- rownames(boot.cube)
  rownames(prob.boot.ratios.corr) <- rownames(boot.cube)
  return.list <-  structure(
         list(boot.ratios = boot.ratios,
              prob.boot.ratios = prob.boot.ratios,
              prob.boot.ratios.corr = prob.boot.ratios.corr,
              sig.boot.ratios = significant.boot.ratios),
         class = "bootRatios")

  return(return.list)
}
# *****************************************************************************
# *******************************************************************************
#' Change the print function for bootRatios
#'
#'  Change the print function for bootRatios
#'
#' @param x a list: output of bootRatios
#' @param ... everything else for the functions
#' @author Herve Abdi
#' @export
print.bootRatios <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Bootstrap Ratios (BR)\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$boot.ratios           ", "The Bootstrap ratios")
  cat("\n$prob.boot.ratios      ", "Probability associated with the BRs")
  cat("\n$prob.boot.ratios.corr ", "Corrected probability associated with the BRs")
  cat("\n$sig.boot.ratio        ", "Logical: gives the significant BRs")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.bootRatios
#--------------------------------------------------------------------



# *****************************************************************************
# function Boot4PTCA. Compute the Bootstrapped Factors scores for I and J sets
#'Boot4PTCA. Compute the Bootstrapped Factors scores
#' for the  I and J sets from
#' a Partial Triadic Correspondence analysis
#' (PTCA)
#'
#' This function bootstraps the Kth dimension of a data cube
#' and computes bootstraped factor scores
#' @param ZeDataCube An I*J*K data cube (K are observations)
#' The third dimension (K) is bootstrapped
#' @param fi  The factor scores for I (rows) from the epCA program
#' @param fj  The factor scores for J (columns) from the epCA program
#' @param eigs  The eigenvalues from the epCA program
#' @param nf2keep  how many factors to keep,  default to 2
#' @param nBootIter How many Bootstrap samples, default to 100
#' (RowsBoot = ZeBootCube_I,ColumnsBoot = ZeBootCube_J)
#' @param compact (default = FALSE) if \code{TRUE} gives a compact
#' version with only the result for the symmetric approch.
#' @return A list: if compact false:
#' 1a) RowsBoot a I*L*B cube of Bootstrapped
#' coordinates for the I-set
#' 1b) RowsBoot.asym a I*L*B cube of Bootstrapped
#' coordinates for the I-set
#' (asymmetric projections);
#'  2a)  ColumnsBoot a J*L*B cube of Bootstrapped coordinates
#'  for the J-set
#'  if compact false 2b)
#'  ColumnsBoot.asym a J*L*B cube of Bootstrapped
#'  coordinates for the J-set
#'  with I: number of rows, J: number of columns
#'  L: number of factors kept (i.e., nf2keep),
#'  B: number of Bootstrap replicates (i.e., nBootIter)
#' @author Herve Abdi
#' @examples #  BootFactorsIJ <-Boot4PTCAt(A.Cube.Of.Data,fi=fi,fj=fj,eigs=eigs)
#' @export
Boot4PTCA <- function( # Boot4PTCA: Create Bootstraped
  # factor scores for I & J set
  # from the CA obtained by the sum of individual contigency tables
  # stores in a observations * variables * individuals
  # with individuals being a random factors
  #*****************    Hervé Abdi. January, 17, 2013 *************
  ZeDataCube,  # The Cube of Data
  fi, # The factor scores for I (rows) from the CA program
  fj, # The factor scores for J (columns) from the CA program
  eigs, # The eigenvalues from the CA analysis
  nf2keep = 2, # how many factors to keep,  default to 2
  nBootIter = 100 , # How many Bootstrap samples, default to 100
  compact = FALSE # If compact == TRUE: give only the symetric version
){
  # ***************************************************************
  # Output:
  #     RowsBoot   : a I * nf2keep * nBootIter array
  #                  of bootstraped factor scores (row)
  #     ColumnsBoot: a J * nf2keep * nBootIter array
  #                   of bootstraped factor scores (columns)
  # first check that nf2keep is not too big
  nL = length(eigs)
  if (nf2keep > nL){nf2keep = nL}
  nI = dim(ZeDataCube)[3] # how many observation do we have
  # compute the multiplication matrices
  Lefj = fj[,1:nf2keep] # the columns factor scores
  Lefi = fi[,1:nf2keep] # the row factor scores
  invdelta = eigs[1:nf2keep]^(-1/2)
  MultmatI.asym = t(t(Lefi) * invdelta^2) # Multiplying matrix I-set asymmetric
  MultmatJ =  t(t(Lefj) * invdelta) # Multiplying matrix J-set
  if (!compact){
  MultmatI =  t(t(Lefi) * invdelta) # Multiplying matrix I-set
  MultmatJ.asym = t(t(Lefj) * invdelta^2) # Multiplying matrix I-set asymmetric
  }
  # initialize the Bootstrap cube
  ZeBootCube_I.asym <- array( dim=c(nrow(ZeDataCube),nf2keep,nBootIter)
                              #, dimnames=c('I-set','Factors','Replicates')
  )
  ZeBootCube_J <- array( dim=c(ncol(ZeDataCube),nf2keep,nBootIter)
                         # ,dimnames=c('J-set','Factors','Replicates')
  )
  if (!compact){
    ZeBootCube_I <- array( dim=c(nrow(ZeDataCube),nf2keep,nBootIter)
                           #, dimnames=c('I-set','Factors','Replicates')
  )
   ZeBootCube_J.asym <- array( dim=c(ncol(ZeDataCube),nf2keep,nBootIter)
                              #, dimnames=c('J-set','Factors','Replicates')
  )
  }

  # Now create I & J Bootstrap Factors Scores for the Cube
  # now go for an ugly loop
  for (m in 1:nBootIter){
    BootCT = apply(ZeDataCube[,,sample(nI,replace = TRUE)],c(1,2),sum)
    # get the Fi scores
    ZeBootCube_I.asym[,,m] = apply(BootCT,2,
                                   function(la) {
                                     la/as.matrix(rowSums(BootCT))}) %*%
      MultmatJ.asym
    ZeBootCube_J[,,m] = apply(t(BootCT),2,
            function(la) {
                la/as.matrix(rowSums(t(BootCT)))}) %*% MultmatI
    if (!compact){
      ZeBootCube_I[,,m] = apply(BootCT,2,
                                function(la) {
                                  la/as.matrix(rowSums(BootCT))}) %*% MultmatJ
    ZeBootCube_J.asym[,,m] <- apply(t(BootCT),2,
                                     function(la) {
                                   la/as.matrix(rowSums(t(BootCT)))}) %*%
                                   MultmatI.asym
                     } # end of !compact
         } # End of m-loop
  # Return the ZeBootCubes
  Names_of_I <- rownames(ZeDataCube)
  Names_of_J <- colnames(ZeDataCube)
  Names_of_F <- paste0('Factor ',seq(1,nf2keep))
  Names_of_Iter <- paste0('Iter ',seq(1,nBootIter))
  rownames(ZeBootCube_I.asym) <- Names_of_I
  rownames(ZeBootCube_J) <- Names_of_J
  colnames(ZeBootCube_I.asym) <- Names_of_F
  colnames(ZeBootCube_J) <- Names_of_F
  if (!compact){
  rownames(ZeBootCube_I) <- Names_of_I
  rownames(ZeBootCube_J.asym) <- Names_of_J
  colnames(ZeBootCube_I) <- Names_of_F ->
                                colnames(ZeBootCube_J.asym)
  }

  # dimnames(ZeBootCube_I) <- list(Names_of_I,Names_of_F,Names_of_Iter)
  return.list <- structure(
      list(RowsBoot.asym = ZeBootCube_I.asym,
           ColumnsBoot = ZeBootCube_J),
       class = "Boot4PTCA")
  if (!compact){
    return.list$RowsBoot = ZeBootCube_I
    return.list$ColumnsBoot.asym = ZeBootCube_J.asym
  }

  return(return.list)

}  # End of function Boot4PTCA
# *******************************************************************************
#' Change the print function for Boot4PTCA
#'
#'  Change the print function for Boot4PTCA
#'
#' @param x a list: output of Boot4PTCA
#' @param ... everything else for the functions
#' @author Herve Abdi
#' @export
print.Boot4PTCA <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Brick of Bootstraped Factor Scores (BFS) from a 0/1 CATA Cube\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$RowsBoot.asym    ", "an I*L*nIter Brick of BFSs for the I-Set (Asymmetric)")
  cat("\n$ColumnsBoot      ", "a  J*L*nIter Brick of BFSs for the J-Set")
  cat("\n$RowsBoot         ", "an I*L*nIter Brick of BFSs for the I-Set")
  cat("\n$ColumnsBoot.asym ", "a  J*L*nIter Brick of BFSs for the J-Set (Asymmetric)")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.cBoot4PTCA
#--------------------------------------------------------------------





#*********************************************************************
# Random permutations
# uses the InertiaTable function
#'InertiaPermutedTables
#' A function to Compute the inertia of a set of random permutations
#' of the "Check-Mark" (e.g. CATA) type of data.
#'
#' InertiaPermutedTables creates a cube of data
#' from the results of a "Check-Mark"
#' data set collected in DataChecks.
#' These data correspond to participants matching (or not)
#' one Descriptor to
#' each stimulus of a set of stimuli.
#' The Stimuli are the columns of DataChecks
#' The Participants are the rows of DataChecks
#' The Descriptors are the numbers in DataChecks
#' (i.e., 5 for Datacheks[2,3] means that Participant 2,
#' choosed Descriptor 5 for Stimulus 3)
#' @author Hervé Abdi
#' @param DataChecks An I*J matrix
#' storing integers.
#' I are Participants, J are Stimuli
#' The entries in Datacheks are integers that match the descriptors
#' (i.e., 5 for Datacheks[2,3] means that Participant 2
# ' choosed Descriptor 5 for Stimulus 3)
#'@param nPerm number of random permutations (default = 1000).
#' Note that the number of Descriptors is "guessed" by the program
#' as the largest number is the dataset
#' @return  # returns a 1*nPerm vector with the nPerm values
#'  of the inertia computed with the nPerm random Permutations
#' @examples # RandomnInertia <- InertiaPermutedTables(ACubeOfDataChecks)
#' @export
InertiaPermutedTables <- function(DataChecks, nPerm = 1000){
  #	Compute the inertia of a set of random permutations
  # of the "Check-Mark" type of data
  # Create a cube of Data from the results of a "Check-Mark"
  # data set collected in DataChecks
  # These data corresponds to participants matching One Descriptor to
  # each stimulus of a set of stimuli
  # The Stimuli are the columns of DataChecks
  # The Participants are the rows of DataChecks
  # The Descriptors are the numbers in DataChecks
  #     (i.e., 5 for X[2,3] means that Participants 2,
  #         choosed Descriptor 5 for Stimulus 3)
  # nPerm = number of random permutations (default = 1000)
  #Note that the number of Descriptors is "guessed" by the program
  # as the largest number is the dataset
  # return a 1*nPerm vector with the nPerm values
  #  of the inertia computed with random Permutations

  #  nI: # of Participants
  nI = nrow(DataChecks)
  # nJ # of stimulis
  nJ = ncol(DataChecks)
  # nK # number of Descriptors
  nK = max(DataChecks)
  # Descriptor by Stimuli by Participants
  # Initialize the tables
  RandomInertia = matrix(nrow =nPerm,ncol=1)
  ZeDataCubePerm = array(0,dim=c(nK,nJ,nI))
  # Go for the nPerm permutation samples
  for (ell in 1:nPerm){ # Ugly loop for the permutations
    # For the permutation test
    # the choice of each subject is permuted
    # (i.e., the colors chosen
    # are kept but they are assigned randomly to the pieces of music)
    #
    # first permute the data per subject
    PerIndex =(
      t(replicate(nI,sample(nJ,replace=FALSE)))
      +
        matrix(rep( seq(from=0,by=nJ,length.out=nI),nJ),nI,nJ)
    )
    # the permuted data matrix
    truc = matrix(as.vector(
               t(as.matrix(DataChecks)))[PerIndex],
               nI,nJ,byrow = FALSE)
    # Get a permuted cube
    for (i in 1:nI){ # Another ugly loop for randomization
      LeTableau = matrix(0,nK,nJ) # Initialize
      lesUns = seq(from=0,by=nK,length=nJ)+as.vector(t(truc[i,]))
      LeTableau[lesUns] = 1
      ZeDataCubePerm[,,i] = LeTableau
    } # end of "i" loop
    # get the Inertia of the  permuted DataTable
    RandomInertia[ell] = InertiaTable(apply(ZeDataCubePerm,
                                                 c(1,2),sum) )
  } # End of "ell" loop
  return(RandomInertia)
} # End of function InertiaPermutedTables
# *******************************************************************************

#********************** Inertia of a Table***************************************
#' InertiaTable  A function to compute the inertia of a contingency table
#'
#' InertiaTable  computes the inertia (i.e., phi square or chi-square / N)
#' of a contingency table. The computation matches the value obtained
#' from the correspondence analysis of the contingency table.
#' Used for permutation tests.
#' @author Hervé Abdi
#' @param X A contingency table (non negative numbers)
#' @return the inertia (a la correspondence analysis) of the contingency table
#' @examples # RandomnInertia<- InertiaPermutedTables(ACubeOfDataChecks)
#' @examples # InertiaOfATable <- InertiaTable(X)
#' @export
InertiaTable = function(X){
  # Compute the inertia of a contingency table
  xpp = sum(X)
  StochPerm = X / xpp
  Inertia = sum((StochPerm -
                   as.matrix(rowSums(StochPerm))
                 %*%  t(as.matrix(colSums(StochPerm))) )^2)
}



# --------------------------------------------------------------------
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890

# ************************************************************************
# DataCheckMark2Cube function to create the Cube of Data from the checks
#' DataCheckMark2Cube Create a cube of Data
#' from the results from "Check-Mark" CATA data.
#'
#'  Create a cube of Data from the results of a "Check-Mark"
#' data set collected in DataChecks
#' These data correspond to participants matching one descriptor to
#' each stimulus of a set of stimuli
#' The Stimuli are the columns of DataChecks,
#'  The Participants are the rows of DataChecks,
#' The Descriptors are the numbers in DataChecks
#'     (i.e., 5 for X[2,3] means that Participant 2,
#'         choosed Descriptor 5 for Stimulus 3)
#' @author Hervé Abdi
#' @param DataChecks A Stimuli by Participants table of checks
#' The Stimuli are the columns of DataChecks,
#'  The Participants are the rows of DataChecks,
#' The Descriptors are the numbers in DataChecks
#'     (i.e., 5 for X[2,3] means that Participant 2,
#'         choosed Descriptor 5 for Stimulus 3)
#' @param NameOfDescriptor a length K vector of names of
#' the descriptors. if NULL (default) descriptors are
#' @return a Stimuli*Descriptors*Participants brick of counts
#' @examples # le.grey <- vec2gray(1:10)
#' @export
DataCheckMark2Cube <- function(DataChecks,NameOfDescriptor = NULL){
  # Create a cube of Data from the results of a "Check-Mark"
  # data set collected in the matrix DataChecks
  # These data correspond to participants matching one descriptor to
  # each stimulus of a set of stimuli
  # The Stimuli are the columns of DataChecks
  # The Participants are the rows of DataChecks
  # The Descriptors are the numbers in DataChecks
  #     (i.e., 5 for X[2,3] means that Participant 2,
  #         choosed Descriptor 5 for Stimulus 3)
  # NameOfDescriptor: gives the name of the Descriptor
  # if null number of Descriptors is "guessed" by the program
  # as the larger number is the dataset

  #  nI: # of Participants
  nI = nrow(DataChecks)
  # nJ # of stimulis
  nJ = ncol(DataChecks)
  # nK # number of Descriptors
  if (is.null(NameOfDescriptor)){
    nK = max(DataChecks)
    NameOfDescriptors <- paste('Descriptor',1:nJ)}
  else {nK = length(NameOfDescriptor)}
  ZeDataCube = array(0,dim=c(nK,nJ,nI))
  # Descriptor by Stimuli by Participants
  for (i in 1:nI){
    LeTableau = matrix(0,nK,nJ) # Initialize
    lesUns = seq(from=0,by=nK,length=nJ)+as.vector(t(DataChecks[i,]))
    LeTableau[lesUns] = 1
    ZeDataCube[,,i] = LeTableau
  }
  rownames(ZeDataCube) <- NameOfDescriptor
  colnames(ZeDataCube) <- colnames(DataChecks)
  dimnames(ZeDataCube)[[3]] <-  rownames(DataChecks)
  return(ZeDataCube)
} # End of DataCheckMark2Cube
# ********************************************************************

# --------------------------------------------------------------------
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890
# 3. Taylor made functions from Sorting_Example4Bangkok.Rmd
# --------------------------------------------------------------------
#' vec2gray transforms a vector of non-negative numbers into
#' gray values
#'
#' transforms a vector of non-negative numbers into
#' gray values
#' @author Hervé Abdi
#' @param levec a vector of  non negative numbers
#' @return a vector of gray values
#' @examples # le.grey <- vec2gray(1:10)
#' @import grDevices
#' @export
vec2gray <- function(levec){# get grey value
  le.grey <-  gray(1- (levec / (max(levec))))
  return(le.grey)}
# --------------------------------------------------------------------

