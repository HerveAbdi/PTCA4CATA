# A set of additional functions for PTCA4CATA
# Hervé Abdi. September 22, 2017.
#
# Boot4Mean:
#
# SupProjPartVariablesCA a function for
# correspondence analysis to project a supplemenray
# variable when we have only values for some observations
# or conversely how to project a supplementary observation
# described by only part of the variables

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Boot4Mean
#---------------------------------------------------------------------
# Bootstrap function for the factors
#' @title  Boot4Mean computes a brick of bootstrap
#' estimates for the means of groups of observations
#' described by several variables (e.g., factor scores).
#'
#' @description \code{Boot4Mean} computes a brick  of bootstrap
#' estimates for the means of groups of observations
#' described by several variables,  as for example,
#' for groups of observations described by factors scores
#' computed from a Principal Component Analysis or
#' or a Multiple Correspondence Analysis.
#' The groups of observations are identified by a design factor.
#' Observations are Bootstrapped within their groups
#' (so each Bootstrap sample has the  same
#' N per group as the original groups).
#' @param Data An observations by variables data set.
#' @param design A vector giving the design for the observations:
#' observations with the same number belong to the same group
#' @param niter (Default = \code{100})
#'  the number of bootstrap iterations.
#' @param suppressProgressBar (default = \code{TRUE}), when
#' \code{TRUE} suppress the progress bar.
#' @return A list with \code{BootCube:} a groups by
#' variables by iterations brick of bootstrapped means, and
#' \code{GroupMeans}: the groups by variables
#' original means of the groups and
#' \code{BootsrappedGroupMeans}:
#' the groups by variables bootstrapped means of
#' \code{BootCube}.
#' @export
#' @author Herve Abdi
#' @details Use with PCA-like techniques as
#' a step before plotting confidence interval ellipses for
#' group means
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname Boot4Mean

Boot4Mean <- function(Data , design,
                      niter = 100,
                      suppressProgressBar = TRUE){
  # Boostrap the means of the groups
  # According to a Design/Factor Matrix
  # Private functions
  # Get the means
  GetMean <- function(Data = Data, design = design)
  {MeanGroups = as.matrix(aggregate(Data,by = list(design),
                                    FUN = mean)[-1])
  }
  # ********* A function inspired from Derek *****************
  # get the bootstrap index values within groups
  boot.design  <- function(design){# Compute Bootstrap
    # indices according to a factor matrix
    boot.index <- vector()
    ZeGroup = as.factor(as.matrix(design))
    ValG = names(table(ZeGroup)) # values of the group
    nGroups = length(ValG)
    # how many groups of observations do we want to look at
    for(i in 1:nGroups){
      boot.index <- c(boot.index,sample(which(ZeGroup==ValG[i ]),
                                        replace=TRUE))
    }
    return(boot.index)
  }
  # **************************************************************
  # first get the fixed effect mean
  FixedMeans = as.matrix(aggregate(Data,by = list(design),
                                   FUN = mean) [-1])
  Truc = as.matrix(aggregate(Data,by = list(design),
                             FUN = mean) )
                                   # A silly way to get the names
  Nom2Row <- as.factor(Truc[,1])
  rownames(FixedMeans) <- Nom2Row
  nG = nrow(FixedMeans)
  nVar = ncol(FixedMeans)
  ZeCubeOfMeans = array(, dim = c(nG,nVar,niter))
  if (suppressProgressBar != TRUE){
    print('Starting Bootstrap.')
    pb <-txtProgressBar(min = 0, max = niter,
                        initial = 0, char = "=",
                        title = 'Bootstrap Iterations', style = 1)
  }
  for (m in 1:niter){ # Bootstrap loop
    BootInd = boot.design(design)
    ZeCubeOfMeans[,,m] =  GetMean(Data[BootInd,],design[BootInd])
    if (suppressProgressBar != TRUE){setTxtProgressBar(pb, m)}
  } # End of loop
  if(is.null(colnames(Data))){# give column names if not herited
    colnames(ZeCubeOfMeans) <-
      paste0('V-',seq(1,ncol(Data)))->colnames(FixedMeans)
  } # Names
  rownames(ZeCubeOfMeans) <- Nom2Row
  BootstrappedGroupMeans <- apply(ZeCubeOfMeans,c(1,2),mean)
  return.list <- structure( list(BootCube=ZeCubeOfMeans,
                                 GroupMeans=FixedMeans,
                                 BootstrappedGroupMeans =
                                        BootstrappedGroupMeans),
                            class = 'bootGroup')
  return(return.list)
}  # End of function Boot4Mean
#=====================================================================
#---------------------------------------------------------------------
#' Change the print function for bootGroup
#'
#'  Change the print function for bootGroup
#'
#' @param x a list: output of bootGroup
#' @param ... everything else for the functions
#' @author Herve Abdi
#' @export
print.bootGroup <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\nBootstrapped Means K groups, J Variables, L iterations \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$BootCube               ", "An K*J*L brick of Bootstrapped means ")
  cat("\n$GroupMeans             ", "The K*J table of means")
  cat("\n$BootstrappedGroupMeans ", "The K*J table of means of BootCube")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.bootGroup
#---------------------------------------------------------------------
#---------------------------------------------------------------------
