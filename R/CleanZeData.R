
# cleanDataCATA
#' Clean a Brick of Data prior to a CATA analysis
#'
#'   \code{cleanDataCATA} cleans a \code{CATA} Brick
#'   of data prior to a CATA analysis.
#'   The CATA Brick can be a list or an array. The list can be
#'   obtained, for example, from \code{PTCA4CATA::read.xls.CATA}.
#'   If so,
#'   this list should include \code{$CATA.Brick}.
#'   If the data is an array, it should be
#'   a Products * Variables * Judges array.
#'   The level of "almost empty" is defined by the parameter
#'   \code{threshold4cleaning}
#'   (set by default to 10% of the number of judges).
#' @param   listCATA could be 1) a List as obtained,
#' for example, from
#' \code{read.xls.CATA},
#' or 2) a Products * Variables * Judges array
#' (with only 0/1 values).
#' @param   threshold4cleaning
#' (default = 10% of # Judges) the minimum number
#' of choices for a variable to be kept.
#'
#' @return A list with \code{$CATA.Brick}
#' a "Product by Variables by  Judges" Brick of 0/1 data
#' (\emph{x(i,j,k)} = 1
#' if Judge \emph{k} choses Variable \emph{j}
#' to describe Product \emph{i}, 0 if not);
#'  \code{$CATA.CT}:
#'   A "Products by Variables" cleaned contingency table
#'   (columns whose sum is equal to or below
#'   the threshold are dropped) that gives the number of
#'   Judges who selected this Variable for that Product.
#'   \code{$nProducts} the number of Products,
#'   \code{$nJudges} the number of Judges, and
#'   \code{$nVars} the number of Variables kept.
#' @author Herve Abdi
#' @examples\dontrun{
#' data('beersBeTACATA')
#' rawCubeOfData     <- beersBeTACATA$rawBrick.CATA
#' cleanedListOfData <- cleanDataCATA(rawCubeOfData, threshold4cleaning =  5)
#' }
#' @export
#'
cleanDataCATA <- function(listCATA,threshold4cleaning = NULL){
  ##
  ## -----------------------------------------------------------------
  # We need a first step of winsorization
  #
  ## -----------------------------------------------------------------
  #
  # threshold = .90 # Columns with less than threshold will be off

  #nProducts <-  dim(zeRawDataCube)[1]
  #nVars     <-  dim(zeRawDataCube)[2]
  if (is.list(listCATA)){
  zeRawDataCube <- listCATA$CATA.Brick
  } else {
    zeRawDataCube <- listCATA
  }
  if (is.null(threshold4cleaning)){
      threshold4cleaning = ceiling(dim(zeRawDataCube)[3]*.10)
  }
  #nJudges   <-  dim(zeRawDataCube)[3]
  # Compute the Contingency Table
  old.CT <- apply(zeRawDataCube,c(1,2),sum)
  ColTotals <- colSums(old.CT)
  Col2Drop = which(ColTotals <= threshold4cleaning)
  if(length(Col2Drop) == 0){# When there is no column to drop
    # do nothing and return the original list
    return(listCATA)
  }
  zeCleanedDataCube = zeRawDataCube[,-Col2Drop,]
  dimnames(zeCleanedDataCube)[[2]] <- colnames(old.CT)[-Col2Drop]
  CT = apply(zeCleanedDataCube,c(1,2),sum)
  nProducts <-  dim(zeCleanedDataCube)[1]
  nVars     <-  dim(zeCleanedDataCube)[2]
  nJudges   <-  dim(zeCleanedDataCube)[3]
  #
  return.list <- structure(list(CATA.Brick = zeCleanedDataCube,
                                CATA.CT = CT,
                                nProducts = nProducts,
                                nVars     = nVars,
                                nJudges   = nJudges),
                                class = 'readCATACube.c')
  return(return.list)

   } # When we do not clean the data
## ___________________________________________________________________
#
#_____________________________________________________________________
#' Change the print function for class cleanCATACube.c
#'
#' Change the print function for class cleanCATACube.c
#' (output of \code{cleanDataCATA()}
#' @param x a list: output of cleanCATACube
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.readCATACube.c <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Data-Brick and Contingency Table(s) from a CATA task \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$CATA.Brick ", "An I-Products*J-Adjectives*K-Judges 0/1 Data Brick.")
  cat("\n$CATA.CT    ", "An I-Products*J-Adjective Cleaned Contingency Table.")
  cat("\n$nProducts  ", "Number of  Products.")
  cat("\n$nVars      ", "Number of Variables (Adjectives).")
  cat("\n$nJudges    ", "Number of  nJuges.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.readCATACube.c
#-------------------------------------------------------------------




