
#' cleanDataCATA
#' Clean a Brick of Data prior to a CATA analysis
#' 
#'   cleanDataCATA cleans a CATA Brick 
#'   of data prior to a CATA analysis.
#'   The CATA Brick is
#'   obtained, for example, from \code{PTCA4CATA::read.xls.CATA}.
#'   This list should include $CATA.Brick.
#'   The level of "almost empty" is defined by the parameter
#'   \code{threshold4cleaning} 
#'   (set by default to 10% of the number of judges).
#' @param   listCATA a List as obtained, for example, from
#' \code{read.xls.CATA}.
#' @param   threshold4cleaning 
#' (default = 10% of # judges) the minimum number
#' of choices for a variable to be kept. 
#' default is 10.
#' @return A list with \code{$CATA.Brick}
#' a product by adjective by judge Brick of 0/1 data
#' (1 if judge chose adjective for product, 0 if not);
#'  \code{$CATA.CT}: 
#'   A product by adjective cleaned contingency table
#'   (columns whose sum is equal or below threshold are dropped).
#'   \code{$nProducts} the number of products,
#'   \code{$nJudges} the number of judges, and
#'   \code{$nVars} the number of variables kept.
#' @author Herve Abdi
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
  zeRawDataCube <- listCATA$CATA.Brick
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
                                class = 'readCATACube')
  return(return.list)
  
   } # When we do not clean the data
## -------------------------------------------------------------------
