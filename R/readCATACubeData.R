#
# A function to read the data from a CATA xls File
# and to create a DataCube
# To be integrated in PTCA4CATA Package
# Herve Abdi October 20, 2016.
# Current version: July 11, 2017.
# Uses the readxl packages and so does not need rJava
#----------------------------------------------------------------------
# --------------------------------------------------------------------
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890
#' Reads the CATA data from an Excel File and creates
#' a "Brick" of data
#' to be analyzed by PTCA4CATA
#'
#' \code{read.xls.CATA}
#' reads the CATA data from an Excel File and creates a Brick of Data
#' and a contingency table. The contingency table can be analyzed
#' by correspondence analysis or Hellinger analysis.
#' The cube of data is needed to perform correct
#' cross-validation methods such as
#' permutations
#' tests,  bootstrap confidence intervals, and
#' bootstrap ratios.
#' With \eqn{I} products,  \eqn{J} adjectives and  \eqn{K} judges
#' The data in the excel file are organized as:
#'         row 1 column 1 = name of Judge 1
#'           row 1 columns  (2 to  \eqn{J} + 1) name of the adjectives
#'         row 2 column 1 = name of product 1.
#'           row 2 column (2 to  \eqn{J} + 1) 0/1 answers
#'             of Judge 1 to the  \eqn{J} adjectives for product 1
#'         ....
#'         row  \eqn{I}+1 column 1 = name of product  \eqn{I}
#'            row 2 column (2 to  \eqn{J} + 1) 0/1 answers
#'         row  \eqn{I} + 2 column 1 = name of product 1.
#'           row 2 column (2 to  \eqn{J} + 1) 0/1 answers
#'             of Judge 2 to the  \eqn{J} adjectives for product 1
#'         ....
#'         And so on till the last judge.
#'         See the help for the excel
#'         file \code{OrangeJuiceCATARawData.xlsx}
#'         for an example of how the excel file should
#'         be organized.
#'  @section Implementation
#' Current version uses Wickham's \code{readxl} package
#' and so does not need \code{rJava}.
#' Current version: is July 11, 2017.
#'
#' @param path2file the name of the \code{xls} file with the data.
#' @param sheet2read the name of the sheet in the excel file.
#' @param orderProducts if \code{TRUE} (default) alphabetically
#' order the products.
#' @param threshold4cleaning the cleaning threshold:
#' The columns whose total
#' is smaller than \code{threshold4cleaning} are eliminated from
#' \code{CleanedContingencyTable}.
#' Default: \code{threshold4cleaning = 0} (i.e.,
#' columns with a zero sum are eliminated).
#' To keep even the columns with a zero sum, use
#' \code{threshold4cleaning = -1}
#' @return A list with \code{CATA.Brick}
#' a "product by adjective by judge" Brick of 0/1 data
#' (1 if Judge chose adjective for product, 0 if not);
#'  \code{ContingencyTable}:
#' A "product by adjective" contingency table;
#'  \code{CleanedContingencyTable}:
#'   A "product by adjective" cleaned contingency table
#'   (columns whose sum is equal or below threshold are dropped).
#' @importFrom  readxl read_excel
#' @author Herve Abdi
#' @export
#'

# Here we read the data from the xls file
# Read the individual data from the sheet
#file2read  = 'BetaCata_Consumers.xlsx'
#sheet2read = 'CATA'
#
read.xls.CATA <- function(path2file, # The file name of the data
                          sheet2read, # The name of the sheet of the data
                          orderProducts = TRUE,
                          threshold4cleaning = 0 # Drop low frequency
                          # columns
                          ){
  # Starts here
  # read the data
  #wb = XLConnect::loadWorkbook(path2file)
  #df = XLConnect::readWorksheet(wb, sheet = sheet2read, header = TRUE)
  # Replace old XLConnect with non rJava routine
  # with read_ readxl
  df <- as.data.frame(readxl::read_excel(path = path2file,
                                         sheet = sheet2read))
  #-----------------------------------------------------------------
#
# The data are organized as: Judge 1. row 1 = name of adjective
#                              (2 to 58)
#                           Column 1 Name of beers (rows 1 to 9)
#                           Judge 2. row 10 = name of adjective
#                               (2 to 58)
#                           Column 1 Name of beers (rows 11 to 19)
#                           ...
#                           Judge 75. row 740 = name of adjective
#                                                          (2 to 58)
#                           Column 1 Name of beers (rows 740 to 749)
#-----------------------------------------------------------------
return.list <-createCube4CATA(df = df, # A data frame
                              orderProducts = orderProducts,
                              threshold4cleaning = threshold4cleaning
                              # Drop low frequency
                              # columns
)

    return(return.list)
} # end of function
#-----------------------------------------------------------------

#
# # Reformat the data for PTCA
# #   (partial triadic correspondence analysis)
# # Here we clean the Data
# #
# # Get the number of Products
# pos.1   <- which(df[,1]==df[1,1])
# #
# nProducts <- pos.1[2] - pos.1[1] -1
# # Number of Judges
# nJudges <- length(pos.1)
# # Number of Attributes
# nVars = ncol(df) - 1
# # the first column gives the name of the products
# #-----------------------------------------------------------------
# # Make a Cube of Data from the xlsx file
# Rows2Drop =  seq(from = nProducts+1,
#                  to = nrow(df), by = nProducts+1)
# Toto = df[-Rows2Drop,-1]
# # Now make Toto a Matrix
# Fourbi = matrix(as.numeric(as.matrix(Toto)),ncol = nVars)
# colnames(Fourbi) <- colnames(Toto)
# rownames(Fourbi) <- paste0(rep(seq(1,nJudges),
#                            each=nProducts),'.',df[,1][-Rows2Drop])
# # Now reshape Fourbi with a horrible loop!
# ZeRawDataCube = array(NA,dim=c(nProducts,nVars,nJudges) )
# #                      dimnames = c('Beers','Attributes','Judges'))
# for (i in 1:nJudges){# Begin Loop on i
#   ZeRawDataCube[,,i] <- Fourbi[((i-1)*nProducts +1):(i*nProducts),]
# }# End Loop on i
# #
# # Get the names of Products, Attributes and Judges
# #
# NamesOfProducts =  df[1:nProducts,1]
# NamesOfAttributes = colnames(df)[1:nVars+1]
# NamesOfJudges = c(colnames(df)[1], df[seq(from = nProducts+1,
#              to = (nProducts+1)*(nJudges-1), by = nProducts+1),1])
# # Fix a potential problme with repeated names of Judges
# if ( length(unique( NamesOfJudges )) != nJudges){
#   NamesOfJudges = paste0('J-',seq(1,nJudges))
#    }
# #
# #------------------------------------------------------------------
# # Give names to the dimensions
# # Note the [[]]. Will not work otherwise
# # change NamesOfProducts as NamesOfProducts
# #        nProducts as nProducts
# dimnames(ZeRawDataCube)[[1]] <- NamesOfProducts
# dimnames(ZeRawDataCube)[[2]] <- NamesOfAttributes
# dimnames(ZeRawDataCube)[[3]] <- NamesOfJudges
# #-----------------------------------------------------------------
#
# # Now Create the Contingency table
# # First step: Create the contingency table
# ContingencyTable = apply(ZeRawDataCube,c(1,2),sum)
#
# #
# return.list <- structure(list(CATA.Brick = ZeRawDataCube,
#                               CATA.CT = ContingencyTable),
#                          class = 'readCATACube')
# # Then check that the column weights
# # do not create a problem for CA
# ColTotals <- colSums(ContingencyTable)
# # threshold4cleaning = 80
# #-----------------------------------------------------------------
# # Do we clean the CATA.CT?
# BadCol <- (ColTotals <= threshold4cleaning)
# if (any(BadCol) ){
#   CleanedContingencyTable    <- ContingencyTable[,!BadCol]
#   return.list$CATA.CleanedCT <-  CleanedContingencyTable
#       }
# #-----------------------------------------------------------------
# return.list$nProducts <- nProducts
# return.list$nVars <- nVars
# return.list$nJudges <- nJudges
#
# return(return.list)
# } # End of function
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for readCATACube
#'
#' Change the print function for readCATACube
#'
#' @param x a list: output of readCATACube
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.readCATACube <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n Data-Brick and Contingency Table(s) from a CATA task \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$CATA.Brick     ", "An I-Products*J-Adjective*K-Judges 0/1 Data Brick")
  cat("\n$CATA.CT        ", "An I-Products*J-Adjective Contingency Table")
  cat("\n$CATA.CleanedCT ", "An I-Products*J-Adjective Cleaned Contingency Table")
  cat("\n$nProducts      ", "Number of  Products")
  cat("\n$nVars          ", "Number of Variables (Adjectives)")
  cat("\n$nJudges        ", "Number of  nJuges")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.readCATACube
#_____________________________________________________________________
#  Test for read.xls.CATA
# # library(XLConnect)
# # Where are the Data?
# dir2read <- '~/Box Sync/R4Bangkok/Dev-PTCA4CATA/'
# # Read the individual data from the sheet
# file2read <- 'greenteaCATA data for ca.xlsx'
# path2file  <- paste0(dir2read,file2read)
# sheet2read <- 'all data'
#
# DataFromCATA <- read.xls.CATA(path2file ,sheet2read)
#_____________________________________________________________________
#
# A function
# to create a DataCube from a df
# To be integrated in PTCA4CATA Package
# Herve Abdi October 21, 2016
#
#_____________________________________________________________________
#_____________________________________________________________________
#        1         2         3         4         5         6         7
#234567890123456789012345678901234567890123456789012345678901234567890
#' create a "Brick" of data and contingency table from CATA df.
#'
#'
#' take a df containing CATA data and create the CATA 0/1/ Data Brick
#' and a contingency table. The contingency table can be analyzed
#' by correspondence analysis or Hellinger analysis.
#' The cube of data is needed to perform correct pemutations
#' test and to compute bootstrap confidence intervals and
#' bootstrap ratios.
#' With I products, J adjectives and K judges
#' The data in the excel file are organized as:
#'         row 1 column 1 = name of Judge 1
#'           row 1 columns  (2 to J+1) name of the adjectives
#'         row 2 column 1 = name of product 1.
#'           row 2 column (2 to J+1) 0/1 answers
#'             of Judge 1 to the J adjectives for product 1
#'         ....
#'         row I+1 column 1 = name of product I
#'            row 2 column (2 to J+1) 0/1 answers
#'         row I+2 column 1 = name of product 1.
#'           row 2 column (2 to J+1) 0/1 answers
#'             of Judge 2 to the J adjectives for product 1
#'         ....
#'         And so on till the last judge.
#'         See df ***** as data for an example
#' Current version uses XLConnect, and so needs JAVA and rJava
#' A newer version is likely to use Wickham's readxl package
#'
#' @param df a data frame with the CATA data
#' (coded as indicated above)
#' @param orderProducts if TRUE (default) alphabetically
#' order the products.
#' @param threshold4cleaning a threshold: the columns whose total
#' is smaller than \code{threshold4cleaning} are eliminated from
#' the data
#' default: threshold4cleaning = 0.
#' @return A list with \code{CATA.Brick}
#' a product by adjective by judge Brick of 0/1 data
#' (1 if judge chose adjective for product, 0 if not);
#'  \code{ContingencyTable}:
#' A product by adjective contingency table;
#'  \code{CleanedContingencyTable}:
#'   A product by adjective cleaned contingency table
#'   (columns whose sum is equal or below threshold are dropped).
#' @author Herve Abdi
#' @export
#'

# Here we read the data from the xls file
# Read the individual data from the sheet
#file2read  = 'BetaCata_Consumers.xlsx'
#sheet2read = 'CATA'
#
createCube4CATA <- function(df, # A data frame
                          orderProducts = TRUE,
                          threshold4cleaning = 0 # Drop low frequency
                          # columns
){
  # Starts here
  # read the data
  #wb = XLConnect::loadWorkbook(path2file)
  # df = XLConnect::readWorksheet(wb,sheet = sheet2read, header = TRUE)
  #-----------------------------------------------------------------
  #
  # The data are organized as: Judge 1. row 1 = name of adjective
  #                              (2 to 58)
  #                           Column 1 Name of beers (rows 1 to 9)
  #                           Judge 2. row 10 = name of adjective
  #                               (2 to 58)
  #                           Column 1 Name of beers (rows 11 to 19)
  #                           ...
  #                           Judge 75. row 740 = name of adjective
  #                                                          (2 to 58)
  #                           Column 1 Name of beers (rows 740 to 749)
  #___________________________________________________________________
  #___________________________________________________________________
  # Reformat the data for PTCA
  #   (partial triadic correspondence analysis)
  # Here we clean the Data
  #
  # Get the number of Products
  pos.1   <- which(df[,1]==df[1,1])
  #
  nProducts <- pos.1[2] - pos.1[1] -1
  # Number of Judges
  nJudges <- length(pos.1)
  # Number of Attributes
  nVars = ncol(df) - 1
  # the first column gives the name of the products
  #-----------------------------------------------------------------
  # Make a Cube of Data from the xlsx file
  Rows2Drop =  seq(from = nProducts+1,
                   to = nrow(df), by = nProducts+1)
  Toto = df[-Rows2Drop,-1]
  # Now make Toto a Matrix
  Fourbi = matrix(as.numeric(as.matrix(Toto)),ncol = nVars)
  colnames(Fourbi) <- colnames(Toto)
  rownames(Fourbi) <- paste0(rep(seq(1,nJudges),
                                 each=nProducts),'.',df[,1][-Rows2Drop])
  # Now reshape Fourbi with a horrible loop!
  ZeRawDataCube = array(NA,dim=c(nProducts,nVars,nJudges) )
  #                      dimnames = c('Beers','Attributes','Judges'))
  for (i in 1:nJudges){# Begin Loop on i
    ZeRawDataCube[,,i] <- Fourbi[((i-1)*nProducts +1):(i*nProducts),]
  }# End Loop on i
  #
  # Get the names of Products, Attributes and Judges
  #
  NamesOfProducts =  df[1:nProducts,1]
  NamesOfAttributes = colnames(df)[1:nVars+1]
  NamesOfJudges = c(colnames(df)[1], df[seq(from = nProducts+1,
                                      to = (nProducts+1)*(nJudges-1),
                                      by = nProducts+1),1])
  # Fix a potential problem with repeated names of Judges
  if ( length(unique( NamesOfJudges )) != nJudges){
    NamesOfJudges = paste0('J-',seq(1,nJudges))
  }
  #
  #------------------------------------------------------------------
  # Give names to the dimensions
  # Note the [[]]. Will not work otherwise
  # change NamesOfProducts as NamesOfProducts
  #        nProducts as nProducts
  dimnames(ZeRawDataCube)[[1]] <- NamesOfProducts
  dimnames(ZeRawDataCube)[[2]] <- NamesOfAttributes
  dimnames(ZeRawDataCube)[[3]] <- NamesOfJudges
  #-----------------------------------------------------------------
  if (orderProducts){
    ZeRawDataCube <- ZeRawDataCube[
      order(dimnames(ZeRawDataCube)[[1]] ),,]
  }

  # Now Create the Contingency table
  # First step: Create the contingency table
  ContingencyTable = apply(ZeRawDataCube,c(1,2),sum)

  #
  return.list <- structure(list(CATA.Brick = ZeRawDataCube,
                                CATA.CT = ContingencyTable),
                                class = 'readCATACube')
  # Then check that the column weights
  # do not create a problem for CA

  # threshold4cleaning = 80
  return.list$nProducts <- nProducts
  return.list$nVars     <- nVars
  return.list$nJudges   <- nJudges

  # Old Cleaning Code
  # #-----------------------------------------------------------------
  # # Do we clean the CATA.CT?
  # ColTotals <- colSums(ContingencyTable)
  # BadCol <- (ColTotals <= threshold4cleaning)
  # if (any(BadCol) ){
  #   CleanedContingencyTable    <- ContingencyTable[,!BadCol]
  #   return.list$CATA.CleanedCT <-  CleanedContingencyTable
  #-------------------------------------------------------------------
  # New Code calls the function cleanDataCATA()

  return.list <- cleanDataCATA(return.list,
                       threshold4cleaning = threshold4cleaning)

  return(return.list)
} # End of function
#*****************************************************************
#-----------------------------------------------------------------
