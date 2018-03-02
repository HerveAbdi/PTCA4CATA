# This file
# Describe the orange dataset
# Create on February 24, 2018 by Herve Abdi
# part of the PTCA4CATA package.
#

#' \code{OrangeJuiceCATARawData}: an example of an excel file
#' with CATA data. This excel file can be read by
#' \code{read.xls.CATA}.
#'
#'  \code{OrangeJuiceCATARawData} example of an excel file
#' with CATA data. Can be read by
#' \code{read.xls.CATA}.
#' In this example of a "Check All That Apply" (CATA) task,
#' 42 assessors evaluated 10 orange juices described by
#' 33 descriptors.
#' To fetch this dataset use \code{system.file()}
#' (see example below).
#' @name OrangeJuiceCATARawData
#' @section OrangeJuiceCATARawData.xlsx
#' @author Herve Abdi
#' @examples
#' path2file <- system.file("extdata", "OrangeJuiceCATARawData.xlsx", package = 'PTCA4CATA')
#' OrangeData <- read.xls.CATA(path2file = path2file, sheet2read = 'CATA')
NULL

