#====================================================================
# Some functions here that will be moved to
# PTCA4CATA
#====================================================================
#' p2Star Converts a vector of probabilities into a vector
#' of stars for display.
#'
#' p2Star Converts a vector of probabilities into a string vector
#' of stars for display.
#' @param pValues a vector of $p$ values (i.e., values between 0 and 1)
#' @param threshold a vector giving the values for thresholding
#' default is \code{c(.05,.01,.001)}.
#' @return pStared a vector of stars
#' @author Herve Abdi
#' @examples
#' \dontrun{
#' stars4p <- p2Star(aVectorOfPValues)
#' }
#'
#' @export
p2Star <- function(pValues,threshold = c(.05,.01,.001)){
  pStared   <- rep("",length(pValues))
  threshold <- sort(threshold,decreasing = TRUE)
  for (i in 1:length(threshold)){
    lindex <- which(pValues <= threshold[i])
    pStared[lindex] <- paste0(rep("*",i),collapse = "")
  }
  return(pStared)
}
# End of function p2Star
#====================================================================
# function CTstared
#====================================================================
#' CTstared adds a vector of characters (i.e., stars) to the variables
#' names
#' of a contingency table.
#'
#' CTstared adds a vector of characters (i.e., stars) to the names
#' ot the variables (i.e., the columns)
#' of a contingency table. Used, for example, to make a heat map
#' of contingency table with the names of the variables
#' begin stared if they are siginficant with a Cochran's test.
#' @param aCT a Contingency Table
#' @param vecOfStars a vector (same length as the number of columns of
#' \code{aCT}) with the stars
#' @param pos position of the stars \code{"before"} (default)
#'  or \code{"after"} the names of the variables.
#'  @return CT.withStars  \code{aCT} with added stars
#' @author Herve Abdi
#' @examples
#' \dontrun{
#'  staredCT <- CTstared(aCT,stars4ThisCT)
#' }
#'
#' @export
CTstared <- function(aCT, vecOfStars,pos = 'before'){
  if (pos == 'after'){
    newColNames <- paste0(colnames(aCT),' ',vecOfStars)
  } else{
    newColNames <- paste0(vecOfStars,' ',colnames(aCT))
  }
  CT.withStars <- aCT
  colnames(CT.withStars) <- newColNames
  return(CT.withStars)
}
# End of function CTstared
#====================================================================
# functions end here
#====================================================================
