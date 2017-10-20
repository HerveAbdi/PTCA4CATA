# Hervé Abdi
# Functions for renormalizing data for CATA file
# To be incorporated in the package PTCA4CATA
# Created October 16, 2016.
#
# Last Correction: October 17, 2016
#

# Functions for CATA-like type of data
# Generate From a set of CA-factor scores the re-normed versions
#         the Asymmetric (i.e. norm 1)
#         and True Barycentric (i.e., norm Lambda^2 = Delta^4
# Create the graphs

#' Renormalize CA factor scores to
#' Asymmetric factor scores or true Barycentric
#'
#'
#' Renormalize CA factor scores to
#' Asymmetric factor scores
#' (each dimension has norm = 1 instead of delta)
#' ond/or true Barycentric factor scores
#' (each dimension as norm = lambda instead of delta).
#'
#' @param X an I by L factor score matrix
#' @param delta a length L vector giving the singular value
#' if delta is longer than L, it is truncated and a warning is printed.
#' @param normalization   option are '
#' asymmetric','barycentric','both' (default). Gives the
#' type of normalization.
#' @return a list with F_A factor score for Asymmetric normalization
#' and F_B for Barycentric normalization (NB only the selected options
#' are returned).
#' @author Herve Abdi
#' @examples
#' \dontrun{
#' NormedFi = renorm4CA(Fi,delta)
#' }
#' # with Fi and delta from a CA program
#' @export

renorm4CA <- function(X,delta,
                      normalization = 'both'
                      # asymmetric','barycentric','both'
                      # type of normalization
){# Begin Function
  if ( length(delta) > ncol(X) ){
    print(c("delta too long:',
            ' truncated to number of columns of X"))
    delta = delta[(1:ncol(X))]
  }
  asymmetric   = TRUE
  barycentric = TRUE
  if (normalization == 'barycentric') asymmetric  = FALSE
  if (normalization == 'asymmetric ') barycentric = FALSE
  FactorNormed = list()
  if (asymmetric){
    FactorNormed$F_A  <- X * matrix(1/delta,nrow = nrow(X),
                                    ncol = ncol(X),
                                    byrow = TRUE)

  }
  if (barycentric){
    FactorNormed$F_B  <- X * matrix(delta,nrow = nrow(X),
                                    ncol = ncol(X),
                                    byrow = TRUE)
  }
  return(FactorNormed)
} #End Function

#*******************************************************************
# Another renormalization routine
# CA renormalization
#
#-------------------------------------------------------------------
# CARenormalization
#' re-normalize CA factor scores to Asymetric and
#' True-Barycentric factor scores
#'
#' Re-normalize a set of CA factor scores
#' to be 1) Asymmetric: points are the vertices of the simplex,
#' (Inertia per dimension = Eigenvalues = 1);
#' 2) True Barycentric: points of one set are at the
#' true barycenter of the points of the other set
#' (Inertia per dimension = Eigenvalues^2 of the
#' standard-aka symmetric analysis);
#' 3) Biplots (see Greenacre, 2007, p. 102): The asymetric set
#' is rescaled by the inverse square root of its masses;
#' 4) The strange SPSS renormalization
#' where the factor scores
#' have variance equal to the singular values.
#' These re-normalizations schemes make sense only when
#' the sets of items for rows and columns are asymmetric
#'  such as,for example, when one set represents an
#'  independent variable and the other one a dependent variable
#'  (e.g., "check all that apply" CATA data).
#'  When plotting both sets on the same map,
#'   the independent variable will be the set
#'  with the largest inertia.
#'  This type of plot is possible with two
#'  different configurations:
#'  1)  IV-set with Asymmetric and DV-set Symmetric, or
#'  2) IV-set with symmetric and DV-set with True Barycentric.
#'
#' @param G a set of factor scores from a correspondence analysis
#' @param delta a vector of singular / eigen values
#' @param singularValues do we have singular values or eigenvalues
#'  when TRUE (default) we have singular values, if FALSE we have
#'  eigenvalues
#' @param masses a vector of masses for tor the observations. if NULL
#'  do not compute the Biplot normalization
#' @author Herve Abdi
#' @return a list with G_A (Asymmetric factor scores), G_B
#'  (True Barycentric factor scores), G_S (SPSS Symmetric Biplot),
#'  and G_P (biPlot).
#' @export
CARenormalization <- function(#
  G, # the factor scores
  delta, # The singular/eigen values
  singularValues = TRUE,
  # if TRUE  => singular values
  # if FALSE => eigen values
  masses = NULL # Masses for Greenacre's Biplots
  # if NULL do not compute
  # left at NULL for compatibility
  #  with previous versions
){
  if (!singularValues) delta <- sqrt(delta)
  if (!is.vector(delta)){
    stop(cat('Error in function CARenormalization: ',
             'delta must be a vector') )
  }
  if(length(delta) != NCOL(G)){
    stop(cat('Error in function CARenormalization: ',
             'delta should have length = ncol(G) ') )
  }
  G <- as.matrix(G)
  # True Barycentric
  G_B  <- G * matrix(delta, nrow = nrow(G),
                     ncol = ncol(G),
                     byrow = TRUE)
  # Asymmetric
  G_A  <- G * matrix(1 / delta, nrow = nrow(G),
                     ncol = ncol(G),
                     byrow = TRUE)

  # SPSS Symmetric Biplot
  G_S  <- G * matrix(1 / sqrt(delta), nrow = nrow(G),
                     ncol = ncol(G),
                     byrow = TRUE)
  return.list <- list(G_A = as.data.frame(G_A),
                      G_B = as.data.frame(G_B),
                      G_S = as.data.frame(G_S) )

  # Greenacre's Biplot
  if (!is.null(masses)){
    G_P <- matrix(sqrt(masses),nrow = nrow(G),
                  ncol = ncol(G),
                  byrow = FALSE) * G_A
    return.list$G_P = as.data.frame(G_P)
  }

  return(return.list)
}
# End of function CArenormalization
#--------------------------------------------------------------------

#--------------------------------------------------------------------
#
#' Create all the normalization for the factor scores of a CA:
#' Symmetric (the standard), Asymetric, and
#' True-Barycentric factor scores.
#'
#' Create all the different normalization for the rows
#' and columns of a correspondence analysis:
#' Re-normalize a set of CA factor scores
#' to be 1) Asymetric: points are 1) the vertices of the simplex,
#' (Inertia per dimension = Eigenvalues = 1) and
#' 2) True Barycentric: points of one set are at the
#' true barycenter of the points of the other set
#' (Inertia per dimension = Eigenvalues^2 of the
#' standard-aka symmetric analysis).
#' These re-normalizations schemes make sense only when
#' the sets of items for rows and columns are asymmetric
#'  such as for example when one set represents an
#'  independent variable and the other one a dependent variable
#'  (e.g., "check all that apply" data).
#'  When plotting both sets on the same map,
#'   the independent variable will be the set
#'  with the largest inertia. This is possible with two
#'  different configurations:
#'  1)  IV-set with Asymmetric and DV-set Symmetric, or
#'  2) IV-set with symmetric and DV-set with True Barycentric.
#'
#' @param ResFrom.epCA The results from
#'  \code{ExPosition::epCA}
#' @param namesOfFactors the names of the factors
#'  (default is 'Dimension')
#' @author Herve Abdi
#' @return a list with the normalized factor scores
#'  \code{Fi}, \code{Fi_A}, \code{Fi_B},\code{Fj},\code{Fj_A}, \code{Fj_B}.
#' @export

createAllNormedFactors <- function(ResFrom.epCA,
                                   namesOfFactors = 'Dimension' #
){# function begins here
  Fi      <- ResFrom.epCA$ExPosition.Data$fi
  masses4i <- ResFrom.epCA$ExPosition.Data$M
  Fj      <- ResFrom.epCA$ExPosition.Data$fj
  masses4j <- ResFrom.epCA$ExPosition.Data$c
  delta <- sqrt(ResFrom.epCA$ExPosition.Data$eigs)
  if ( length(delta) > ncol(Fi) ){
    delta = delta[(1:ncol(Fi))]
  }
  names4Dimensions <- paste(namesOfFactors,1:ncol(Fi))
  colnames(Fi) <- names4Dimensions -> colnames(Fj)
  I.set <- CARenormalization(Fi, delta = delta, masses = masses4i)
  J.set <- CARenormalization(Fj, delta = delta, masses = masses4j)
  #return
  return.list = structure(list(Fi = Fi,
                               Fi_A = I.set$G_A,
                               Fi_B = I.set$G_B,
                               Fi_P = I.set$G_P,
                               Fi_S = I.set$G_S,
                               Fj = Fj,
                               Fj_A = J.set$G_A,
                               Fj_B = J.set$G_B,
                               Fj_P = J.set$G_P,
                               Fj_S = J.set$G_S),
                          class = 'createNormedFactors')
  # Note the class "createNormedFactors"
  # it is "linked" to the
  # function print.createNormedFactors
  # that modifies the print function so that it will
  # print the description of the results
  # when the print function is used
  return(return.list)

} # End of function createAllNormedFactors

#--------------------------------------------------------------------
# Change the print function for
# the createNormedFactors environment
#
#' Change the print function for createNormedFactors
#'
#'  Change the print function for createNormedFactors
#'
#' @param x a list: output of createNormedFactors
#' @param ... everything else for the functions
#' @author Herve Abdi
#' @export
print.createNormedFactors <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\nDifferent Normalization Schemes for CA \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$Fi          ", "I-Set (row) Symmetric        factor scores (Fi'M*Fi = L)")
  cat("\n$Fi_A        ", "I-Set (row) Asymmetric       factor scores (Fi_A'M*Fi_A = I)")
  cat("\n$Fi_B        ", "I-Set (row) true Barycentric factor scores (Fi_B'M*Fi_B = L^2)")
  cat("\n$Fi_P        ", "I-Set (row) Biplot           Fi_P = M^(1/2) * Fi_A")
  cat("\n$Fi_S        ", "I-Set (row) SPSS Biplot      factor scores (Fi_S'M*Fi_S = L^(1/2)")
  cat("\n$Fj          ", "J-Set (col) Symmetric        factor scores (Fj'W*Fj = L)")
  cat("\n$Fj_A        ", "J-Set (col) Asymmetric       factor scores (Fj_A'W*Fj_A = I)")
  cat("\n$Fj_B        ", "J-Set (col) true Barycentric factor scores (Fj_B'W*Fj_B = L^2)")
  cat("\n$Fj_P        ", "J-Set (row) Biplot           Fj_P = W^(1/2) * Fj_A")
  cat("\n$Fi_S        ", "J-Set (row) SPSS Biplot      factor scores (Fj_S'W*F_S = L^(1/2)")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.createNormedFactors
#--------------------------------------------------------------------





