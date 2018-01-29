# An add-on to get a monmaxHelper for a bootstrap cube
# to be included in PTCA4CATA in due time
# H.A.. Fist version October-31-2017.
#
#---------------------------------------------------------------------


# Current function needs to be expanded to include
# constraints from another matrix
# a possible solution is to create another function
# that mixes two sets of constraints
#---------------------------------------------------------------------
# Two functions to get the constraints correct with
# a bootstrap cube
#
# This funciton should be a private function for minmaxHelper4Cube
minAndMax <- function(x){
  leMin = min(x)
  leMax = max(x)
  return(list(leMin = leMin, leMax = leMax))
}
# A helper function to get min and max from a  (bootstrap) cube
minmaxHelper4Cube <- function(aCube, axis1 = 1, axis2 = 2){
  dimCube = length(dim(aCube))
  if (length(dimCube) == 2){
    constraints = minmaxHelper(aCube, axis1,axis2)
    return(constraints)
  }
  minmaxx <- minAndMax(aCube[,axis1,])
  minmaxy <- minAndMax(aCube[,axis2,])
  constraints <- list(minx = minmaxx$leMin,  miny = minmaxy$leMin,
                      maxx = minmaxx$leMax, maxy =  minmaxy$leMax)
  constraints <- lapply(constraints,'*',1.1) # expand for comfort
  return(constraints)
}
# A test
#minmaxHelper4Cube(boot.scores.agg,3,4)
#---------------------------------------------------------------------
#---------------------------------------------------------------------

