##*************************************************************
## COURSERA
## R PROGRAMMING 
## Programming Assignment 2: Caching the Inverse of a Matrix
##
## AUTHOR: Katarzyna Pienczk
##
##*************************************************************

##-------------------------------------------------------------
## FUNCTION: makeCacheMatrix
## DESC: 
##    Creates a CacheMatrix object with the following interface:  
##        - set(newMatrix) - sets a matrix
##        - get() - returns the matrix
##        - setInverse(newInverse) - caches an inverse matrix (some inverse matrix conditions must be satisfied)
##        - getInverse() - returns the previously cached inverse matrix
##
##-------------------------------------------------------------

makeCacheMatrix <- function(m = matrix()){
  mInverse <- NULL
  
  #--- set() ---------------------------------
  set <- function(newMatrix){
    if (class(newMatrix)!="matrix")
      stop("Incorrect class: not a matrix")
    
    m <<- newMatrix
    mInverse <<- NULL
  }
  
  #--- get() ---------------------------------
  get <- function() m
  
  #--- setInverse() --------------------------
  setInverse <- function(newInverse){ 
    
    ## the original m matrix must be a square matrix
    ## and cannot be a singular matrix
    if (nrow(m)!=ncol(m)) 
      stop("Incorrect original matrix: not a square matrix. Inverse matrix cannot be set")
    if (det(m) == 0)
      stop("Incorrect original matrix: is a singular matrix. Inverse matrix cannot be set")
    
    ## validate passed arguments
    ## the inverse matrix 
    ##   - should be a square matrix
    ##   - should have the same dimensions as the original m matrix  
    ##   - multiplied by the original matrix should return an identity matrix
    
    if (class(newInverse)!="matrix")
      stop("Incorrect class: not a matrix")
    if (nrow(newInverse)!=ncol(newInverse)) 
      stop("Incorrect dimensions: not a square matrix")
    
    testDims <- dim(m)==dim(newInverse)    
    if (FALSE %in% testDims)
      stop("Incorrect dimensions: should equal to the dimensions of the original matrix")
    
    test <- round(m %*% newInverse)
    if (FALSE %in% (test==diag(nrow(m))))
      stop("Incorrect values: multiplication by the original matrix should return an identity matrix")
    
    ## set an inverse matrix
    mInverse <<- newInverse
  }
  
  #--- getInverse() --------------------------
  getInverse <- function() mInverse
  
  #--- return a list -------------------------
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

##-------------------------------------------------------------
## FUNCTION: cacheSolve
## DESC:
##    This function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix. 
##    If the inverse has already been calculated, 
##    the cachesolve retrieves the inverse from the cache.
##
##-------------------------------------------------------------

cacheSolve <- function(mObject, ...){
  
  ## check if there's a cached inverse matrix available
  inverseMatrix <- mObject$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached inverse matrix")
    return(inverseMatrix)
  }
  
  ## if there's no inverse matrix available in the cache,
  ## compute one and store it in the cache
  message("computing inverse matrix")
  matrix <- mObject$get()
  inverseMatrix <- solve(matrix, ...)
  
  message("caching inverse matrix")
  mObject$setInverse(inverseMatrix)
  
  inverseMatrix
}

##*************************************************************
