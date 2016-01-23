## R programming assignment 2 : Caching the Inverse of a Matrix
## take advantange of Lexical scoping rules

## Matrix inversion is usally a costly computation and there my be some
## benefit to caching the inverse of a matrix rather than compute it repeatly.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## x is formal variable for makeCacheMatrix function
  ## inver is a local variable for makeCacheMatrix function
    inver <- NULL
    set <- function(y) {
      ## y is formal variable for set function
      ## <<- is using y value to reset parent environment values x and inver
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inver <<- solve
    getinverse <- function() inver
    list( set = set , get = get ,setinverse = setinverse, getinverse = getinverse)
    
}


## cacheSolve function computes the inverse of the special "matrix" returned by
## makeCacheMatrxi above. If the inverse has already been calculated ,
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver2 <- x$getinverse()
  if (!is.null(inver2)) {
        message("getting cached inverse matrix")
        return(inver2)
  }
  origM <- x$get()
  inver2 <- solve(origM,...)
  
  ## here is the part to cache the Matrix. That's why it will not get from cache when you run first time.
  
  x$setinverse(inver2)
  
  inver2
  
}

## result for reference:
##
##  > testm <- matrix(1:4,2)
##  > testm
##       [,1] [,2]
##  [1,]    5    7
##  [2,]    6    8
##  > solve(testm)
##       [,1] [,2]
##  [1,]   -4  3.5
##  [2,]    3 -2.5

##------Testing two functions--------------------

## > testMatrix <- makeCacheMatrix(matrix(5:8,2))
## > cacheSolve(testMatrix)
##        [,1] [,2]
##  [1,]   -4  3.5
##  [2,]    3 -2.5
##  > cacheSolve(testMatrix)
##  getting cached inverse matrix
##        [,1] [,2]
##  [1,]   -4  3.5
##  [2,]    3 -2.5
##  > 

