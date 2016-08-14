## Code created by Adam Powers on 2016-08-14
##1.  set the values in the matrix
##2.  get the values in the matrix
##3.  set the inverse of the matrix
##4.  get the inverse of the matrix

##`makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Create a sample 2x2 matrix
##matrix(data, nrow, ncol, byrow)
##x <- matrix(1:4,2,2)

##Test
##mymatrix <- makeCacheMatrix(matrix(1:4,2,2))
##The following line returns the matrix input to makeCacheMatrix
##mymatrix$get
##The following line checks to make ensure that getinverse initially is NULL
##mymatrix$getinverse()

##`cacheSolve`: This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

##Note:  Computing the inverse of a square matrix can be done with the `solve`
##function in R. For example, if `X` is a square invertible matrix, then
##`solve(X)` returns its inverse.
##For this assignment, assuming that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)  ##solve is used to determine the inverse matrix
    x$setinverse(m)
    m
  }

##Run cacheSolve on mymatrix to get inverse of mymatrix
##cacheSolve(mymatrix)
##       [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##Check to see if mymatrix has been updated with inverse
##mymatrix$getinverse()
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##Multiply x by the inverse to see that result is a matrix with 1's in the diagonal
##x %*% mymatrix$getinverse()
##      [,1] [,2]
##[1,]    1    0
##[2,]    0    1
