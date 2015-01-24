## makeCacheMatrix takes the input of a matrix,
## and wraps the matrix in a list with 4 functions for interaction.
## cacheSolve will solve the inverse of the wrapped matrix
## produced by makeCacheMatrix

## Returns a list with 4 functions to interact with 'x'. These 4 functions
## reside in their own environment, not on the global environment
## function - set - sets the value of matrix 'x'
## function - get - returns the value of matrix 'x'
## function - setinverse - sets the inverse of matrix 'x'
## function - getinverse - returns null if the inverse of matrix 'x'
## does not exist, or the inverse of matrix 'x'
makeCacheMatrix <- function(x = matrix()) {
	# variable ci (cached inverse) will store the cache of the inverse matrix
	ci <- NULL
	set <- function (y) {
		x <<- y
		ci <<- NULL
	}
	get <- function () x
	setinverse <- function (inverse) ci <<- inverse
	getinverse <- function () ci
	list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
## This function assumes that the matrix in 'x' is always invertible.
## If 'x' has cached data, it will return the cached matrix
## Otherwise, it will solve the inverse of 'x'
cacheSolve <- function(x, ...) {
	# variable ci (cached inverse) will attempt to retrieve
	# the cache of the inverse matrix
	ci <- x$getinverse()
	# this check runs when ci contains data
	if (!is.null(ci)) {
		message("getting cached data")
		return(ci)
	}
	data <- x$get()
	ci <- solve(data, ...)
	x$setinverse(ci)
	ci
}