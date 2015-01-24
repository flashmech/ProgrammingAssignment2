## makeCacheMatrix takes the input of a matrix,
## and wraps the matrix in a list with 4 functions for interaction.
## cacheSolve will attempt to solve the inverse of the wrapped matrix
## produced by makeCacheMatrix

## makeCacheMatrix takes the input of a matrix,
## and wraps the matrix in a list with 4 functions for interaction.
makeCacheMatrix <- function(x = matrix()) {
	# variable cc will store the cache of the inverse matrix
	cc <- NULL
	set <- function (y) {
		x <<- y
		cc <<- NULL
	}
	get <- function () x
	setinverse <- function (inverse) cc <<- inverse
	getinverse <- function () cc
	list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
## If 'x' has cached data, it will return the cached matrix
## Otherwise, it will solve the inverse of 'x'
cacheSolve <- function(x, ...) {
	cc <- x$getinverse()
	if (!is.null(cc)) {
		message("getting cached data")
		return(cc)
	}
	data <- x$get()
	cc <- solve(data, ...)
	x$setinverse(cc)
	cc
}