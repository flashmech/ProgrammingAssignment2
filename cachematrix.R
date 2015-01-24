## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# variable cc is the cache of the inverse of this matrix
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