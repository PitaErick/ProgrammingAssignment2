## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Functions that cache the results data.
## The result is passed from a makeCacheMatrix call to cacheSolve
makeCacheMatrix <- function(x = matrix()) {
	j <- NULL
	set <- function(y){
		x <<- y
		j <<- NULL
	}
	get <- function()x
	setInverse <- function(inverse) j <<- inverse
	getInverse <- function() j
	list(set = set, get = get, 
		setInverse = setInverse, 
		getInverse = getInverse)
}


# Code that calculates and caches data from the inverse of an array
# '@param x is the result of a previous call to makeCacheMatrix
# '@param ... are additional arguments to pass to the resolve function

cacheSolve <- function(x, ...) {
	j <- x$getInverse()
	if(!is.null(j)){
		message("getting cached data")
		return(j)
	}
	mat <- x$get()
	j <- solve(mat,...)
	x$setInverse(j)
	j
}
