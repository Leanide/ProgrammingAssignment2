## cacheSolve function allows to calculate the inverse matrix,
## and using makeCacheMatrix function, use this result
## without calculating it again.


## makeCacheMatrix creates list containing four functions,
## which allows using calculated result for cacheSolve function.
## Argument "x" is invertible square matrix.

makeCacheMatrix <- function(x = matrix()) {

    s <- NULL
	
	set <- function(y){
	    x <<- y
		s <<- NULL
	}
	
	get <- function() x
	
	setsolve <- function(solve) s <<- solve
	
	getsolve <- function() s
	
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## cacheSolve function trying to get calculated result from makeCacheMatrix function,
## if this is not possible, the result is calculated and returned.
## Argument "x" is makeCacheMatrix function's returned result.

cacheSolve <- function(x, ...) {
	
	s <- x$getsolve()
		
	if(!is.null(s)){
		message("getting cached data")
		return(s)
	}
		
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
		
}

