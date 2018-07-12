## Put comments here that give an overall description of what your
## functions do

## stores the inverse matrix if already calculated to reduce computation time

makeCacheMatrix <- function(x = matrix()) {
	inv_m <- NULL
	set_m <- function(y) {
		x <<- y
		inv_m <<- NULL
	}
	get_m <- function() x
	set_Inv <- function(Inverse_m) inv_m <<- Inverse_m
	get_Inv <- function() inv_m
	list(set_m = set_m, get_m = get_m, set_Inv = set_Inv, get_Inv = get_Inv)
}


## checks and calculates the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$get_Inv()
        if(!is.null(inv_m)) {
        message("getting cached data")
        return(inv_m)
        }
        data <- x$get_m()
        inv_m <- solve(data)
        x$set_Inv(inv_m)
        inv_m
}
