##  This file contains two functions that implement a 
## smart caching functionality for *heavy* objects.
## 
## USAGE:
## a smart object can be created like this:
## cached_m <- makeCacheMatrix(matrix(c(1,0,0,1),nrow=2,ncol=2))
##
## The smart object can be used like this:
## solution <-cacheSolve(cache_m)
##

## This function creates a cache for a *heavy* object

makeCacheMatrix <- function(x = matrix()) {
            inverse_matrix <- NULL
            set <- function(mat) {
                    x <<- mat
                    inverse_matrix <<- NULL
            }
            get <- function() x
            set_solution <- function(solution) inverse_matrix <<- solution
            get_solution <- function() inverse_matrix
            list(set = set, get = get,
                 set_solution = set_solution,
                 get_solution = get_solution)

}


## This funciton performs a smart calculation: It first consults
## a cache. If a cached object is found the funciton uses it to save
## time and resources on *heavy* object calculation. If the cache is empty
## it performs the calculation and stores the object in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            solution <- x$get_solution()
            if(!is.null(solution)) {
                    message("using cached solution")
                    return(solution)
            }
            data <- x$get()
            solution <- solve(data, ...)
            x$set_solution(solution)
            solution
}
