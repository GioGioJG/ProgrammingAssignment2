## The following functions are used to create a special object 
## that stores a matrix and caches the inverse
 
## The first function, makeCacheMatrix create a "matrix" that in fact it's a list that 
## contains a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
                INV <- NULL
                set <- function(y) {
                  x <<- y
                  INV <<- NULL
                }
                get <- function() x
                setINVERSA <- function(INVERSA) INV <<- INVERSA
                getINVERSA <- function() INV
                list(set = set, get = get,
                     setINVERSA = setINVERSA,
                     getINVERSA = getINVERSA)

}


## The second function gets the inverse of the “matrix” 
## returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
            INV <- x$getINVERSA()
            if(!is.null(INV)) {
              message("getting cached data")
              return(INV)
            }
            d <- x$get()
            INV <- solve(d, ...)
            x$setINVERSA(INV)
            INV
}
