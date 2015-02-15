## since the only difference between a vector and a matrix are the number of dimensions,
## I reason that the same code should work.   So, I have basically just copied the example
## given verbatim, except I changed m to i, mean to inverse and, in cacheSolve, I changed the
## call to the mean function to be a call to the solve function.

## like the example, there are four nested/child function: get, set, getinverse, setinverse.
## i define each child function in turn, and then return a list of all four functions to the 
## calling function .   this function is usefully thought of as a constructor function.

makeCacheMatrix <- function(x = matrix()) {
        ## set i to NULL, since it hasn't been used before.
        i <- NULL
        
        ## define the set function.   get the new matrix and clear any cached inverse.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## define the get function.  just return the matrix (non-inverted) to the caller.
        get <- function() x
        
        ## define setinverse.  the caller has already calculated the inverse of x and is 
		## giving it to us, so all that needs doing is to save the value
        setinverse <- function(inverse) i <<- inverse
        
        ## define getinverse.  the caller wants to see what is cached for the inverse.
        ## just return whatever is stored.   if it's a NULL, it's up to the caller to 
        ## calculate the inverse and then store it later using setinverse.
        getinverse <- function () i

        ## return the list of functions to the caller
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## so cacheSolve is the way that regular folks are going to ask to solve the cacheMatrix.
## cacheSolve works by first seeing if an inverse has been cached by the cacheMatrix.   if
## not, then it calculates the inverse (using regular solve) and saves its result to the
## cacheMatrix.   it then returns the same value (the calculated inverse) to the calling 
## function.

cacheSolve <- function(x, ...) {
        ## check to see if x has an inverse
        i <- x$getinverse()
        
        ## if it has an inverse, tell the user you're using cached data and return the
        ## cache contents
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
                ## end of function execution if a cache was found
        }
        ## if we're here, then there was no cache found.   time to build one.   start by
        ## getting the non-inverted matrix.
        data <- x$get()
        
        ## now, invert it.
        i <- solve(data)
        
        ## don't forget to cache that inverse you worked so hard for.
        x$setinverse(i)
        
        ## tell the user what she got
        i
}
