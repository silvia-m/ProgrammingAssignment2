#STEP 1
#this function returns 4 functions operating on the matrix 'x'
makeCacheMatrix <- function(x = matrix()) {
        # initialise the inverse
        i <- NULL
        # 1) set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # 2) get the value of the matrix
        get <- function() x
        # 3) set the value of the inverse
        setinv <- function(inv) i <<- inv
        # 4) get the value of the inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#STEP 2
#this function checks if the cached inverse of matrix 'x' exists, and 
#- either returns it from cache
#- or calculates it and stores it in cache
cacheSolve <- function(x, ...) {
        #get the inverse
        i <- x$getinv()
        #check if the inverse is already calculated
        if(!is.null(i)) { # also check if the matrix is the same??
                message("getting cached inverse matrix data")
                #if already calculated, return it from cache
                return(i)
        }
        #if not yet calculated, get the matrix
        data <- x$get()
        #calculate the inverse of that matrix
        i <- solve(data, ...)
        #set the value of the inverse in the cache to the just calculated inverse
        x$setinv(i)
        #return the inverse
        i
}
