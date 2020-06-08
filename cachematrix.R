## makeCacheMatrix will create a matrix. It contains other functions such as:
## set and get (for matrix values), setInverse and getInverse (for inverse values)

## This function will create a matrix of specified size

makeCacheMatrix <- function(x = matrix()) {
       
       inver <- NULL
       set <- function(y)
       {
              x <<- y
              inver <<- NULL
       }
       get <- function()
       {
              x
       }
       setInverse <- function(inverse)
       {
              inver <<- inverse
       }
       getInverse <- function()
       {
              inver
       }
       list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function will inverse the matrix if its not already done

cacheSolve <- function(x, ...) {
       
        inver <- x$getInverse()
        if(!is.null(inver))
        {
               message("Getting cached data...")
               return(inver)
        }
        
        matrix <- x$get()
        inver <- solve(matrix, ...)
        x$setInverse(inver)
        inver
        ## Return a matrix that is the inverse of 'x'
}
