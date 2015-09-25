## makecacheMatrix gives a list of 4 functions to be used by 
## cacheSolve function to get or set the Inverse of the Matrix in cache

makeCacheMatrix <- function(x = matrix()) {
        
        #Initialize the cache
        cache<-NULL
        
        
        #1. The set function to create Matrix
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        
        #2. The get function to get the Matrix values
        get <- function() x
       
        #3. The setInverseMatrix function to inverse the matrix and store 
        # it in the cache
        setInverseMatrix <- function(inverse) cache <<- inverse
        
        #4. The getInverseMatrix function to get the inverted matrix from
        # the cache
        getInverseMatrix <- function() cache
        
        # return the list of 4 functions
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}



## Calculates the Inverse of the Matrix

cacheSolve <- function(x, ...) {
        ## attempt to get the inverse of the matrix stored in cache
        cache <- x$getInverseMatrix()
        
        # check whether cache exists, if yes return the cache
        if (!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        
        # create matrix as it doesnot available in the cache
        matrix <- x$get()
        
        # SOlve function gives the inverse of a Matrix
        cache <- solve(matrix, ...)
        
        #Set the inverted Matrix in the cache 
        x$setInverseMatrix(cache)
        
        #Return the cache
        return (cache)
}
