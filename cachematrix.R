## - Cache the inverse of a matrix
##   > makeCacheMatrix() :  object to store a matrix and its inverse
##   > cacheSolve : compute the inverse of the matrix (or retrieve it from 
##                  cache if already computed)



## - This function creates a special "matrix" object that can cache its inverse
## - This function takes as input a matrix

makeCacheMatrix <- function(x = matrix()) 
{
    # - declare a matrix inverse (null until set)
    matInverse <- NULL
    
    # - set matrix and its inverse variables (overwrite)
    set <- function(y) 
    {
        # - the <<- operator can be used to assign a value to an object in an 
        #   environment that is different from the current environment
        x <<- y
        matInverse <<- NULL
    }
    
    # - get matrix
    get <- function() 
    {
        x
    }
    
    # - set the matrix inverse (bind to var one level above function)
    setInverse <- function(inverse) 
    {
        matInverse <<- inverse
    }
    
    # - get the matrix inverse
    getInverse <- function() 
    {
        matInverse
    }
    
    # - return list of setter and getter functions
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## - This function computes the inverse of the special "matrix" returned by 
##   makeCacheMatrix above. If the inverse has already been calculated (and the 
##   matrix has not changed), then the cachesolve should retrieve the inverse 
##   from the cache.
## - This function takes as input an object created by makeCacheMatrix()
##   the three dots alows user to pass additional arguments to the matrix
##   inverse function solve()

cacheSolve <- function(x, ...) 
{
    # - first try and get the inverse
    matInverse <- x$getInverse()
    
    # - check if inverse has already been cached, if so, return it
    if(!is.null(matInverse)) 
    {
        return(matInverse)
    }
    
    # - the matrix inverse has not already been computed
    #   proceed to compute it and set it in cache
    
    # - compute the inverse of the matrix
    mat <- x$get()
    matInverse <- solve(mat, ...)
    
    # - cache the inverse
    x$setInverse(matInverse)
    
    # - return inverse 
    matInverse
}
