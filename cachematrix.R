## Taking the inverse of a matrix is a very expensive operation.  If this is done repeatively inside of a loop 
## for a large matrix, this can add a lot of time to the execution of the program.  If the matrix remains static
## throughout the execution of the program, you can use the below caching function to cache the inverse of the matrix.

## Here is an example of usage:
##   > y <- makeCacheMatrix(matrix(rnorm(25),5,5))
##   > y$get()
##              [,1]       [,2]       [,3]        [,4]       [,5]
##   [1,]  1.3298710  0.2597559  0.6683538  1.83610886 -0.8618265
##   [2,]  0.5906217  0.7563500  0.9217894  0.04428946  0.6447108
##   [3,]  1.4086042  0.9035579  0.6075207 -0.35727633  1.2515353
##   [4,] -0.4799988 -0.4410498 -0.3670124 -0.39079758  0.6125625
##   [5,]  1.4515399  1.2354427 -0.3006592 -0.23120268  1.2381546
##   > cacheSolve(y)
##               [,1]       [,2]       [,3]       [,4]       [,5]
##   [1,]  0.15842041 -1.2919852  1.3498687 -0.4131082 -0.3770660
##   [2,] -0.33296680  1.1320129 -1.3489296 -0.7404430  0.9086265
##   [3,] -0.03358036  0.4703419  0.5202806 -0.1558978 -0.7170569
##   [4,]  0.60739652  0.9193677 -1.1267203  1.1147503  0.5314523
##   [5,]  0.25178059  0.6710019 -0.3205874  1.3934278  0.2681845
##   > 

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Return a matrix that is the inverse of x$get()
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
