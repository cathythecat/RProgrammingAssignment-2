## Caching the Inverse of a Matrix:

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinverse(i)
        i
}
Testing My Functions:

> a=matrix(rnorm(16),4,4)
> makeCacheMatrix(a)
$set
function (y) 
{
    x <<- y
    i <<- NULL
}
<environment: 0x055859e0>

$get
function () 
x
<environment: 0x055859e0>

$setinverse
function (inverse) 
i <<- inverse
<environment: 0x055859e0>

$getinverse
function () 
i
<environment: 0x055859e0>

> cacheSolve(makeCacheMatrix(a))
            [,1]       [,2]       [,3]       [,4]
[1,] -2.69913193 -1.2504486  1.2697254  0.2113779
[2,] -0.03264755 -0.1248391  0.7872877 -0.1897640
[3,]  3.84006587  3.2683968 -3.0265637 -0.5062016
[4,] -1.54036364 -0.7612827  0.7252272  0.6026776
