## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix defines functions set, get to write/read matrix resp.,  setInvMatrix/getInvMatrix to write/read Inverted Matrix resp.

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(UserInvMatrix) invMatrix <<- UserInvMatrix
    getInvMatrix <- function() invMatrix
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}

## cachesolve checks if Ivnerted Matrix is already cached in the memory, if not inverts matrix using solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInvMatrix()
    if(!is.null(invMatrix)) {
        message("getting cached Inverted Matrix")
        return(invMatrix)
    }
    mMatrix <- x$get()
    invMatrix <- solve(mMatrix, ...)
    x$setInvMatrix(invMatrix)
    invMatrix
}
