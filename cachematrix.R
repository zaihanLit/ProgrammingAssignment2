## two functions:
## 1.makeCacheMatrix()
## 2.cacheSolve()

## this function return a list which has 4 methods:
## 1.set the matrix
## 2.get the matrix
## 3.set the inverse of the matrix
## 4.get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(val) {
                x <<- val
                inv_x <<- NULL
        }
        get <- function() x
        setinv <- function(inv_val) inv_x <<- inv_val
        getinv <- function() inv_x
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## this function use the list created by makeCacheMatrix to caculate the inverse of matrix
## if the inverse of matrix has been cached, then return it
## if not, use solve() to get the inverse of matrix, cache the inverse and return
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        inv_data <- solve(data)
        x$setinv(inv_data)
        inv_data
}
