## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeCacheMatrix  creates a special "matrix" object that can cache its inverse.
## i is  inverse matrix, that its is null is not exist
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## firts cheket if the inverse has already calculate: 
##if not null(i) then the inverse has already calculate and show the massege "getting matrix inverse cache" 
##and and recover the matrix via getinverse and print in console the matrix inverse
## if not cahe the it calcualte the matrix inverse return the inverse and chache the matrix via setinverse  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached matrix inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


