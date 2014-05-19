## Put comments here that give an overall description of what your
## functions do

## The firest code creates a cached, or saved matrix along with its inverse
## The code runs the following way get will return the cashed value, if it exists
## while set will save a cached value that can be gotten with get later
## get inverse and set inverse also will work the same way, storing and retrieving the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

       m <- NULL
       
       ##This is the first set, it returns a function that has used <<- to cash Y, a matrix, in the enviornemtn
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       
       ## Having defined set, one needs a get to get the data 
       get <- function() x
       
       ##Setinverse will cashe an inverse in m, getinverse only gets back m
       
       setinverse <- function(solve) m <<- solve
       getinverse <- function() m
       
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)       
       
       
       
}


## cacheSolve determins if a cached copy of a matrix inverse has been created, matrix inverse solve 
## functions are time and reource intensive and it would be nice, if the work has already been donwe
## to use the cased version

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
       ##First we test to see if an inverse is cashed in the variable m
       m <- x$getinverse()
       
       ##If my has a value, not empty, we return the cached value and return m
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
              ##Function terminates at m
       }
       ##If no cache wth get the matrix, solve its inverse with solve and then set the inverse to be cached
       ## and returning the inverse m
       data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)

       m
       
       ##It should be clear that this will only solve a matrix inverse one time, thus in certian
       ##problems will reduce resources and time to run code
}
