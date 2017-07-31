## Functions are coded processes that take input from a user and generate
## a desired output with the data provided

## This function creates a special matrix which can
##cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  f <- NULL
  set <- function(y) {
    x <<- y
    f <<- NULL
  }
  get<-function() x
  setinv<-function(solve) f <<- solve
  getinv<-function() f
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x,...) {
          f<-x$getinv()
          if(!is.null(f)){
              message("getting cached data")
              return(f)
          }
          to_be_inverted <- x$get()
          f <- solve(to_be_inverted,...)
          x$setinv(f)
          return(f)
        ## Return a matrix that is the inverse of 'x'
}

