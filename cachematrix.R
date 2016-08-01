#This functions compute the inverse of a matrix making sure that the inverse is not computed
#again if it was already computed.  


# Based on the course's example. It returns a list of three functions which are used by cacheSolve
# to verify if the inverse was already computed, and if not to compute it and keep it in the cache.

makeCacheMatrix <- function(x = matrix()) {
  cacheinv <- NULL
  get <- function() x
  setinv <- function(inverse) cacheinv <<- inverse
  getinv <- function() cacheinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## takes the list of functions as its argument. Since the list contains the body of the funcions AND their enclosing
#environments it is possible to retrieve the value of cacheinv (the inverse or NULL otherwise). If the cacheinv is NULL 
# cacheSolve computes the inverse, otherwise it just uses the function getinv to get the cached inverse

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  datmatrix <- x$get()
  inverse <- solve(datmatrix)
  x$setinv(inverse)
  inverse
}


#test: a 2x2 invertible matrix
c=rbind(c(1, -1/4), c(-1/4, 1))

#test the functions
listoffunctions <- makeCacheMatrix(c)
cacheSolve(listoffunctions)


environment(listfromfunction$get)

