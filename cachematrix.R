## Use "makeCacheMatrix" to initialize an inverse-caching matrix list. Use 
## "cacheSolve" to compute or retrieve the inverse from the list.
## 

## Make a new cache matrix list which stores its values through getter and 
## setter functions defined in the function environment (<<- operator). Can either 
## provide a matrix as input to initialize the value, or can set the value later.
makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInv <- function(theInverse) {inv <<- theInverse}
  getInv <- function() {inv}
  return(
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv))
}


## Call solve on the cache matrix list x and set the cached inverse. Note that 
## the ", ..." from the template has been removed so that it is not possible to 
## input a "b" matrix and compute a solution to a linear system instead of a 
## matrix inverse.

cacheSolve <- function(x) 
{
  inv <- x$getInv()
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  
  theMatrix <- x$get()
  inv <- solve(theMatrix)
  x$setInv(inv)
  return(inv)
}
