makeCacheMatrix <- function(x,y,z) 
    {
    m <- NULL
    x <- matrix(x, nrow = y, ncol = z)
    set <- function(y)  {
                        x <<- matrix(x, nrow = y, ncol = z)
                        m <<- NULL
                        }
    
    get <- function()x 
    setReverse <- function(Reverse) m <<- Reverse
    getReverse <- function() m
    return(list(set = set, get = get,
                setReverse = setReverse,
                getReverse = getReverse))
  }


cachesolve <- function(x) {
  m <- x$getReverse()
  if(!is.null(m)) {
   message("getting cached data")
    return(m)
  }
 
  data <- x$get()
  m <- solve(data)
  x$setReverse(m)
  m
}
