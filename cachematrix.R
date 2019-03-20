## Our aim in this experiment is to write a pair of functions, namely,  
 ## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix 
 

 ## makeCacheMatrix is a function which creates a special "matrix" object that can  
 ## cache its inverse for the input (which is an invertible square matrix) 

makeCacheMatrix <- function(x,y,z) 
    {
    m <- NULL
    x <- matrix(x, nrow = y, ncol = z) #Creating matrix based on input variables
    set <- function(y)  {
                        x <<- matrix(x, nrow = y, ncol = z)
                        m <<- NULL
                        }
    
    get <- function()x  #get matrix
    setReverse <- function(Reverse) m <<- Reverse #set reverse of matrix
    getReverse <- function() m #Get reverse of matrix
    return(list(set = set, get = get, 
                setReverse = setReverse,
                getReverse = getReverse)) #output
  }

## cacheSolve is a function which computes the inverse of the special "matrix"  
 ## returned by makeCacheMatrix above. If the inverse has already been calculated  
 ## (and the matrix has not changed), then the cachesolve should retrieve the  
 ## inverse from the cache 

cachesolve <- function(x) {
  m <- x$getReverse() #Checks if inverse has already been calculated and is present in memory
  if(!is.null(m)) {
   message("getting cached data")
    return(m)
  }
 
  data <- x$get()
  m <- solve(data) # calculate inverse of matrix 
  x$setReverse(m) # returns inverse of given matrix
  m
}

 ## ---------------Checking the program------------------------ 

# x<-makeCacheMatrix(c(2,3,4, 9,8,7,-7,-5,-2),3,3)
# cachesolve(x)
#          [,1]      [,2] [,3]
#[1,] -1.727273  2.818182   -1
#[2,]  1.272727 -2.181818    1
#[3,]  1.000000 -2.000000    1
