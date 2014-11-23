## Notes:
#' Found this post extremely helpful in providing a working 
#' well commented example:
#' https://class.coursera.org/rprog-009/forum/thread?thread_id=457

makeCacheMatrix <- function(x) {      # input x will be a matrix
  print(x)
  m <- NULL    #  m will be our 'inverse' and it's reset to NULL every 
  #    time makeVector is called
  
  #  note these next three functions are defined but not run when makeVector is called.
  #   instead, they will be used by cachemean() to get values for x or for
  #   m (mean) and for setting the mean.  These are usually called object 'methods'
  
  get <- function() { 
    x 
  }   # this function returns the value of the original matrix
  
  set <- function(y) {    # takes an input matrix
    x <<- y         # saves the input matrix
    m <<- NULL      # resets the inverse to NULL, basically what happens when a new object is generated.
  }
  
  setSolve <- function(solve)  { 
    m <<- solve 
  }
  # this is called by cacheSolve() during the first cacheSolve()
  #  access and it will store the value using superassignment
  
  getSolve <- function() { m } # this will return the cached value to cacheSolve() on
  #  subsequent accesses
  
  list(get = get,          #  OK, this is accessed each time makeCacheMatrix() is called,       
       setSolve = setSolve,  #   that is, each time we make a new object.  This is a list of 
       getSolve = getSolve)  #   the internal functions ('methods') so a calling function
  #   knows how to access those methods.      
  
}

cacheSolve <- function(x, ...) {   # the input x is an object created by makeCacheMatrix
  m <- x$getSolve()               # accesses the object 'x' and gets the inverted matrix
  if(!is.null(m)) {              # if inverse was already cached (not NULL) ...
    
    message("getting cached data")  # ... send this message to the console
    return(m)                       # ... and return the mean ... "return" ends 
    #   the function cachemean(), note
  }
  data <- x$get()        # we reach this code only if x$getSolve() returned NULL
  m <- solve(data, ...)   # if m was NULL then we have to calculate the inverse
  x$setSolve(m)           # store the calculated inverse value in x (see setSolve() in makeVector
  m               # return the mean to the code that called this function
}

set.seed(19)
mx <- matrix(rnorm(4), nrow = 2,ncol = 2)
bM <- makeCacheMatrix(mx) 
cacheSolve(bM) 
cacheSolve(bM) 
solve(mx)
