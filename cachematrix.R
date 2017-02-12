## Function makeCacheMatrix creates a catche matrix and associated list of functions
## Function cacheSolve computes inverse of the catche matrix by solve, if the inverse already 
##   exists in cache, it will not be recomputed. 

makeCacheMatrix <- function(x = matrix()) {
  ## Returns: A list of functions to: Set x, Get x, Set x inverse, Get x inverse
  ##----------------------------------------------
  ## Initialization:
  ## x      : is initialized as a square matrix in function argument, assumed invertible
  ## matinv : is set to NULL, initializing it as an object within the makeVector() environment to be used by later code in the function.
  matinv = NULL
  ##--------------------------------------------
  
  ## Mutator (setter) function. Sets the value of x (to y) and matinv (to NULL)
  ## in the parent environment.
  ## The cache inverse function will test for existing inverse value, when NULL, inverse will 
  ## be recalculated
  set = function(y) {
    x <<- y
    matinv <<- NULL
  }
  
  ## Retriever (getter) function. Gets the value of x. Since x is not defined in the 
  ## function below, call to get() will get value of x from parent environment 
  get = function() x
  
  
  ## Since matinv is defined in the parent environment and we need to access it after 
  ## set_matinv() completes, the code uses the <<- form of the assignment operator 
  ## to assign the input argument to the value of matinv in the parent environment.
  set_matinv = function(inverse) matinv <<- inverse 
  
  #Gets the value of matinv from parent environment
  get_matinv = function() matinv
  
  # Setting up the function output as a list. Each element in the list 
  # is assigned as elementName = value 
  
  ## Names 'set' ----> set() function 
  ## Names 'get' ----> get() function 
  ## Names 'set_matinv'-> set_matinv() function 
  ## Names 'get_matinv'-> get_matinv() function 
  list(set=set, get=get, set_matinv=set_matinv, get_matinv=get_matinv)

}


## Function below will return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  ## Input:  x as type CacheMatrix [list of set(), get(), set_matinv(), get_matinv()]
  ## Output: inverse of CacheMatrix
  
  matinv = x$get_matinv()
  
  # if the inverse exists for the CacheMatrix
  if (!is.null(matinv)){
    message("getting the cached inverse")
    return(matinv)
  }
  
  # Inverse Computation
  data <- x$get()
  matinv = solve(data, ...)
  
  # Set the inverse matrix to x
  x$set_matinv(matinv)
  
  # Return the new matinv
  matinv
}
