## makeCacheMatrix is a function that returns a list of different functions,
## when an input matrix is provided. Inside these list the four functions are:
#        1. set() sets the value of the matrix using input matrix.
#        2. get() gets the matrix
#        3. setInverse() Sets the value of inverse if calculated
#        4. getInverse() Gets the value of inverse matrix if exist. In the begining it is assigned as NULL value

makeCacheMatrix <- function(x = matrix()) {
  m  <- NULL
  set  <- function(y){
    x <<- y
    m <- NULL
  }
   
  get <- function() x
  
  setInverse  <- function(inverse){
    m <<- inverse
  }
  
  getInverse <- function() m
  
  list(set = set, get = get, setInverse= setInverse, getInverse = getInverse)
}


## cacheSolve is a function that uses "makeCacheMatrix" function in its #
#implementation. In the begining it checks if the inverse of the input matrix #
#exist in the cache. If exist it prints it otherwise computes the matrix.
#Furhter detailed information is given below:


cacheSolve <- function(x, ...) {
  m  <- x$getInverse() #uses the getInverse function from the output list and assigns the value to m. If inverse                  doesnot exist in cache, NULL is assigned to m.
  if(!is.null(m)){
    message("Getting cache data")
    return(m)
  }
# if the inverse doesnot exist:
  data  <- x$get() # assigns the matrix to data
  m  <- solve(data, ...) # computes the inverse of the data provided
  x$setInverse(m) # set the inverse value to m, to be stored in cache
  m
        ## Return a matrix that is the inverse of 'x'
}
