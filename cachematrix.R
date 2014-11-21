## Functions for calculating inverse of a matrix
## and caching the inverse matrix for later use

## This function makes the list of four functions
makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL # initially, matrix inverse is set to NULL
  
  set <- function(y) { # sets new matrix
    x <<- y
    mi <<- NULL        # if a new matrix is set, the inverse is not known
  }
  get <- function() x  # returns the parameter passed to makeCacheMatrix
  setinverse <- function(inverse) mi <<- inverse # sets inverse (mi) equal to 
                                                 # input parameter 'inverse'
  getinverse <- function() mi   # returns currently known inverse. May be NULL
  #print(mi)
  list(set = set, get = get,    # returns the list of functions
       setinverse= setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mi <- x$getinverse() # gets the cached inverse of x, can be NULL
  if(!is.null(mi)) {# if casched inverse is not NULL, i.e. inverse exists
    message("getting cached data")
    return(mi)      #..then cached inverse is returned, exits function 
  }  
  data <- x$get()  # if the inverse was NULL however,  values of x will be retrieved
  mi <- solve(data, ...)  # inverse of x will be calculated based on these values..
  x$setinverse(mi)        # ..and cached..
  #print(mi)
  mi                      # ..and returned
}


############################
## Exmple functions for vectors from course director
## Purely for comparison
## Commented out with single #s
############################

## Making the vector
#makeVector <- function(x = numeric()) {
#  m <- NULL # initiall, vector mean is set to NULL
#  
#  set <- function(y) { # whath is y??
#    x <<- y
#    m <<- NULL
#  }
#  get <- function() x
#  setmean <- function(mean) m <<- mean # 
#  getmean <- function() m
#  
#  list(set = set, get = get,
#       setmean = setmean,
#       getmean = getmean)
#}


## calculating vector mean
#cachemean <- function(x, ...) {
#  m <- x$getmean() # gets the cached mean of x
#  if(!is.null(m)) {# if casched mean is no NULL, i.e. mean exists
#    message("getting cached data")
#    return(m)      #..then cached mean is returned 
#  }  
#  data <- x$get()  # if the mean was NULL however,  values of x will be retrieved
#  m <- mean(data, ...)  # mean of x will be calculated based on these values..
#  x$setmean(m)     # ..and cached..
#  m                # ..and returned
#}
