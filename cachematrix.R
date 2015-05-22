##05162015 - first look at the file
##05201015 - little commits.
## Below are comments related to the inverted matrix and caching exercise.
## 1. makeCacheMAtrix create an inveted matrix and caches it.
## 2. cacheSolve references the previously cached inverted matrix and inverts it.

## first crate a matrix fct
makeCacheMatrix <- function(x = matrix()) {

  ## wrong copy paste job from the sample code for reference.
  m <<- x$getsolve() 
  if(!is.null(m){
    print("here's the cached inverse of matrix x:")
    return(m)
  }
  data <- x$getsolve()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
  ##m <- NULL
  m <- square(x)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- mean
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## inverting the cached matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Code to write.
}
