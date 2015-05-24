## Below are comments related to the inverted matrix and caching exercise.
## Step I. makeCacheMAtrix create an inveted matrix and caches it.
## Step II. cacheSolve references the previously cached inverted matrix and inverts it.

## Step I: First create the funtion to cache the value of our inverted special matrix. For that you need 4 functions:
## 1. set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the inverse of the matrix if it doens't exist.
## 4. Get the value of the matrix if it already exists.
## See the below 4 steps:

makeCacheMatrix <- function(x = matrix()) {
  
  invmat <- NULL
  # 1. Set a matrix and its value
  set <- function(mat){
    x <<- mat
    invmat <<- NULL
  }
  
  # 2. Get the value of the matrix after evaluating from the parent environment.
  get <- function() x
  
  # 3. Set the inverse (solve) of our matrix
  setinvmat <- function(solve) invmat <<- solve
  
  # 4. Get the inverse of our matrix
  getinvmat <- function() invmat
  
  # Make a list of all the previous 4 functions
  list(set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat)
}

## Step II: Return the cache of the inversed matrix. Two steps have to be completed:
## 1. Verify if the the inverse of our matrix already exists in the parent env. If it does, then grab that value and return it.
## 2. If the value of the inverse is not present, compute the value and return the output.

cacheSolve <- function(x, ...) {
  # Return the inversed matrix for 'x'
  invmat <- x$getinvmat()
  
  # 1. Verify if the inverse already exists if so return it.  
  if(!is.null(invmat)) {
    print("here's the cached inverse:")
    return(invmat)
  }
  
  # 2. If the inveser doens't exist, get the matrix newmat and inverse it.
  newmat <- x$get()
  invmat <- solve(newmat, ...)
  x$setinvmat(invmat)
  invmat
  
}

## ==== Change history ====
# 05162015 - first look at the file
# 05201015 - little commits.
# 05211015 - completed - needs testing.
# 05231015 - completed

