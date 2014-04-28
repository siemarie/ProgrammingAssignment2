## The following script checks the cache for an existing inversed matrix given x.
## If found, the script returns the cached matrix. Otherwise, the script creates
## a new inversed matrix and stores it in the cache.

storedX<<-NULL

makeCacheMatrix <- function(x = matrix()) {
  set <<- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse <<- function(inverse) inv_x <<-inverse
  getinverse <<- function() inv_x
}

cacheSolve <- function(x, ...) {
  if(!is.null(storedX)) {
    if(identical(x,storedX)) {
      if (!is.null(inv_x)) {
        message("Retrieving inversed matrix from cache.")
        inv_x<<-inv_x
      } else {
        set(x)
        storedX<<-x
        message("Inversed matrix not found in cache. Creating a new inversed matrix.")
        require("MASS")
        library("MASS")
        makeCacheMatrix(x)
        inv_x <<- ginv(x)
        setinverse(inv_x)
      }
    } else {
      set(x)
      storedX<<-x
      message("Inversed matrix not found in cache. Creating a new inversed matrix.")
      require("MASS")
      library("MASS")
      makeCacheMatrix(x)
      inv_x <<- ginv(x)
      setinverse(inv_x)
    }
  } else {
    set(x)
    storedX<<-x
    message("Inversed matrix not found in cache. Creating a new inversed matrix.")
    require("MASS")
    library("MASS")
    makeCacheMatrix(x)
    inv_x <<- ginv(x)
    setinverse(inv_x)
  }
  inv_x
}
