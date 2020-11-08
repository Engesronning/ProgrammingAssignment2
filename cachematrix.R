## first function creates matrix object that gets and sets value of 
## a matrix and gets/sets value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m

      list( set = set,
            get = get,
            setinverse = setinverse,
            getinverse = getinverse)
  # print(x)
}


## Second function checks input matrix it has already been cashed
## if not previously cashed then invert the input matrix

cacheSolve <- function(x, ...) {
      ## retrieve matrix with getinvesre
      m <- x$getinverse()
      
      
      ##Checking if input matrix is same as unchanged input matrix
      if(!is.null(m)){
          message("Getting chached matrix")
          return(m)
      }
      ## if input not already inverted -> do the job
      data <- x$get()
      m <- solve(data,...)
      x$setinverse(m)
      m
}
