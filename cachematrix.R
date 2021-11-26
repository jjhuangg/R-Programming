## the aim is to write 2 functions tp cache the inverse of a matrix 

## makeCacheMatrix creates a special matrix object

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3) {
        s <- NULL
        set <- function(y){
          x << - y
          s << - NULL
                
         }
        get <- function() x
        setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}
                            
## cacheSolve computes the inverse of the special matrix

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s 
}
