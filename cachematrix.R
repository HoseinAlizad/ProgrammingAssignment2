# Function for Creating a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # initializing
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function (inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) #output as list
}

# function for getting the cache
cacheSolve <- function(x, ...) # Return a matrix that is the inverse of 'x'
{
  inv <- x$getInverse()
  # checking inverse state
  if(!is.null(inv)){
    message("getting cached data")
    # return Inverse Value
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) #calculating matrix inverse with solve function
  x$setInverse(inv)
  inv #return the result      
}