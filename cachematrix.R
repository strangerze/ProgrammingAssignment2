## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create a  "matrix" object with 4 functions(set,get,setinverse,getinverse) to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- matrix()
        set <- function(y){
                x <<- y
                m <<- matrix()
        }    ##set the x and initialize the m
        get <- function() x  ##output the x
        setinverse <- function(y)  m<<-y  ## set the inverse of 'x'
        getinverse <- function(y)  m    ## output the inverse of 'x'
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## Compute the inverse of 'x' 
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.na(m)){
                return(m)  ##if the inverse has already been calculated, retrieve the inverse from the cache
        }
        matrix <- x$get()
        m<-solve(matrix)
        x$setinverse(m) ## set the inverse of 'x' to the cache
        m## Return a matrix that is the inverse of 'x'
}
