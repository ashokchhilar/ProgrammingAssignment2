## There are two functions in this file
## 1. makeCacheMatrix: This function defines a sperial matrix object that can cache its inverse in it
## 2. cacheSolve : This function takes as input the datastructure defined above and returns the inverse of
##        the matrix defined in makeCacheMatrix. This function returns cached inverse value whenever available 
##        or calculate a fresh inverse matrix if it is a new matrix (or was changed since last inverse calculation)

## Sample Usage
##>  B<-matrix(c(1,2,3,4), nrow=2, ncol=2)
##>  m<-makeCacheMatrix(B)
##>  B_inv <- cacheSolve(m)
##>  B_inv
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

##>  B %*% B_inv
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1


## This function creates a special "matrix" object
## that can cache its inverse
## It is implemented as a list with following operations
## 1.  set the value of the marix
## 2.  get the value of the matrix
## 3.  set the value of the matrix inverse
## 4.  get the value of the matrix inverse
## This function is designed to be used in conjunction with
## function cacheSolve to calculate the inverse of the matrix whenever required

makeCacheMatrix <- function(x = matrix()) {
  inverse<- NULL   #this is used as inverse cache
  
  set<-function(y){
    #set the value of matrix, also reset value of inverse
    x<<-y
    inverse<<- NULL
  }
  
  #function to return raw matrix
  get<-function() x
  
  #this will be set by cacheSolve when an inverse is calculated to cache it
  setinverse<-function(inv) inverse <<- inv
  
  #return the cached inverse value
  getinverse<-function() inverse
  
  #expose functions as a list
  list(set=set, get=get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## itretrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  
  #check if a cached value exists
  if(!is.null(m)){
    # Message to indicate that the value is taken from cache (optional)
    message("getting cached data")
    return(m)
  }
  
  #If inverse doesnt already exists, we data to calculate inverse
  data<-x$get()
  
  #calculate the matrix inverse
  m<-solve(data, ...)
  
  #set the calculated inverse value in CacheMatrix object
  x$setinverse(m)
  
  #return the inverse
  m
}


