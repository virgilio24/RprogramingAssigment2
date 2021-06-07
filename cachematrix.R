##Coursera.  Rprograming
#Student: Virgilio Chacon Cuadra


##In this assignment, several investigations were conducted, which allowed these functions to be 
#created to cache time-consuming calculations. They tried to carry out the required tests, 
#which are not incorporated in the delivery file.

## In the presented function, we tried to create a special "vector", so that it generates a list 
##that contains a function for theestablishment of the vector value and obtaining the vector
##value. Likewise, an attempt is made to establish the value of the mean and therefore the value 
##of said position measure.

makeCacheMatrix <-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function() {x}
  setInverse <- function (inverse) {inv<<-inverse}
  getInverse<- function() {inv}
  list(set=set,get=get,setInverse,getInverse=getInverse)
}


## The following function calculates the mean of the "vector" created in the first function. 
##Essentially it checks if the mean has already been calculated. If so, treat the cache mean and 
#skip the calculation. Otherwise, it calculates the mean of the data and sets the value of the mean 


cacheSolve <- function(x, ...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat, ...)
  x$setInverse(inv)
  inv
}
