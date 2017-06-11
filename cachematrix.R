## The two functions are used to store a matrix and/or its inverse in a cache 
##defined by the environment and to calculate the inverse and/or to  store it.



## The "makeCacheMatrix" function, takes a matrix as an input.
## It builds a set of functions and returns the functions within a
## list to the parent environment.
## This function is useful to store the value of a matrix, and/or its inverse
## in a cache,to reduce the cost of calculating again and again.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
  
    #set
    set<- function(y){
        x<<-y
        inv<<- NULL
    }
  
    #get
    get <- function()x
  
    #set Inverse
    setInv <- function(inverse){
        inv<<- inverse
    }
  
    #get Inverse
    getInv <- function() inv
  
    #The list to be returned
    list(set = set,
        get = get,
        setInv = setInv,
        getInv = getInv)

}



## This function returns the inverse of the matrix, if the matrix 'x' is being 
## passed for the first time inverse is retained and also stored in the cache.
## If,the matrix 'x' is being passed the nth time (n>1) then the inverse
## is obtained from the cache.



cacheSolve <- function(x, ...) {
        
    #First, we attempt to get the inverse by checking the cache
    inv<- x$getInv()
  
    #If found, return the desired value from the cache, accompanied by the message
    if(!is.null(inv)) {
         message("Getting Cached Data!")
         return(inv)
    }
  
    #If not,store the value of x in a variable "data"
    data <- x$get()
  
    #Call the "solve()" fn, to find inverse and "setInv()" to store the inverse in cache
    inv <- solve(data, ...)
    x$setInv(inv)
  
    #To return the inverse
    inv
}
