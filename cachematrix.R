##########################
# Coursera R Programming #
#Programming Assignment 2#
##########################


#Introduction
#============

#The goal of this assignment is to write two R functions
#to cache the inverse of a matrix, 
#potentially time-consuming computations.

#The "makeCacheMatrix" and "cacheSolve" functions are
#directly adapted from the examples given
#in the Programming Assignment 2 instructions,
#"makeVector" and "cachemean".


#First function
#==============

#This function creates a special "matrix" object 
#that can cache its inverse.
#Here, the input is a matrix, and not a vector like it is
#in the example in the instructions.

makeCacheMatrix <- function(m = matrix()){
  
  #Initialization
  i <- NULL 
  
  #Sets the matrix
  set <- function(matrix){
    m <<- matrix
    i <<- NULL
    #the <<- operator can be used to assign a value 
    #to an object in an environment that is different 
    #from the current environment.
  }
  
  #Gets the matrix
  get <- function(){
  #Returns it
  m 
  }
  
  #Sets the inverse of the matrix
  setinverse <- function(inverse){
    i <<- inverse
  }
  
  #Gets the inverse of the matrix
  getinverse <- function(){
  #Returns it
  i 
  }
  
  #Returns the list of functions in the "makeMatrix" function
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



#Second function
#===============

#This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above.
#If the inverse has already been calculated 
#(and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  
  #Returns the inverse of the matrix x
  m <- x$getinverse()
  
  #Returns the inverse of x if it was already set
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  #Gets the matrix
  data <- x$get()
  
  #Calculates the inverse
  m <- solve(data) %*% data 
  # "%*%" corresponds to matrix multiplication
  
  #Sets the inverse
  x$setinverse(m)
  
  #Returns the matrix
  m
}
