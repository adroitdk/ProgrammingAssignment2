## --------------------------------------------------------------------------------------------------------
## Functions created as per requirements of Programming Assignment 2 of course R Programming
## Revision History
## --------------------------------------------------------------------------------------------------------
## Revision History:
##   April 27, 2014, Dhawal Kulkarni - Initial version based on assignment specifications
##
## Contents:
## 1) makeCacheMatrix: function creates a special "matrix" object that can cache its inverse.
## 2) cacheSolve: function computes the inverse of the special "matrix" object returned by makeCacheMatrix. 
##
## Example usage:
## 		Set up invertible matrix to use:
##
##			> x <- matrix(c(1,1,1,3,4,3,3,3,4),nrow=3,ncol=3)
##
##			> x
##			[,1] [,2] [,3]
##			[1,]    1    3    3
##			[2,]    1    4    3
##			[3,]    1    3    4
##
##    Check that the matrix has an inverse:
##
##			> solve(x)
##			     [,1] [,2] [,3]
##			[1,]    7   -3   -3
##			[2,]   -1    1    0
##			[3,]   -1    0    1
##
##    Build cached object cm corresponding to calculation of inverse of matrix x
##			> cm<-makeCacheMatrix(x)
##
##    First call to use cm for computing inverse of matrix x
##			> invm<-cacheSolve(cm)
##
##    note: NO message indicating that cached value is being used to return the inverse matrix
##
##    Verify inverse matrix returned:
##			> invm
##			     [,1] [,2] [,3]
##			[1,]    7   -3   -3
##			[2,]   -1    1    0
##			[3,]   -1    0    1
##
##    Second call to use cm for computing inverse of matrix x
##			> invm<-cacheSolve(cm)
##			getting cached data
##
##    note: message indicating that cached value is being used to return the inverse matrix
##
##    Verify inverse matrix returned:
##			> invm
##			     [,1] [,2] [,3]
##			[1,]    7   -3   -3
##			[2,]   -1    1    0
##			[3,]   -1    0    1
##   			
## --------------------------------------------------------------------------------------------------------

## --------------------------------------------------------------------------------------------------------
## makeCacheMatrix: function returns a special "matrix" object that helps manage computing and caching 
##                  inverse of a given matrix.
## input:  x - an invertible matrix
## output: a list of four functions:
##				 1) set        - set the value of cached version of matrix x
##         2) get        - get the value of cached version of matrix x
##         3) setinverse - set the cached value of inverse of matrix x
##         4) getinverse - get the cached value of inverse of matrix x
## logic:
##         1) define function "set" to assign the value of matrix in its enclosing environment to its input
##         2) define function "get" to return the value of matrix in its enclosing environment
##         3) define function "setinverse" to use the R function "solve" and return inverse of its input
##         4) define function "getinverse" to return the value of the variable used to store inverse matrix
##         5) create and return the list (get, set, setinverse, getinverse)
##
## --------------------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        ## x - input matrix; default would be an empty 1 X 1 matrix (see help on matrix())
				## part of makeCacheMatrix environment because it is an argument to it
				
        ## m - variable to hold inverse matrix of x 
				## part of makeCacheMatrix environment because it is a local variable in it
				m <- NULL
				
				## makeCacheMatrix environment is the enclosing environment for functions 
				## "set", "get", "setinverse","getinverse" defined below because of lexical scoping
				
				## Function "set" takes y as an argument and assigns x in its enclosing environment to it
				##   it also resets value of m in its enclosing environment to NULL
				##   note: <<- operator is used to set the values of variables in enclosing environment
				
				set <- function(y) {
								## When this function completes its execution, 
								##   values of x and m in enclosing environment are modified
								x <<- y 
								m <<- NULL
				}
				
				## Function "get" takes no arguments and returns x (free variable inside get) therefore
				## x from its enclosing environment is returned
				get <- function() x
				
				## Define function "setinverse" that takes matrix z as an argument
				## and assigns it to inverse matrix variable m in its enclosing environment
				setinverse <- function(z) m <<- z

				## Define function "getinverse" that returns value of m from its enclosing environment 
				getinverse <- function() m
				
				## create and return list of (set, get, setinv, getinv)
				list(set = set, get = get,
						 setinverse = setinverse,
						 getinverse = getinverse)
}

## --------------------------------------------------------------------------------------------------------
## cacheSolve: function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## input:  cx - cached object for an invertible matrix
## output: inverse matrix from cached object 
##         if cached object does not have the inverse computed prior to a call, then it is computed and returned
##         if cached object has the inverse matrix already computed prior to the call then the previous 
##            cached result is returned
## logic:
##         1) Use the "getinverse" function from the cached object to get the value of inverse matrix
##				 2) Returned value is NOT null (i.e. inverse matrix has been computed prior to this call)
##    				print message that the cached value is being returned
##         3) If returned value is null (i.e. inverse matrix has not been computed prior to the call), then
##            3a) use the "get" function to assign value of the matrix for which inverse has to be computed
##                to a local variable "data" 
##            3b) compute inverse of data using "solve" function and assign its value to local variable invm
##            3c) Use the "setinverse" function from the cached object cx to set the value of inverse matrix
##                in enclosing environment to invm
##            3d) return the inverse matrix computed - invm 
##
## --------------------------------------------------------------------------------------------------------
cacheSolve <- function(cx, ...) {
				## 1) Use the "getinverse" function from the cached object cx to get the value of inverse matrix
				invm <- cx$getinverse()
				 
				if(!is.null(invm)) {
								## 2) Returned value is NOT null (i.e. inverse matrix has been computed prior to this call)
								##    print message that the cached value is being returned
								message("getting cached data")
								return(invm)
				}
				
				## code reaches this point if "getinverse" returns null value in invm
				
				## 3a) use the "get" function to assign value of the matrix for which inverse has to be computed
        ##     to a local variable "data" 
        data <- cx$get()
				
				## 3b) compute inverse of "data" using "solve" function and assign its value to local variable invm
        invm <- solve(data, ...)
				
				## 3c) Use the "setinverse" function from the cached object cx to set the value of inverse matrix
				##     in enclosing environment to invm
        cx$setinverse(invm)
				
				## 3d) return the inverse matrix computed - invm 
        invm				
}

