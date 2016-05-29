## git repo source : kgk123
## Coursera Assignment - R Programming Week 3
## Demonstration of Lexical Scoping using the <<- operator
## How to use :
## Step 1 : Define an invertible square matrix y and assign as argument in makeCacheMatrix
## Step 2 : x <- makeCacheMatrix (y)
## Step 3 : x
## -- Observe x is a list of functions
## Step 3 : cacheSolve(x)
## --Observe results printed is inverse of matrix y
## Step 4 : cacheSolve(x)
## -- Observe result prints message "getting cached data"
## -- Observe result printed is inverse of matrix y
## Step 5 : overwrite y with new values and create the Cache Matrix
## Step 6 : x <- makeCacheMatrix (y)
## Step 7 : cacheSolve(x)
## -- Observe the "getting cached data" message is not printed
## Step 8 : cacheSolve(x)
## -- Observe the "getting cached data" message is printed


## function name	: makeCacheMatrix
## arguments 		: a square invertible matrix
## returns		: inverse of a matrix
## purpose		: generate a list of functions - set,get,setinverse,getinverse
## 		 	  cache the matrix and the inverse computed
makeCacheMatrix <- function(x = matrix()) {
      
	## i stores the inverse matrix
	i <- NULL

	## store the input matrix and reset inverse matrix
	setmatrix <- function(y) {
		x <<- y
		i <<- NULL
	}
      
	## return the input matrix
	getmatrix <- function() x

	## set inverse matrix defined in the higher environment for caching
	setinverse <- function(inverse) i <<- inverse

	## i is the inverse matrix returned
	getinverse <- function() i

	## return a list of functions
	list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}

## function name	: cacheSolve 
## arguments 		: a list of functions
## returns 		: inverse of a matrix
## purpose		: Assume the matrix is invertible. 
##			  No checks added to keep code simple Eg:(det(data)!= 0)
cacheSolve <- function(x,...) {
       
	  ## get the matrix from cache
        i <- x$getinverse()

	  ## check if matrix is null
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }

	  ## get the input matrix
        data <- x$getmatrix()

	  ## calculate invers
        i <- solve(data)

	  ## cache the input matrix
        x$setinverse(i)

	  ## return the inverse matrix
        i
}
