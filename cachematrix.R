## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Inverse matrices are stored in the memory using scoping rules
makeCacheMatrix <- function(x = matrix()) {
		matrixInverse <- NULL
		set <- function(y) {
			x <<- y
			matrixInverse <- NULL
		}
		get <- function() x
		setInverse <- function(Inverse) matrixInverse <<- Inverse
		getInverse <- function() matrixInverse
		list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}

## Write a short comment describing this function
## Function to return the matrix inverse
cacheSolve <- function(x, ...) {
        ## Here corcopr librarary is used to generate inverse of the matrix 
		## to avoid singularity error
		if(require("corpcor")){
			print("corpcor library is already loaded")
		} else {
		print("Installing corpcor library")
		install.packages("corpcor")
		if(require(corpcor)){
			print("corpcor installed and loaded")
			} else {
				stop("could not install corpcor")
			}
		}
		
		## Return a matrix that is the inverse of 'x'
		inverse <- x$getInverse()
		if(!is.null(inverse)){
			message("getting cached matrix from the memory")
		return(inverse)
		}
		message("The inverse of the matrix is not in memory so the inverse is computed")
		data <- x$get()
		inverse <- pseudoinverse(data, ...)
		x$setInverse(inverse)
		inverse
}