# cachematrix.R ---> de JoGre78
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This makeCacheMatrix function sets a matrix object and reads its values
# It also set an inverse square matrix and get its values
# A matrix object can set to a cache its own object


# Define a matrix X as an entry
makeCacheMatrix <- function(x = matrix()) {

	# Set an empty inverse matrix
	inverse_Matrix <- NULL
	
	# Set the matrix
	set_Matrix <- function (y){
		
		# <<- operator used to assign a value to an object in a different environment
		x <<- y
		inverse_Matrix <<- NULL
	}
	# Get the values of the Matrix
	get_Matrix <- function() x
	
	# Set the inverse Matrix
	set_InvMatrix <- function(inverse) inverse_Matrix <<- inverse
	
	# Get the values of the Inverse Matrice
	get_InvMatrix <- function() inverse_Matrix
	
	# Return a list with the different function set/get of the matrix and its inverse
	 list(	set_Matrix=set_Matrix,		get_Matrix=get_Matrix,
	 		set_InvMatrix=set_InvMatrix,get_InvMatrix=get_InvMatrix)
}


## Write a short comment describing this function

# cacheSolve function uses the output of the makeCacheMatrix as an input
# check if it is empty - if so it uses the orignale matrix and generates its inverse and return it
# if not empty it reurns directly the inverse matrice

cacheSolve <- function(x, ...) {
	
		# set inverse of X
		inverse_Matrix <- x$get_InvMatrix()
		
		# check if the Inverse Matrix is not NULL
		if (! is.null(inverse_Matrix)){
			# get it from the cache and return it
			message ( "Not Null. Getting cached inverse matrix...")
			return(inverse_Matrix)
		}
		# Otherwise set & get the Inverse Matrix
		# Get originale matrix
		Matrix <- x$get_Matrix()
		# Use Solve function to set inverse matrix
		inverse_Matrix <- solve(Matrix,...)
		#set Inverse Matrix
		x$set_InvMatrix(inverse_Matrix)
		# return inverse matrix
		return(inverse_Matrix)
        ## Return a matrix that is the inverse of 'x'
}
