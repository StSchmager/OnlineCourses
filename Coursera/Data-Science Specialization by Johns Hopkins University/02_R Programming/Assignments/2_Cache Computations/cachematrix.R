## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function
#1) Applied the equivalent from the Cache-Vector-Mean example.
#2) More specific comments are added to explain the function.

makeCacheMatrix     <- function(x = matrix()) {
                    # This function takes a matrix as its input, that is by default set as class: matrix
                    # (a different input object class would be coerced)
                    # The input matrix is assumend to be invertible

                    i <- NULL 
                    # NULL is pre-assigned as an inverse matrix in order to display in the output, ...
                    # ... that no inverse matrix has been calculated yet.
                    
                    set  <- function(y) {
                    # This sub-function takes an alternate matrix as an input and the original variables are ...
                    # ... overwritten by new variables.
                    # '<<-' assigns a value (new matrix) to an object (former matrix) in an environment that is different from ...
                    # the current environment, the set-function environment.
                         x <<- y
                         # The matrix that was given in the first place is overwritten by the ...
                         # ... new matrix that the sub-function declared.
                         i <<- NULL
                         # NULL is still assigned as an inverse matrix by default.
                         }
                    
                    get <- function() x
                    # This sub-function prints the current input matrix.
                    
                    setinverse <- function(inverse) i <<- inverse
                    # This sub-function assigns a particular inverse matrix to the inverse-matrix variable/object ...
                    # in an environment that is different from the current one, the setinverse-function.
                    
                    getinverse <- function() i
                    # This sub-function prints the inverse matrix if it has been calculated yet;
                    # Otherwise, the default NULL-inverse-matrix is printed and signals that the inverse matrix ...
                    # ... has still to be calculated by the cacheSolve function
                    
                    list(set = set,
                         get = get,
                         setinverse = setinverse,
                         getinverse = getinverse)
                    # This is the last object of the function body and therefore the output of the 1st function.
                    # A list with the function's sub-functions is printed.
                    # The sub-functions may be called with the help of the $ operator.
                    }


## Write a short comment describing this function
#1) Applied the equivalent from the Cache-Vector-Mean example.
#2) More specific comments are added to explain the function.

cacheSolve     <- function(x, ...) {
               # This function takes an object (e.g. x) as its input, to which the makeCacheMatrix function and ...
               # ... matrix was assigned to (x <- makeCacheMatrix('Input-Matrix'))
     
               
               i <- x$getinverse()          
               if(!is.null(i)) {
                    message("getting cached data")
                    return(i)
               }
               # This if-clause checks to see if the inverse matrix has already been calculated ...
               # ... in which case 'm' must be unequal to 'NULL';
               # if 'm' is not equal to 'NULL' and thus the inverse matrix has been calculated already ...
               # ... it `get`s the inverse from the cache and skips the computation.
               # A message explains that.
               # Otherwise and if not yet calculated, it ...
               
               data <- x$get()
               i <- solve(data, ...)
               # ... calculates the inverse of the input matrix (data) and...
               
               x$setinverse(i)
               i
               # ... returns a matrix that is the inverse of 'x'.
}

## Testing the functions with examples from the web
#A)https://www.youtube.com/watch?v=S4n-tQZnU6o
A <- matrix(c(1, 0, 1, 0, 2, 1, 1, 1, 1), 3, 3)
InvA <- matrix(c(-1, -1, 2, -1, 0, 1, 2, 1, -2), 3, 3)
x <- makeCacheMatrix(A)
x$get()
A
x$getinverse()
cacheSolve(x)
x$getinverse()
cacheSolve(x)
InvA
InvA == cacheSolve(x)
A %*% InvA == diag(3)

#B)https://www.youtube.com/watch?v=iUQR0enP7RQ
B <- matrix(c(3, 2, -4, -5), 2, 2)
InvB <- matrix(c((5/7), (2/7), (-4/7), (-3/7)), 2, 2)
x$set(B)
x$get()
B
x$getinverse()
cacheSolve(x)
x$getinverse()
cacheSolve(x)
InvB
InvB == cacheSolve(x)
B %*% InvB == diag(2)

#C)http://www.mathwords.com/i/inverse_of_a_matrix.htm
C <- matrix(c(4, 3, 3, 2), 2, 2)
InvC <- matrix(c(-2, 3, 3, -4), 2, 2)
x$set(C)
x$get()
C
x$getinverse()
cacheSolve(x)
x$getinverse()
cacheSolve(x)
InvC
InvC == cacheSolve(x)
C %*% InvC == diag(2)