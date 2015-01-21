## coding by iamxyz for the assignment2 in R programming class

## function makeCacheMatrix and cacheSolve are used to get the inverse of 
## a given matrix.By store the inverse matrix calculated before in the memory,
## the functions will output the inverse matrix directly without calculating
## if the given matrix keeps unchanged.So they will be helpful to reduce the 
## computing time espically in the processing of the big size matrix. 


## makeCacheMatrix will create two objects: a matrix and a inverse matrix, which will
## be stored out of the current environment, as well as four functions are provided 
## as set new matrix, get the old matrix, set the new inverse matrix and get the old
## inverse matrix. A list will be returned by makeCacheMatrix which will be used to
## access the four functions as described before.
##
## An example goes here:  
## > m1 <- matrix(1:4,2,2)
## > m1
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > k <- makeCacheMatrix(m1)

makeCacheMatrix <- function(x = matrix()) {

    im <- NULL
    set <- function(y){
        x <<- y
        im <<- NULL
    }
    get <- function() x
    set_im <- function(inverse_matrix) im <<- inverse_matrix
    get_im <- function() im
    list(set = set, get = get,
         set_im = set_im,
         get_im = get_im)
    
}


## cacheSolve is used with the makeCacheMatrix which will store the matrix in the
## memory. It will get the inverse of the given matrix, if the inverse matrix has
## not been calcluated, the cacaheSolve will make it as well as store it in the 
## memory; if the inverse matrix is exist , it will be send out without any computing.
##
## An example goes here (following with the first example) :

## > cacheSolve(k)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(k)
## getting cached inverse matrix
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
## > m2 <- matrix(5:8,2,2)
## > m2
##       [,1] [,2]
## [1,]    5    7
## [2,]    6    8
## > k$set(m2)
## > cacheSolve(k)
##       [,1] [,2]
## [1,]   -4  3.5
## [2,]    3 -2.5
## > cacheSolve(k)
## getting cached inverse matrix
##      [,1] [,2]
##[1,]   -4  3.5
##[2,]    3 -2.5


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    im <- x$get_im()
    if(!is.null(im)) {
        message("getting cached inverse matrix")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$set_im(im)
    im
    
}


 