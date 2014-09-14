# There are two functions to create matrixes and cache their
# inverse matrixes separtately.

## Function 1: This function is to create a matrix, which is able
## to store the inverse matrix .

makeCacheMatrix <- function(x = matrix()) {
    Imatrix=NULL
    setmatrix=function(){x<<-pool;Imatrix<<-NULL}
    fetchmatrix=function(){x}
    setImatrix=function(solve){Imatrix<<-solve}
    fetchImatrix=function(){Imatrix}
list(setmatrix=setmatrix,fetchmatrix=fetchmatrix
     ,setImatrix=setImatrix,fetchImatrix=fetchImatrix)
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
### first cache the inverse of a given matrix.    
    Imatrix=x$fetchImatrix()
### If we have had it,return it and tell me it is not a new matrix.
    if(!is.null(Imatrix)){message("I had it!!")
    return(Imatrix)}
### else,get the new matrix,calculate its inverse matrix and
### stored it.return the inverse matrix newly calcuated.
    new=x$fetchmatrix()
    Imatrix=solve(new,...)
    x$setImatrix(Imatrix)
    Imatrix
}

