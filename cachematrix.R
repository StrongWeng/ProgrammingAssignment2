#  Lexical scoping help us make use of other functions we created
## ,just like part 3 of Assignment 1,we fetched the result of part
## 2 to save us from repeating same functions and save time of 
## computing. Anyway, The function here doesn't only fetch but also
## complements each other.If it doesn't find answer, it will 
## compute and store it for furture uses.
 
## The function we are going to construct contains two aspects.
## first function called makeCacheMatrix is to make a matrix and
## store the inverse of a matrix,and second function is to fetch
## the result.If it doesn't find the result we ask, it solves,
## prints the result to console and stores the results back to 
## first function.

#  Function 1: This function is to create a matrix, which is able
## to store the inverse matrix to other place.

makeCacheMatrix=function(x = matrix()) {
### initialize the inverse of a matrix as null
### for setImatrix(newImatrix) to store in.
    Imatrix=NULL 
### set the values of the input matrixes
    setmatrix=function(pool){x<<-pool;Imatrix<<-NULL}
### Based on cacheSolve's cache order,it needs to check if 
### the inverse matrix exists.But, need to read in Imatrix prior
### to check it.
    fetchImatrix=function(){Imatrix}
### If Imatrix is null, then cacheSovle function asks for fetching
### the original matrix.
    fetchmatrix=function(){x}
### after solving the new matrix, 
### new Imatrix is assigned by superassignment
    setImatrix=function(newImatrix){Imatrix<<-newImatrix}
### list accessable functions    
    list(setmatrix=setmatrix,fetchmatrix=fetchmatrix
     ,setImatrix=setImatrix,fetchImatrix=fetchImatrix)
}

#  Function 2:this function fetch the result from the 
### makeCacheMatrix function at the first satrt,
### if Imatrix is null, it fetches the original matrix,solves it and 
### stores it back makeCacheMatrix function.
cacheSolve <- function(x, ...) {
### first subset fetchImatrix from first function to cache the
### inverse of a matrix.    
    Imatrix=x$fetchImatrix()
### If we have had it,return it and remind that it had been computed.
if(!is.null(Imatrix)){message("getting cached inverse!!")
    return(Imatrix)}
### else,get and solve the new matrix.
    new=x$fetchmatrix()
    Imatrix=solve(new,...)
### stored it back to first function.    
    x$setImatrix(Imatrix)
## return the inverse matrix newly computed.   
    Imatrix
}