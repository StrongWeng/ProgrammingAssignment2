#  Lexical scoping help us make use of other functions we ccreated
## ,just like part 3 of Assignment 1,we fetched the result of part
## 2 to save repeating same functions and save time of computing.
## However, The function here is powerful than assignment 1 in that
## if it doesn't find answer, it will compute and store it for 
## furture uses.
 
## I first writed a function called makeCacheMatrix,and then writed
## second function to fetch the result.If it doesn't find the
## answer we ask, it computes from scratch and stores the results.

#  Function 1: This function is to create a matrix, which is able
## to store the inverse matrix to other place.

makeCacheMatrix <- function(x = matrix()) {
    Imatrix=NULL ### initialize the inverse of a matrix as null.
### Just like second function's cache order,it first find the 
### inverse matrix here. 
    fetchImatrix=function(){Imatrix}
### If it doesn't find it, then it asks for this matrix to compute.
    fetchmatrix=function(){x}
### after computing out, new Imatrix is assigned by superassignment
    setImatrix=function(newImatrix){Imatrix<<-newImatrix}
### Save the result to other place(i.e pool here) and reset Imtrix
### to null
    setmatrix=function(){x<<-pool;Imatrix<<-NULL}
list(setmatrix=setmatrix,fetchmatrix=fetchmatrix
     ,setImatrix=setImatrix,fetchImatrix=fetchImatrix)
}

#  Function 2:this function fetch the result of the first function,
## if it is null, it fetches the original matrix,solve it and 
## store it back first function.
cacheSolve <- function(x, ...) {
### first subset fetchImatrix from first function to cache the
### inverse of a matrix.    
    Imatrix=x$fetchImatrix()
### If we have had it,return it and tell me it had been computed.
    if(!is.null(Imatrix)){message("I have had it!!")
    return(Imatrix)}
### else,get and assign the new matrix,then calculate its inverse 
### matrix
    new=x$fetchmatrix()
    Imatrix=solve(new,...)
### stored it back to first function.    
    x$setImatrix(Imatrix)
## return the inverse matrix newly calcuated.   
    Imatrix
}