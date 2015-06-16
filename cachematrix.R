## Time-consuming computations can be speeded up by cacheing the result if the content of e.g. vector or matrix has not changed. 
## So, instead of repeating the computation, the program will look up the cached value of the computation (which is stored in
## a different environment than the current environment).  


## This function makes a 'special' matrix object that can cache its inverse; a square invertible matrix.
## It creates a list containing a function to set value of matrix, get the matrix, set the inverse and get the inverse of the matrix.
## List is used as input in next function 'cacheSolve' 

makeCacheMatrix <- function(x = matrix()) {
         m<-NULL
         set<-function(y){
               x<<-y                          
               m<<-NULL                       
         }
         get<-function()x
         setmatrix<-function(solve)m <<- solve ##
         getmatrix<-function()m
         list(set=set,get=get,                  
             setmatrix=setmatrix,
             getmatrix=getmatrix)              
}


## The following function calculates and returns the inverse of the matrix created in the function above. 
## But, first it checks whether if the inverse of the matrix already has been calculated. 
## If this is the case,it gets it from cache and skips calculation; otherwise it computes the inverse and 
## saved it in cache via set matrix function. 

cacheSolve<-function(x=matrix,...){          
            m<-x$getmatrix()                 
            if(!is.null(m)){                 
            message("getting cached data")
            return(m)
       }
       matrix<-x$get()                       
       m<-solve(matrix,...)
       x$setmatrix(m)                        
       m
}




