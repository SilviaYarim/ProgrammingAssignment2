
##This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x=matrix()){
        ## x:square invertible matrix
        
        inv = NULL
        set = function(y){
                x <<- y
                inv <<- NULL
        }
        get = function()x
        setinv = function(inverse) inv <<- inverse
        getinv = function() inv
        
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        
}

##This function computes the inverse of the special matrix returned by makeCacheMatrix.
cacheSolve <- function(x,...){
        ## x: output of makeCacheMatrix()
        ## return the inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()
        
        #if the inverse has already been calculated
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat.inv = x$get()
        inv = solve(mat.inv,...)
        
        x$setinv(inv)
        return(inv)
        
}
    
spematrix<- makeCacheMatrix(matrix(1:4,2,2))
> spematrix$getinv()
NULL
> cacheSolve(spematrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(spematrix)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
                
                
