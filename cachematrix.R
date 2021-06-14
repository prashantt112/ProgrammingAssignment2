makeCacheMatrix <- function(x = matrix()) {
      inv=NULL           ##Initialising inverse as null
      set=function(y){
        x=y
        inv=NULL
      }
      get=function()      ##function to get matrix x
        {
       x 
      }
      setInverse = function(inverse)
      {inv=inverse}
      getInverse=function()    ##Function to obtain inverse of the matrix
      {inv}
      list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}
cacheSolve <- function(x, ...) ##gets cache data
  {
        inv=x$getInverse()   
        if(!is.null(inv))      ##cheching whether inverse is null 
          {
          message("getting cached data")
       return(inv)             ##returns inverse value
        }
        mat=x$get()
        inv=solve(mat,....)    ##calculates inverse value
        x$setInverse(inv)
        inv                 ##return a matrix is the inverse of 'x'
}
