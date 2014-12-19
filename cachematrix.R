## The two functions in this file demonstrate the use of lexical scoping and caching features in R
## which save us a lot of re-computation time for complex tasks such as Matrix Inversion

## This function creates a vector that can store the inverse of matrix in cache

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(inverse) m<<- inverse
    getmatrix<-function() m
    list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## This function returns the inverse of matrix from cache if it's already computed else it computes the inverse of matrix 
## and stores in cache for further re-use

cacheSolve <- function(x, ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("Getting matrix from cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
