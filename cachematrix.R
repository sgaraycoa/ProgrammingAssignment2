##These functions cache the inverse of a matrix (assuming it is invertible).

##This function gets & sets the matrix and then gets & sets the inverse.
makeCacheMatrix<-function(x=matrix()){
        m<-NULL
        set<-function(y) {
                x<<- y
                m<<-NULL
        }
        get<- function() x
        setinv<- function(solveit) m<<-solveit
        getinv<- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function inverts the matrix if it has not already been solved; either way it outputs the inverse.        
cacheSolve<-function(x,...)  {
        m<- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data<- x$get()
        m<- solve(data,...)
        x$setinv(m)
        m
}




######
