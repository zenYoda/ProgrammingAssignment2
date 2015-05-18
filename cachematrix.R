# Creates a list of functions to analyse the matrix
# getMat: fetches the current matrix
# getInv: fetches the current inverse of the matrix
# setInv: assigns a new inverse for the matrix

makeCacheMatrix <- function(x = matrix()) {                         #create list of functions
    matInv<-NULL
    getMat<-function() x                                            #returns the matrix
    setInv<-function(inv) matInv<<-inv                              #assigns the inv of the matrix
    getInv<-function() matInv                                       #returns the inv of the matrix
    list(getMat=getMat,setInv=setInv,getInv=getInv)
}

# Checks whether inverse of the matrix has already been calculated
# If yes: avoid re-calculation & simply fetch the existing one
# If no: calculate and set it as the inverse for later use

cacheSolve<-function(x,...){                                        #caching function to lower inverse calculation redundancy
    matInv<-x$getInv()                                              #use to get inverse of existing matrix
    if(!is.null(matInv)){                                           #if inv is already calculated, just fetch it
        message("Fetching cached data...")
        return(matInv)
    }
    mat<-x$getMat()                                                 #else, calculate the inverse
    matInv<-solve(mat)                                               
    x$setInv(matInv)
    matInv
}
