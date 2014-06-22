## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse and
## then cacheSolve calculates the inverse of the  "matrix" returned by makeCacheMatrix.
## If the inverse matrix has already been calculated, then cacheSolve returns the message "getting
## cached data" and the inverse from the cache.

## makeCacheMatrix creates a list of functions:
## 1. a function to set the value of the matrix
## 2. a function to get the value of the matrix
## 3. a function to set the value of the inverse
## 4. a function to get the value of the inveerse

makeCacheMatrix <- function(x = matrix()) {
   #the variable that will contain the cached inverse matrix
   M<-NULL
   #set the value of the matrix
   set<-function(y){
     x<<-y
     M<-NULL
   }
   #get the value of the matrix
   get<-function()x
   #set the value of the inverse
   setinv<-function(inverse)M<<-inverse
   #get the value of the inverse
   getinv<-function()M
   #it returns the special "matrix" with our previously defined functions
   list(set=set, get=get,
        setinv=setinv,
        getinv=getinv)

}                                                         

## cacheSolve creates the inverse of the special "matrix", previously created with the function makeCacheMatrix.
## If the inverse has been already calculated, it returns the cached inverse matrix and skip the computation, otherwise 
## it computes, caches (with the setinv function) and returns it.

cacheSolve <- function(x, ...) {
    M<-x$getinv()
    if(! is.null(M)){
      message("getting cached data")
      return(M)
    }
    data<-x$get()
    M<-solve(data,...)
    x$setinv(M)
    M

}

##Some example with 2x2 matrix
a<-makeCacheMatrix(matrix(5:8,nrow=2))
a$get()
a$getinv()
a$set(matrix(1:4,nrow=2))
a$get()
cacheSolve(a)
cacheSolve(a) ##The message "getting cached data" is returned and the already computed inverse is retrived
a$getinv() #the inverse matrix
b<-a$getinv()
b%*%a$get()# the product of the matrix and its inverse is the identity matrix, as required. 