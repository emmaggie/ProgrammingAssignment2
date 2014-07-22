## This is a suite of two functions (makeCacheMatrix & cacheSolve), which 
## facilitate caching matrix and its computed inverse for future reference.
## USAGE: makeCacheMatrix(x), cacheSolve(x1,...).
## x - matrix to store and have its inverse computed and stored
## x1 - object returned by makeCacheMatrix()


## makeCacheMatrix() returns a list of four functions: 
## (set(),get(),set_m_inv(),get_m_inv()). It helps to store and retrieve 
## a matrix (set(),get()) and its inverse (set_m_inv(),get_m_inv()).

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set<-function(y){
                  x <<-y
                  m_inv <<-NULL
        }
        get<-function() x
        
        set_mat_inv<-function(mat_inv) m_inv <<- mat_inv
        get_mat_inv<-function() m_inv
        
        list(set=set,get=get,set_mat_inv=set_mat_inv,get_mat_inv=get_mat_inv)
}

## cacheSolve() takes object x of a class list created with a function .
## makeCacheMatrix(). It returns inverse of a matrix passed to makeCacheMatrix().
## The cached inverse is returned if it exists. Otherwise, inverse is computed and 
## cached with makeCacheMatrix() for future reference. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <-x$get_mat_inv()
        if(!is.null(m_inv)){
            message('returning cached inverse matrix')
            return(m_inv)
        }
        else{
            mat<-x$get()
            m_inv<-solve(mat, ...)
            x$set_mat_inv(m_inv)
            return(m_inv)
        }
}


## RESOURCES:
## More on matrix operations in R:
## http://www.statmethods.net/advstats/matrix.html
## Check out the matlab emulator!