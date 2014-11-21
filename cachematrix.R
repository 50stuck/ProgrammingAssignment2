## Put comments here that give an overall description of what your
## functions do
# These functions create a "matrix" object, compute its inverse matrix,
# and cache it. If the inverse matrix was allready cached, these functions will
# not re-compute it, but rather retrieve the inverse matrix from the cache.
# The functions also enable changing an object (instead of creating a new one),
# and re-computing the inverse matrix (even if it was calculated before the
# change)

## Write a short comment describing this function
# The "makeCacheMatrix" function includes four methods that are attributed to
# an input matrix: "set", which enables changing the object (instead of having to
# create a new one); "get", which returns the input matrix; "setIM" which caches
# the inverted matrix (calculated by cacheSolve) to the variable IM; and "getIM"
# which returns the cached inverted matrix.

makeCacheMatrix <- function(x = matrix()) { #the function takes in a matrix
        IM <- NULL #sets IM as null when first running
        set <- function(newmatrix) { #a method that allows you to change the matrix
                                     #without having to creating a new object.
                x <<- newmatrix #saves the new matrix into x 
                IM <<- NULL #"resets" the IM variable to NULL in case "set" method was used
        }
        get <- function() x #a method that returns the input matrix. later used
                            #by cacheSolve to create the inverted matrix.
        cacheIM <- function(IM) IM <<- IM #a method that caches the new calculated
                                        #inverted matrix (when called by cacheSolve)
                                        #to the IM variable
        getIM <- function() IM #a method that returns the IM variable. later used
                               #by cacheSolve to check if IM is NULL or not
        list(set = set,
             get = get, #lists the methods in the function to enable
             set = set, #cacheSolve to later on use them.
             cacheIM = cacheIM,
             getIM = getIM)
}

## Write a short comment describing this function
# The "cacheSolve" function takes an object that was created by makeCacheMatrix
# and checks to see if it's inverted matrix was already computed. If so - the
# function gets the data from cach and indicate it has done so. If not (or if
# the original matrix was changed using the "set" method) the function computes
# the input matrix's inverted matrix, stores it to cache, and returns it.

cacheSolve <- function(obj, ...) { #The function takes in an object that was
                                   #created by makeCacheMatrix
        IM <- obj$getIM() #Uses the "getIM" method to store the inverted matrix,
                          #if was already computed, to the variable "IM".
        if(!is.null(IM)) { #Checks if IM is not null (meaning: the inverted matrix)
                           #was already cached.
                message("getting cached data") #Indicates this to the console
                return(IM) #Returns the cached value 
        }
        #If the inverted matrix was not cached:
        data <- obj$get() #uses the "get" method to take the input matrix
        IM <- solve(data, ...) #computes the inverted matrix and sets it to IM
        obj$cacheIM(IM) #uses the "setIM" method to cache inverted matrix to IM
        IM #returns the inverted matrix
}
