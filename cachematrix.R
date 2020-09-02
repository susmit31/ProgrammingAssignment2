## makeCacheMatrix takes in a matrix as its input, and equips it with
## with the machinery to cache its inverse. cacheSolve takes in a matrix
## and calculates its inverse if it's not already in the cache. 
## If it's is already in the cache, retrieves it.

## Given a matrix, equipping it with "getter" and "setter" methods for 
## calculating and caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	get<-function() return(x)
	setInv<-function(invMat) m<<-invMat
	getInv<-function() m
	list(get=get,getInv=getInv,setInv=setInv)
}


## Get the inverse from the cache. If it has been calculated before, it will
## be a non-null value, and this will be returned. Otherwise,
## the inverse will be null, and it will be computed and stored in the cache
## memory, and then it will be returned.
cacheSolve <- function(x, ...) {
	m<-x$getInv()
	if(!is.null(m)) return(m)
	data<-x$get()
	m<-solve(data,...)
	x$setInv(m)
	return(m)
}

