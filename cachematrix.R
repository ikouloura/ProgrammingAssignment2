makeCacheMatrix <- function(x = matrix()) {
m<- NULL
	set<- function (y){     ##set the value of matrix
		x<<-y
		m<<-NULL
	}
	get <- function ()x      ##get the value of matrix
	setinvmatr <- function (solve) m<<- solve     ##set the inverse matrix
	getinvmatr <- function ()m    ##get the inverse matrix
	list(set = set, get = get,    ##return a list of functions
             setinvmatr = setinvmatr,
             getinvmatr = getinvmatr)
}

cacheSolve <- function(x, ...) {##calculate inverse matrix
	m<- x$getinvmatr()    ##get the inverse data
	if(!is.null(m)){      ##check cached data
		message("getting cached data")
		return(m)
	}
	else{
		message("no cached data")
		matrix <- x$get()    ##get matrix
		m <- solve (crossprod(matrix))   ##calculate inverse matrix
		x$setinvmatr(m)     ## set inverse matrix
		m
	}
}

