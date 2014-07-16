makeVector <- function(x = numeric()) {

	##Creates a special "vector", which is really a list containing
	##a function to
	##1. set the value of the vector
	##2. get the value of the vector
	##3. set the value of the mean
	##4. get the value of the mean
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
cachemean <- function(x, ...) {
	##calculates the mean of the special vector created with above function
	##it first checks if the mean has already been calculated
	##if so it gets the mean from the cache and skips computation
	##otherwise it calculates the mean and sets the value of the mean in
	##the cache via the setmean function
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}