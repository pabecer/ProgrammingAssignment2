## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Esta función crea matriz especial que puede:
#       1. Setear el valor de la matriz
#       2. Recuperar el valor de la matriz
#       3. Setear el valor de la matriz inversa
#       4. Recuperar el valor de la matriz inversa
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inversa) inv <<- inversa
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Esta funcion calcula la matriz inversa de la matris "especial" creada
## por makeCacheMatrix. Si la inversa ya fue calculada y la matriz no ha cambiado
## entonces deberia entregar la matriz inversa guardada en el cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("Obteniendo matriz inversa desde el cache")
                return(inv)
        }
        matriz <- x$get()
        inv <- solve(matriz)
        x$setinv(inv)
        inv

}



