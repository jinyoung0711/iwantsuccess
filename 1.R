a <- 1:3
b <- rnorm(3)
c <- rep(TRUE,3)
d <- letters[1:3]
lst <- list(a,b,c,d) 
class(lst)
typeof(lst)
mode(lst)
sapply( lst, typeof )
sapply( lst, mode )
sapply( lst, class )

b = rnorm(3)
class(b)
mode(b)
typeof(b)


c = c(1L, 2L, 3L)
class(c)
mode(c)
typeof(c)


d = c(1:3)
class(d)
mode(d)
typeof(d)

fun <- function( x ){
  if( is.numeric(x) ) x.num <- x 
  else stop("x must be a numeric atomic vector!")
  res <- x.num^2
  return(res) 
}
fun(1:3)  

