# 동전던지기 결과 예측

guessfun <- function( x ){
  if( !is.character(x) || length(x) > 1) stop("x must be a character H or T!")
  res <- sample( x = c("H","T"), size = 1 ) 
  if( x == res) print("You win!")
  else print("You lose!")
}
#guessfun(1) #Error in guessfun(1) : x must be a character H or T!
#guessfun(c("H","T")) #Error in guessfun(c("H", "T")) : x must be a character H or T!
#guessfun(H) #Error in guessfun(H) : object ’H’ not found
guessfun("H")

# if-else() -> switch()

centre1 <- function(x, type){
  if(type == "mean") mean(x)
  else if(type == "median") median(x)
  else if(type == "trimmed") mean(x, trim = 0.1)
}
x <- rcauchy(10)
centre1(x, type = "mean")

centre2 <- function(x, type) {
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}
centre2(x, "mean")

guessfun <- function( x ){
  res <- sample( x = c("H","T"), size = 1)
  if( !is.character(x) ) stop( "x must be a character H or T!") 
  if( length(x) > 1 ) stop( "x must be a character H or T!") 
  if( is.character(x) & x == res ) print("You win!")
  else print("You lost!")
  ress <- is.character(x) & x == res
  resss <- list(res, ress)
  structure( resss, class = "fun" ) #create an class ‘‘fun’’ 
  #or attr( ress, "class") <- "fun"
}  
toss <- guessfun( "H" )

myfun <- function( x ){
  UseMethod("myfun")
}

methods( generic.function = "myfun" )

myfun.default <- function( x ){
  res <- round(c( mean = mean(x), var = var(x) ), 2)
  return(res)
}
myfun.matrix <- function( x ){
  res <- list( mean = apply( x, 2, mean ),
               var = apply( x, 2, var ) )
  return(res)
}
myfun.factor <- function( x ){
  res <- table( x )
  return(res)
}


fun1 <- function(x){
  y <- 1 + x
  return(y)
}
fun2 <- function(x){
  y <<- 1 + x
  return(y)
}
y <- 1
fun1(1)
y
fun2(1)
y

A <- 1:20
dim(A) <- c(5,4)

A
class(A)
mode(A)
typeof(A)

xx <- yy <- 1:10
cols <- 1:10 
plot(x = xx, y = yy, xlab ="xlab", ylab = "ylab", main = "", 
     xlim = c(0,11), ylim = c(0,11), type = "n" )
points(x = xx, y = yy, pch = 16, col = cols, cex = 1.2 )
text(x = xx, y = yy + 0.5, labels = as.character(cols))
title( "Here you can write title!", col.main = 4)
abline( v = mean(xx), col = "grey50", lty = 2)
abline( h = mean(yy), col = "grey30", lty = 3) 
box( col = 6 )

par( mar = c(0,5,3,0) )
xx <- factor(mtcars$cyl)
n.lev <- length(levels(xx))
boxplot( mpg ~ cyl, data = mtcars, col = c(3,4,6), border = c(3,4,6),
         pars = list(boxwex = 0.3), xaxt = "n", xlab = "", ylab = "")
axis(side = 4, at = 1:n.lev, labels = paste("cyl",levels(xx), sep = "="), 
     col.axis = 4, las = 1)

# lapply()를 R로 직접 구현
lapply.my <- function(X, FUN, ...) {
  res <- vector( "list", length(X) )
  for (i in seq_along(X) ) {
    res[[i]] <- FUN(x[[i]], ...)
  }
  return( res )
}
lapply.my(x, mean)
Reduce(`+`, 1:4)

# Reduce()를 R로 직접 구현
Reduce.my <- function( f, x ) {
  res <- x[[1]]
  for(i in seq(2, length(x))) {
    res <- f(res, x[[i]])
  }
  res
}
Reduce.my( sum, 1:3 )

A <- matrix(1:20, nrow = 5)
apply(A, 1, mean)
apply(A, 2, mean)

# tapply() R로 직접 구현
tapply.my <- function(x, group, f, ..., simplify = TRUE) {
  pieces <- split(x, group)
  sapply(pieces, f, simplify = simplify)
}  

aggregate(mtcars, list(cyl = mtcars$cyl), mean )

aggregate(mtcars, list(cyl = mtcars$cyl, hp = mtcars$hp >= 120 ), mean )

aggregate( mpg ~ cyl, data = mtcars, mean)
aggregate( mpg ~ ., data = mtcars, mean)

aggregate( mpg ~ cyl + (mtcars$hp >= 120), data = mtcars, mean)

aggregate( cbind(mpg, drat, wt) ~ cyl, data = mtcars, mean)


a <- 1:3 
class(a)
mode(a)
typeof(a)
b <- rnorm(3)
class(b)
mode(b)
typeof(b)
c <- rep(TRUE,3)
class(c)
mode(c)
typeof(c)
d <- letters[1:3]
class(d)
mode(d)
typeof(d)

typeof( c(d, b) )

typeof( c(b, a) )

typeof( c(a, c) )

typeof( c(a, b, c, d) )

a <- 1:3
b <- rnorm(3)
c <- rep(TRUE,3)
d <- letters[1:3]
lst <- list(a,b,c,d) 

sum( lst[3] )
sum( lst[[3]] )

dats <- list(
  dat1 = factor( c("a", 1, T) ),
  dat2 = unclass( factor( c("a", 1, T) ) ),
  dat3 = data.frame(  x = 1:3, y = letters[1:3] ),
  dat4 = unclass( data.frame(  x = 1:3, y = letters[1:3] ) )
)

funs <- list(
  fun1 = is.vector,
  fun2 = typeof
)

sapply( dats, function(y) sapply( funs, function(f, x) f(x), y ) )

fun <- function(x, type){
  if(type == "mean") mean(x)
  else if(type == "median") median(x)
  else if(type == "trimmed") mean(x, trim = 0.1)
}

fun <- function(x, type){
  switch( type, 
         mean = mean(x),
         median = median(x),  
         trimmed = mean(x, trim = 0.1)
  )
}

x <- c(1,2,3,4,5)
fun(1:5, "mean")

fun1(1:5, "mean")

x <- 1:3
for(i in seq_along(x) ) print(i)

i = 1
while(i < 4) {
  print(i)
  i = i + 1
}

i = 1
repeat{
  print(i)
 
  if( i == 3) break;
  i = i + 1
  }

dat <- list( a = 1:3, b = "c", c = c(F,T) )
class(dat[3])
class(dat[[3]])

fun <- function(x){ UseMethod("fun") }
fun.default <- function(x) sum(x)
fun.matrix <- function(x) apply(x, 2, sum)
fun.factor <- function(x) table(x)/sum(table(x))

fun(factor(c("M", "F", "F", "M")))

fun(matrix(c(1,2,3,1,2,3), nrow = 3, ncol = 2))

is_vector <- function(x) is.null(dim(x))
x <- 1:4
y <- list(T, 1:4, letters[1:10])
z <- matrix(1:4,2,2)
is_vector(x); is_vector(y); is_vector(z); is_vector(mtcars)

indx <- 1:4
x <- list(a = indx, b = LETTERS[indx])
str(x)
x
y <- as.data.frame(x)
y[] <- x
z <- x

sapply( list(x,y,z), FUN = class)

a <- 1:4
b <- letters[1:4]

ls <- list(a = a, b = b)

mode(unlist(ls))

x <- letters[1:4]
f1 <- factor(x)
f2 <- f1
levels(f2) <- rev(levels(f2))
f3 <- rev(factor(x))
f4 <- factor(x, levels = rev(x))     
     
#답:
fun_upper <- function(x, y) panel.smooth(x, y, lwd = 2, col = "grey50", col.smooth = 2)
fun_lower <- function(x, y) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  corcoef <- round(cor(x, y),2)
  ramp <- colorRamp( c("grey60", "red") )  
  col <- rgb( ramp( abs(corcoef) ), max = 255 )
  cex <- 2 * abs(corcoef) + 1
  text( 0.5, 0.5, paste0("Cor. = \n", corcoef), col = col, cex = cex )
}
fun_diag <- function(x) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = F)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, 
       col = "lightblue", border = "blue")
}
fun_text <- function(x = 0.5, y = 0.5, txt, cex, font, col = "darkblue") 
  text(x, y, txt, cex = cex, font = font, col = col)
pairs( mtcars[c("mpg", "hp", "wt", "qsec")], 
       upper.panel = fun_upper, 
       lower.panel = fun_lower, 
       diag.panel = fun_diag, 
       text.panel = fun_text )

fun <- function(x, y, ... ){
  
  xhist <- hist(x, plot = FALSE)
  yhist <- hist(y, plot = FALSE)
  top <- max(c(xhist$counts, yhist$counts))
  xrange <- range(x)
  yrange <- range(y)
  nf <- layout(matrix(c(2,0,1,3),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)
  
  opar <- par(mar = c(5,5,1,1))
  on.exit( par(opar))
  plot(x, y, xlim = xrange, ylim = yrange, ... )
  box()
  par(mar = c(0,5,1,1))
  barplot(xhist$counts, axes = F, ylim = c(0, top), space = 0, 
          density = 20, col = 4, 
          border = 4, horiz = FALSE, 
          ylab = "Frequency" ) 
  axis( side = 2, col = 4, col.axis = 4 )
  par(mar = c(5,0,1,1))
  barplot(yhist$counts, axes = F, xlim = c(0, top), space = 0, 
          density = 20, col = 2, 
          border = 2, horiz = TRUE, 
          xlab = "Frequency" )    
  axis( side = 1, col = 2, col.axis = 2 )
}
fun( x = mtcars$wt, y = mtcars$hp, cex = 1, col = "grey50", pch = 16,
     xlab = "Weight (1000 lbs)", ylab = "Gross horsepower" )

par( mar = c(5,5,0,0) )    
x <- mtcars$mpg
boxplot( mpg ~ cyl, data = mtcars, 
         col = c(4, 2, 3), 
         border = c("blue", "red", "green"), 
         xlab = "Miles/(US) gallon", 
         ylab = "Number of cylinders",
         las = 1, 
         pars = list(boxwex = 0.3),
         outline = T
)


y <- mtcars$mpg    
x <- mtcars$cyl
mm <- length( unique(x) )

plot( y ~ x, xlim = c(0, mm) + 0.5, type = "n", xaxt = "n", xlab = "Miles/(US) gallon", ylab = "Number of cylinders" )

draw_segment <- function( x, y, col, border, boxwex = 1 ){
  
  pts <- fivenum(y)
  cuts <- pts[3] + c(-1,1) * IQR(y) * 2.5
  ind <- y < cuts[1] | y > cuts[2]
  
  pts <- fivenum(y[!ind])
  polygon( x = c( rep(x, 2) - (boxwex/2), rep(x, 2) + (boxwex/2) ), y = c(pts[c(2,4)], pts[c(4,2)]), col = col, border = border )
  segments( x0 = (x - (boxwex/2)*0.5), y0 = pts[c(1,5)], 
            x1 = (x + (boxwex/2)*0.5), y1 = pts[c(1,5)], 
            lwd = 1, col = border )
  segments( x0 = (x - (boxwex/2)) + 0.01, y0 = pts[3], 
            x1 = (x + (boxwex/2)) - 0.01, y1 = pts[3], 
            lwd = 3, col = border )
  segments( x0 = x + c(-1,1)*(boxwex/2), y0 = pts[2], 
            x1 = x + c(-1,1)*(boxwex/2), y1 = pts[4], col = border )
  segments( x0 = x, y0 = pts[1], 
            x1 = x, y1 = pts[2], lty = 2, col = border )
  segments( x0 = x, y0 = pts[4], 
            x1 = x, y1 = pts[5], lty = 2, col = border )
  
  points( rep(x, sum(ind)), y[ind], col = border )
}

xloc <- seq_along( 1:mm )
xlabels <- sort( unique(x) )
ysplit <- split( y, x )

cols <- palette()[c(4, 2, 3)]
borders <- c("blue", "red", "green")

opar <- par( mar = c(5,5,0,0) )
plot( y ~ x, xlim = c(0, mm) + 0.5, type = "n", xaxt = "n", xlab = "Miles/(US) gallon", ylab = "Number of cylinders", las = 1 )
lapply( 1:mm, function(i) draw_segment( x = xloc[i], y = ysplit[[i]], col = cols[i], border = borders[i], boxwex = 0.3 ) )
#위의 lapply() 함수는 아래의 for 구문과 동일함
#for( i in 1:mm ) draw_segment( x = xloc[i], y = ysplit[[i]], col = cols[i], border = borders[i], boxwex = 0.3 )

axis( side = 1, at =  xloc, labels = xlabels )
par( opar )

x <- 4
fun <- function( x )
{
  function( z )
  {
    res <- z + x^2
    return(res)
  }
}
z <- 5
x <- 2

typeof( fun( 2 ) )

fun(2)

funn <- fun( 2 )
funn( 10 )

x <- 6
funn( 10 )
environment(funn)
as.list( environment(funn) )

set.seed(12345)
dat <- replicate(10, sample( c(1:10, 999), 15, replace = T), simplify = FALSE)
dat

set.seed(12345)
dat <- replicate(10, sample( c(1:10, 999), 15, replace = T), simplify = FALSE)
dat

fun <- function( x ){
  x[ x == 999 ] <- NA
  return(x)
}

lapply( dat, fun )
