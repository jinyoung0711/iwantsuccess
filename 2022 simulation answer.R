# 1번
dad <- c(164, 171, 176, 165, 178, 174, 172, 166)
son <- c(171, 175, 174, 170, 185, 182, 165, 176)
dat <- data.frame(dad,son)

cor(dat[,1],dat[,2]) #아버지와 아들 키의 표본상관계수
B <- 400

# 아버지와 아들의 키를 같은 쌍으로 뽑기 위해 index를 random으로 rep = T 복원 추출!!
f.cor <- function(x) {
  ind <- sample(1:8,8,rep=T)
  cor(x[,1][ind],x[,2][ind])
}
b.cor <- replicate(B, f.cor(dat))

#표준오차 방법1
# sqrt(sum((boot-mean(boot))^2)/(B-1))
sd(b.cor)

# 2번
m = 5; p = 0.2; B = 1000;
X = numeric(B)
function_x <- function() {
  for (i in 1:1000){
    if (x == 0) X[i] = 0.2
    else X[i] = (1 - 0.2) / 1-exp(-5) * rpois(x, lambda = 5)
  }
}
function_x()

for (i in 1:1000) {
  x[i] <- function(i)
}

for (i in 1:1000){
  
  repeat {
    function(i)
  if (u2 < (256/27) * u1 * (1 - u1)^3) { x[i] = u1; break}
  }
}


simul <- function() {
  n <- 1
  u <- numeric()
  repeat {
    u <- c(u,runif(1))
    if(cumprod(u)[n]<0.1) break
    n=n+1
  }
  n
}
\
B = 1000
replicate(B, function_x())

x = numeric(n)
m = 5; p = 0.2; B = 1000;
x1 = 0.2; x2 = (1 - 0.2) / rpois(1, lambda = 5)
x = ifelse(runif(n) < alp, x1, x2)
x
?rpois

# 3-1번

n = 25
Yij_1 = numeric(n); Yij_2 = numeric(n)
T1 = 1; T2 = 0
for (i in 1:25){
  Ui = rnorm(1, mean = mu_u, sd = sig)  
  error_1 = rnorm(1, mean = 0, sd = 1)  
  error_2 = rnorm(1, mean = 0, sd = 1)  
  for (J in 1:2) {
    if ( J == 1)  Yij_1[i] = alp * T1 + Ui + error_1
    if ( J == 2)  Yij_2[i] = alp * T2 + Ui + error_2
  }
} 
Y <- rbind(Yij_1,Yij_2)
Y

# 3-2번
mean(Yij_1); mean(Yij_2)
var(Yij_1); var(Yij_2)
LL = ( mean(Yij_1) - mean(Yij_2) ) - 2 * sqrt(var((Yij_1) + var(Yij_2)) / n)
UL = ( mean(Yij_1) - mean(Yij_2) ) + 2 * sqrt(var((Yij_1) + var(Yij_2)) / n)
CI = c(LL, UL)
CI


# 3-3번
Di = Yij_1 - Yij_2
mean(Di); var(Di)
