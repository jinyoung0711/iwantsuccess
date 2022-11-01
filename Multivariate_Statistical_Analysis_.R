# 2.25
S = matrix(c(25, -2, 4, -2, 4, 1, 4, 1, 9), nrow = 3, byrow = TRUE) # 행 순서로 입력하고 싶으면 byrow = T (False가 default)
S
cov(S)
det(cov(S))

# (a) 상관행렬과 표준편차행렬 구하라 ~
Vhalf = diag(c(5, 2, 3)) # 대각행렬 만들 수 있음 diag()
Vhalf.inv = solve(Vhalf) # 역행렬 만들 수 있음 solve()

(Rho = Vhalf.inv %*% S %*% Vhalf.inv) # %*% 행렬의 곱 대칭행렬이고 피어슨 상관계수를 의미

# (b)
(Vhalf %*% Rho %*% Vhalf)
S

# 2.38
A = matrix(c(13, -4, 2, -4, 13, -2, 2, -2, 10), nrow=3, byrow=T)
A
eigen(A) #$values 18  9  9
eigen(A)$values[1] #maximum of x'Ax/x'x
eigen(A)$values[3] #min

# 2.4
# A'-1 * A-1' = I  증명
# (AB)-1 * B-1A-1 = I 증명

# 2장 다 암기하고 있으세여... 그만큼 충요합니다....

# 2.5 
# QQ' = I 증명

# 2.6 B 고유값이 양수인지 보여주기

# 2.7 손으로도 할 수 있어야한대... 거짓말 ... 나쁜사람 ... 나빳어....
# 역행렬의 EIGENVALUES는 변하지 않고, EIGENVALUES의 역수가 된다.

( A = matrix ( c(9, -2, -2, 6), nrow = 2) )

# EXERCISE 2.7-(a)
eigen(A)

# EXERCISE 2.7-(b)
eigen(A)$vectors %*% diag(eigen(A)$values) %*% t(eigen(A)$vectors)

# EXERCISE 2.7-(c)
solve(A)

# EXERCISE 2.7-(d)
eigen (solve (A))

# EXERCISE 2.16
( A = matrix ( c(9, -2, -2, 6, 3, 4), nrow = 2) ) # n = 3, p = 2
( A.tran = t(A) )

# EXERCISE 2.24

( A <- diag(c(4, 9, 1)) )

# EXERCISE 2.24 (a)
solve(A)

# EXERCISE 2.24 (b)
eigen(A)$values
eigen(A)$vectors

# EXERCISE 2.24 (c)
eigen( solve(A) )

# 타원 중간고사에 많이 내는 문제 반대로 회전하는 문제도 생각

# EXERCISE 3.1
( X = matrix( c (9, 5, 1, 1, 3, 2), ncol = 2) )

# EXERCISE 3.1 - (a)
plot(X)
colMeans(X)
points(5,2, col = 'red')

# EXERCISE 3.1 - (b)
y1 = X[,1]; y2 = X[,2]
y1 <- matrix(c(9,5,1) , nrow = 3)
str(y1)

y1[1] - 5 %*% 1
y1[2] - 5 %*% 1
y1[3] - 5 %*% 1

y2 - 2 %*% 1
y2[1] - 2 %*% 1
y2[2] - 2 %*% 1
y2[3] - 2 %*% 1


########################################

# EXERCISE 3.6
X <- matrix(c(-1, 2, 5, 3, 4, 2, -2, 2, 3), ncol = 3)
X

# EXERCISE 3.6 - (a)
colMeans(X)
X[,1] - c(1) %*% t(colMeans(X))

########################################

# EXERCISE 3.14
( X = matrix( c (9, 5, 1, 1, 3, 2), ncol = 2) )
b = c(2,3); c = c(-1, 2)
b # 행벡터 같아보이지만, 열벡터 t(b) 해보면 알 수 있음
c

# EXERCISE 3.14 - (a)
vec1 <- X %*% b # 원하는 관측값 3개 구함
mean(vec1)
var(vec1)

vec2 <- X %*% c
mean(vec2)
var(vec2)

cov(vec1, vec2)

# EXERCISE 3.14 - (b)
t(b) %*% colMeans(X);
t(b) %*% var(X) %*% b # var(X)는 S

t(c) %*% colMeans(X);
t(c) %*% var(X) %*% c

t(b) %*% var(X) %*% c


# EXERCISE 4.29 
getwd()
setwd('C:/Users/82106/22_다변량분석_데이터')
T1.5 = read.table("T1-5.dat")
T1.9 = read.table("T1-9.dat")
str(T1.5)
colnames(T1.5) = c('wind', 'solar', 'CO', 'NO', 'NO2', 'O3')

T1.5

# Test for Normality
Table4.1 <- read.table("T4-1.DAT")
Radiation <- Table4.1[,1]
obj <- qqnorm(Radiation)
qqline(Radiation, col = 2)

cor(obj$x, obj$y)
shapiro.test(Radiation)

# Chi-square QQ plot
Table4.3 <- read.table("T4-3.DAT") # Example 4.14
names(Table4.3) <- c('x1', 'x2', 'x3', 'x4', 'dsq') 
n <- nrow(Table4.3)
x <- qchisq( (1:n - 0.5)/n, df = 4)
qqplot(x, Table4.3$dsq)

# Compute the Sample Variance Matrix and dsqure
# dsqure = t(xi-xbar)S.inv(xi-xbar)
X = as.matrix(Table4.3[, 1:4])
xbar = colMeans(X)
X.cen = X - (rep(1,n) %*% t(xbar))
S = (t(X.cen) %*% X.cen)/ (n-1)
S.inv = solve(S)
apply(X.cen, 1, function(v)t(v) %*% S.inv %*% v) # idnetical to 'dsq' column in lumber

# EXERCISE 4.28 
x = T1.5$solar
qqnorm(x); qqline(x, col = 2)
( r.Q = cor(sort(x), qnorm(ppoints(42))) )

# EXERCISE 4.29 - (a)
hist(x)



# y = rnorm(42)
# qqnorm(y); qqline(y, col = 2)

# 2. Nomarallity Test - I
# 
# r.Q = cor(sort(x), qnorm((1:42 - 0.5) / 42)) # 순서통계량 값과 대응되는 정규분위수
# r.Q # 왜 0.5를 뺴는가? (j-0.5) / n 0이나 1이 안되게 하려고 1/2를 뺌 . 1/2이 아닐 이유도 딱히 없음...
( r.Q = cor(sort(x), qnorm(ppoints(42))) ) # 상관계수가 1에 가깝다고 느끼면 정규분포 따른다고 결론을 내림
# 충분히 큰가? 크지않은가? 판단하는 기준 
# 기각여부를 결정하는 기준값은 0.9726
# 따라서 정규분포라는 귀무가설을 기각할 증거가 충분하다.

# 2. Nomarallity Test - II
# 귀무가설은 정규분포이다. 
shapiro.test(x)
# 따라서 정규분포라는 귀무가설을 기각할 증거가 충분하다.



# EXERCISE 4.29 - (a)
( x = T1.5[5:6] )
x[1] - mean(x$NO2)
mean(x$O3) # (2 x 42 matrix )
t(x - mean(x)) %*% t( var(x) ) %*% (x - mean(x))
x - mean(x)
y = x$NO2 - mean(x$NO2)
z = x$O3 - mean(x$O3)
matrix(y,z)
c(y)
c(z)
new_x <- cbind(y, z)
t(new_x) %*% t(var(x)) %*% new_x

S = cov(new_x)
S

getwd()

## 필수로 알아야 하는 내용 X ##
install.packages("mvtnorm") # 이변량 정규난수 생성하는 패키지
library(mvtnorm)

?rmvnorm
n = 1000
X <- rmvnorm(n, sigma = matrix( c(1.0, 0.8, 0.8, 1), nrow = 2) ) # sigma는 covariance matrix를 의미
head(X)
plot(X[, 1], X[, 2]) # plot(X)

# 전체 50%, 500개가 포함되는 타원을 어떻게 구할 것인가.

# 등고선 그림 그리는 함수
?contour
?outer # z 결정

# Contour plot
S.inv = solve(cov(X))
xbar = colMeans(X)
ff = function(x1, x2) S.inv[1,1] * x1^2 + 2 * S.inv[1,2] * x1 * x2 + S.inv[2,2] * x2^2
x1 = seq(-3, 3, length.out = 100)
x2 = seq(-3, 3, length.out = 100)
z = outer(x1, x2 ,ff)
contour(x = x1, y = x2, z, levels = qchisq(0.9, df = 2) ,col = 2, add = T)
##############################

# Principle Components in Two-Dimensional Problem : Similar to Figure 8.4
library(MASS)
Sigma = matrix(c(1.0, 0.9, 0.9, 1.0), nrow = 2)
X = mvrnorm(100, mu = c(0,0), Sigma = Sigma) # 이변량정규분포로부터 100개의 관측값 생성
plot(X, xlab = "x1", ylab = "x2", asp = 1)
e1 = eigen(Sigma)$vectors[,1] # the first eigen-vector
e2 = eigen(Sigma)$vectors[,2] # the second eigen-vector
arrows(x0 = 0, y0 = 0, x1 = e1[1], y1 = e1[2], col = 4) # 첫 번째 주성분을 나타내는 축 
arrows(x0 = 0, y0 = 0, x1 = e2[1], y1 = e2[2], col = 4) # 두 번째 주성분을 나타내는 축 

text(x = e1[1] + 0.1, y = e1[2] + 0.1, labels = "y1", col = 4)
text(x = e2[1] + 0.1, y = e2[2] + 0.1, labels = "y2", col = 4)

# How to Draw a Biavriate Normal pdf 
mu = c(1,1)
s1 = s2 = 2; rho = 0.3; s12 = rho * s1 * s2
Sigma = matrix(c(s1^2, s12, s12, s2^2), nrow = 2)
n.grid = 50
x = seq(from = mu[1]-4*s1, to = mu[1]+4*s1, length.out = n.grid)
y = seq(from = mu[2]-4*s2, to = mu[2]+4*s2, length.out = n.grid)
z = matrix(nrow = n.grid, ncol = n.grid)
normal.pdf = function(x, mu, Sigma) {
  Sigma.inv = solve(Sigma)
  (det(2*pi*Sigma))^(-1/2)*exp(-0.5 * t(x-mu)%*% Sigma.inv %*% (x-mu))
}

for (i in 1:n.grid) 
  for (j in 1:n.grid)
    z[i,j] = normal.pdf(c(x[i], y[i]), mu, Sigma)
persp(x,y,z, theta = 15, phi = 30, xlab = "x1", ylab = "x2", zlab = "f(x1, x2)")

