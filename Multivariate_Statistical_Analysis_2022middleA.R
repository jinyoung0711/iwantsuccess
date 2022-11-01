# 2022 중간 A

# 1-1
x <- matrix(c(-0.259, -0.477, -0.840, 
              0.0853, -0.8773, 0.4723,
              0.9621, -0.0508, -0.2680), nrow = 3)

y = diag(c(20.64, 5.58, 2.78))

A = x %*% y %*% t(x) # 특이값 분해 공식 

# 1-2
 
A.half = x %*% sqrt(y) %*% t(x) # 대각행렬은 sqrt 적용 씹가능 
A.half %*% A.half

# 1-3
y2 = diag(1/sqrt(c(20.64,5.58,2.78)))
A.half.minus = x %*% y2 %*% t(x)
A.half.minus %*% A.half.minus
A.inv = solve(A)
A.inv

# 2

# p(x3 > 178)
S = matrix(c(9, 3, 6, 3, 4, 4, 6, 4, 16), nrow = 3)
1 - pnorm(178, mean = 170, sd = 4)

# p(x3 > 178 | x1=171, x2=159)
S12 = matrix(c(6,4), nrow = 1)
S22 = S[1:2,1:2]

conditional.mean = 170 + S12 %*% solve(S22) %*% (c(171,159)-c(165,155))

conditional.var = 16 - S12 %*% solve(S22) %*% t(S12)

1 - pnorm(178, mean = conditional.mean, sd=sqrt(conditional.var))

# 3-1 : 표준화했을 때와 하지 않았을 때 표본상관계수가 동일함을 증명
x <- matrix(c(1,5,6,2,7,2,4,5), ncol = 2)
x

cov(x[, 1], x[, 2]) / (x_sd1 * x_sd2)

new_x <- scale(x)

newx_bar1 <- mean(new_x[ ,1]); newx_sd1 <- sd(new_x[, 1])

newtemp1 <- (new_x[, 1] - newx_bar1 ) / newx_sd1

newx_bar2 <- mean(new_x[ ,2]); newx_sd2 <- sd(new_x[, 2])

newtemp2 <- (new_x[, 2] - newx_bar2 ) / newx_sd2

cov(newtemp1, newtemp2) / (newx_sd1 * newx_sd2)

# 3-2 : 앞 문제에서 증명한 사실이 주성분 분석에 대해 시사하는바 ?
# 즉, 주성분분석을 표본공분산행렬 S로 하는 것과 표본상관행렬 R로 하는 것에 대해 어떤 점을 알려주는가?

# Answer. 상관행렬로 주성분 분석을 하는 것은 표준화된 변수의 공분산 행렬로 주성분분석을 한것과 동일
# 표준화된 변수의 공분산행렬은 표준화전 변수들의 상관행렬이 되기 떄문.

# 4
Table8.6 <- read.table("T8-6.DAT")
names(Table8.6) = c("Country","100m/s","200m/s","400m/s","800m/m","1500m/m","5000m/m","10000m/m","Maraton/m")
head(Table8.6)

Table8.6[, 5:9] <- Table8.6[,5:9] * 60
names(Table8.6) = c("Country","100m/s","200m/s","400m/s","800m/s","1500m/s","5000m/s","10000m/s","Maraton/s")
obj <- princomp(Table8.6[, 2:9])
summary(obj, loadings = T)

obj1 <- princomp(Table8.6[, 2:9], cor = T)
summary(obj1, loadings = T)

# ans : 데이터의 값들이 분산이 차이가 많이 나기 때문에 상관행렬을 바탕으로 주성분 분석을 진행하는 것이 더 좋아보인다.

obj2 <- princomp(Table8.6[, 2:9], cor = T)
summary(obj2, loadings = T)

# ans : 두개의 주성분으로 약 92%를 설명할 수 있기 때문에 두개의 주성분이 적절해보인다.
# ans : 첫번째 주성분의 경우 8개 변수의 eigen-vector가 비슷한 값을 갖는다. 따라서 전반적인 종목의 육상기록을 나타낸다고 해석할 수 있다.

order = Table8.6[order(obj2$scores[, 2]),]
head(order, 5)

# ans : 100m, 200m, 400m에서는 다른 나라보다 값이 크지만, 1500m, 5000m, 10000m, marathon에서는 다른 나라들보다 값이 작은 것을 확인할 수 있다.