# Exercise 8.1
S = matrix(c(5, 2, 2, 5), ncol = 2)
S
E = eigen(S)
E$values[1] / sum(E$values)
cov(S)
R
# Exercise 8.2 - (a)
s12 = 2 / ( sqrt(5) * sqrt(2) )
R = matrix(c(1, s12, s12, 1), ncol = 2) # 상관행렬
R
E = eigen(R); E
E$values[1] / sum(E$values)

# Exercise 8.2 - (b)
# 공분산행렬과 상관행렬이 다른 행렬이기 때문에 당연히 다르다

# Exercise 8.2 - (c)
E$vectors[, 1] * sqrt(E$values[1]) # rho(Y_1, X_1), rho(Y_1, X_2) Correlation Coefficient 
E$vectors[, 2] * sqrt(E$values[2]) # rho(Y_2, X_1), rho(Y_2, X_2) Correlation Coefficient 

exp(-6+0.05*40+3.5)

## Example 8.3
X = read.table("T8-5.DAT")
names(X) = c("pop","degree","employed","gov","homevalue")
colMeans(X) # mean vector
(S = cov(X)) # sample covariance matrix S

# eigen 함수를 이용하여 직접 코딩
(e = eigen(S)$vectors) # eigen-vectors of S
(lm = eigen(S)$values) # eigen-values of S
round(cumsum(lm)/sum(lm)*100, digits=1) # cumulative percentage of total variance

# To print out correlation coefficients between variables and principal components: 교재 444쪽 표 참조
sd.vec = sapply(X,sd)
data.frame(r_y1.xk=sqrt(lm[1])*e[,1]/sd.vec, r_y2.xk=sqrt(lm[2])*e[,2]/sd.vec)

# Draw a scree plot
plot(1:5, lm, type="b", pch=16, axes=F, xlab="", ylab="")
axis(side=1, at=1:5)
axis(side=2, at=c(0,round(max(lm),digits=0)))

# OR we can use the built-in function 'princomp'
obj = princomp(X, cor=FALSE)
summary(obj, loadings=TRUE) # cumulative percentage of total variance
plot(obj) # scree plot

# Example 8.4 : male turtle E
setwd('C:/Users/82106/22_다변량분석_데이터')

turtle = read.table("T6-9.DAT")
X = turtle[turtle$V4 =="male",1:3]
names(X) = c("length", "width", "height")
LX = log(X)
obj = princomp(LX)
summary(obj)
(obj$sdev)^2 # eigenvalues or 주성분의 분산
cov(LX) # covariance matrix 공분산행렬 출력
obj$loadings # or summary(obj, loadings = TRUE) 

summary(princomp(X), loadings = TRUE) # eigen value를 볼 수 있음

# 상관행렬로 주성분 분석을 하는 것은 표준화된 변수의 공분산 행렬로 주성분분석을 한것과 동일
# 표준화된 변수의 공분산행렬은 표준화전 변수들의 상관행렬이 되기 때문.
# 변수의 특성이 다르다거나, 변수의 분산이 차이나는 경우 공분산행렬보다는 상관행렬을 이용하는 것이 좋다.

# Example 8.5
X = read.table("T8-4.DAT")
names(X) = c("JPMorgan", "CitiBank", "WFargo", "RDShell", "ExMobil")
cor(X)
summary(princomp(X, cor = TRUE)) # 상관행렬로 주성분 분석 진행
summary(princomp(X, cor = FALSE))
cov(X)

# Example 8.6 상관행렬만 주어진 경우
R = matrix(c(1, .7501, .6329, .6363, .7501, 1, .6925, .7386,
             .6329, .6925, 1, .6625, .6363, .7386, .6625, 1), ncol= 4)

R
obj <- princomp(covmat = R) # covmat = ()를 해주면 상관행렬을 바탕으로 주성분분석. 
summary(obj, loadings = TRUE)
(obj$sdev)^2

# Exercise 8.18
track.women = read.table("T1-9.DAT")

# KOREA, S와 KOREA, N를 수정해서 T1-9-corrected.TXT라는 새로운 파일을 생성하였음
track.women = read.table("T1-9-corrected.TXT")
names(track.women) = c("Country","m100","m200","m400","m800","m1500","m3000","mara")

# (a) 상관행렬과 고유값, 고유벡터를 출력
X = track.women[,-1]
(R = cor(X)); (S = cov(X))
E = eigen(R)$vectors; lm = eigen(R)$values

# (b)
# 표준화된 변수의 두 주성분을 출력 (두 개의 고유벡터를 출력하면 됨)
# 변수와 주성분의 상관계수를 출력해야 함

obj = princomp(X,cor=T)
summary(obj,loadings=T)
data.frame(r.xky1=sqrt(lm[1])*E[,1], r.xky2=sqrt(lm[2])*E[,2])
# (c) 두 주성분의 해석 - (b)의 출력 결과를 보고 해석하면 됨
# (d) 제1주성분 점수의 크기 순으로 나라 이름을 재배열해서 출력
# 주성분 점수(principal component scores)는 princomp의 출력 결과를 이용하면 편리
track.women[order(-obj$scores[,1]),1] # 주성분점수의 부호를 바꾼 이유는?




## Exercise 8.26
setwd()
psych = read.table("T4-6.DAT")
names(psych) = c("Indep","Supp","Benev","Conf","Lead","Gender","SEstat")
X = psych[,1:5]
## (a) and (b) with S, sample covariance matrix
obj = princomp(X,cor=F)
summary(obj,loadings=T)
plot(obj)
# 변수와 주성분의 상관계수를 출력하려면?
S = cov(X)
E = eigen(S)$vectors; lm = eigen(S)$values
sd.vec = sqrt(diag(S))
data.frame(r.xky1=sqrt(lm[1])*E[,1]/sd.vec,r.xky2=sqrt(lm[2])*E[,2]/sd.vec)
## (c)
y1 = obj$scores[,1]; y2 = obj$scores[,2]
plot(y2,y1)
plot(y2,y1,col=psych[,6]) # 성별로 점의 색을 구분
plot(y2,y1,col=psych[,7]) # 사회경제적지위에 따라 점의 색을 구분
# 성별과 사회경제적지위에 따라 구분되는 4개의 그룹을 점의 색으로 구분하려면 어떻게?

S = matrix(c(1, 0.8, 0, 0,
             0.8, 1.0, 0, 0,
             0, 0, 1, 0.9,
             0, 0, 0.9, 1), nrow = 4)
S

obj <- eigen(S)
obj$values / sum(obj$values) # 2개의 주성분으로 90% 이상을 설명 가능
obj.princomp = princomp(covmat = S)
summary(obj.princomp) 
# eigen vector를 보고싶으면 
summary(obj.princomp, loadings=TRUE)

# Example 8.3
X = read.table('T8-5.DAT')
names(X) = c("pop", "degree", "employed", "gov", "homevalue")
comMeans(X) # mean vector
( S = cov(X))
obj = princomp ( X , cor=FALSE) # 상관행렬로 하고 싶으면 cor = TRUE
summary(obj, loadings = TRUE) # cumulative percentage of total variance
plot(obj)

obj$scores # PC scores

# Example 8.6
S = matrix(c(7476.45, 303.62, 303.62, 26.19), nrow = 2)
obj <- princomp( covmat = S)

# Example 8.6 (a)
obj$loadings[, 1:2]
(obj$sdev)^2

# Example 8.6 (b)
summary(obj)

# Example 8.6 (c) R 코드 공유 예정
x_bar = matrix(c(155.60, 14.70))


e1 = obj$loadings[,1]
# Example 8.6 (d)
e1 * obj$sdev[1] / diag(sqrt(S)) # corr coefficients between y1 vs (x1, x2)
e2 * obj$sdev[2] / diag(sqrt(S)) # corr coefficients between y2 vs (x1, s2)


# 질문1 : Sample Principle Component 구하는 방법
track = read.table('T1-9-corrected.txt')
names(track) = c('country', 'm100', 'm200', 'm400', 'm800', 'm1500', 'm3000', 'marathon')

X = track[, -1]

# How to get sample PC (or PC scores)
# Way 1 : output of princomp
obj <- princomp(X, cor = FALSE)
head(obj$scores)
# Way 2 : by Matrix Multiplication; Need Centering (Not Scaling)
obj$loadings[, 1:2] # eigen vectors
head(t(t(obj$loadings[, 1:2]) %*% t(X))) # (n x 2)
tmp = as.matrix(X) %*% obj$loadings[, 1:2]; head(tmp) # 위 코드와 same
# 주성분 점수는 Centering을 한다. (평균을 0으로 만드는 것 : 관례임)
spc = scale(tmp, scale = FALSE) # 평균만 0으로 만듬
head(spc)
head(obj$scores[, 1:2])

# Example 8.4 : male turtle E
turtle = read.table("T6-9.DAT")
x = turtle[turtle$v4 == 'male', 1:3]
names(x) = c('length', 'width', 'height')




# 2021 중간 5번

R = matrix(c(1.000, 0.402, 0.396, 0.301, 0.305, 0.339, 0.340,
             0.402, 1.000, 0.618, 0.150, 0.135, 0.206, 0.183,
             0.396, 0.618, 1.000, 0.321, 0.289, 0.363, 0.345,
             0.301, 0.150, 0.321, 1.000, 0.846, 0.759, 0.661, 
             0.305, 0.135, 0.289, 0.846, 1.000, 0.797, 0.800, 
             0.339, 0.206, 0.363, 0.759, 0.797, 1.000, 0.736, 
             0.340, 0.183, 0.345, 0.661, 0.800, 0.736, 1.000), nrow = 7)
princomp(R, cor = TRUE)

# 2022 중간 A
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

head(Table8.6[sort(obj2$scores[,2], decreasing = TRUE)], 5)
length(obj2$scores)
str(Table8.6)

Table8.6[sort(obj2$scores[,2], decreasing = TRUE)]

?rank
