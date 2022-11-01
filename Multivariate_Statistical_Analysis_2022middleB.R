# 1번 
S = matrix(c(194, -379, -35, -379, 7323, 74, -35, 74, 7), nrow = 3)
S

obj = eigen(S)
P = obj$vectors

D = diag(obj$values)
D.minus.half = diag(1/sqrt(obj$values))

S.minus.half = P %*% D.minus.half %*% t(P)

S.minus.half %*% S.minus.half
solve(S)

# 2번
# 임의의 확률벡터 X
rmultinom(9)
?rmultinom

# 3번
A = matrix(c(4, 2, 10,
             2, 9, 5,
             4, 7, 1), nrow = 3)
B = cov(A)

eigenvector.matrix = eigen(B)$vectors

eigenvalue.matrix = diag(eigen(B)$values)

B.star = eigenvector.matrix %*% eigenvalue.matrix %*% t(eigenvector.matrix) # 특이값 분해 공식 
B.star; B

# 4번

Table8.6 <- read.table("T8-6.DAT")
names(Table8.6) = c("Country","100m/s","200m/s","400m/s","800m/m","1500m/m","5000m/m","10000m/m","Maraton/m")
head(Table8.6)

Table8.6[, 5:9] <- Table8.6[,5:9] * 60
names(Table8.6) = c("Country","100m/s","200m/s","400m/s","800m/s","1500m/s","5000m/s","10000m/s","Maraton/s")
obj <- princomp(Table8.6[, 2:9])
summary(obj, loadings = T)

obj1 <- princomp(Table8.6[, 2:9], cor = T)
summary(obj1, loadings = T)

# ans : 미터/초로 변환한 자료는 데이터들의 분산이 차이가 많이 나기 때문에 상관행렬을 바탕으로 주성분 분석을 진행하는 것이 더 좋아보인다.

obj2 <- princomp(Table8.6[, 2:9], cor = T)
summary(obj2, loadings = T)
screeplot(obj2)

R = cor(Table8.6[, 2:9]); R
E = eigen(R)
E$vectors[, 1] * sqrt(E$values[1]) # rho(Y_1, X_1), rho(Y_1, X_2) Correlation Coefficient 

# ans : 두개의 주성분으로 약 92%를 설명할 수 있기 때문에 두개의 주성분이 적절해보인다.
# ans : 첫번째 주성분의 경우 8개 변수의 eigen-vector가 비슷한 값을 갖는다. 따라서 전반적인 종목의 육상기록을 나타낸다고 해석할 수 있다.

order = Table8.6[order(obj2$scores[, 2]),]
head(order, 5)

# ans : 100m, 200m, 400m에서는 다른 나라보다 값이 크지만, 1500m, 5000m, 10000m, marathon에서는 다른 나라들보다 값이 작은 것을 확인할 수 있다.
