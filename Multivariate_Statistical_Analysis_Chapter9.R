setwd('C:/Users/82106/22_다변량분석_데이터')

#### [Example 9.4] Principal Component Method ####
stock = read.table("T8-4.DAT")
names(stock) = c("JPM", "Citi", "WFargo", "Shell", "Exxon")

# 적재행렬 L (estimated factor loadings) 출력 : 인자의 수는 2개
R = cor(stock); p = ncol(R); SD = eigen(R)
E = SD$vectors; lm = SD$values
( L = E[, 1:2]*matrix(c(rep(sqrt(lm[1]), p), rep(sqrt(lm[2]), p)), ncol = 2))

# princomp 함수를 이용해서 주성분을 먼저 구하고 이를 이용해 적재행렬을 구할 수도 있다.
# obj = princomp(stock,cor=T)
# L = data.frame(PC1=obj$sdev[1]*obj$loadings[,1],PC2=obj$sdev[2]*loadings(obj)[,2]); L = as.matrix(L)

# communality 출력
rowSums(L*L)

# specific variances 출력
(psi =diag(R) - rowSums(L*L)) # R의 대각원소는 모두 1

# Residual matrix 출력
R - (L%*%t(L) + diag(psi))

## Table 9.3과 residual matrix 출력: ML method에 의한 적재행렬 추정: Example 9.4 참조.
stock = read.table("T8-4.DAT")
names(stock) = c("JPM","Citi","WFargo","Shell","Exxon")
R = cor(stock)

# 상관행렬 R을 이용한 인자분석, 인자의 수는 2개
(fa.obj = factanal(factors=2, covmat=R, n.obs=nrow(stock) ,rotation="none"))

# residual matrix를 출력하고자 함
L = fa.obj$loadings # 또는 L = loadings(fa.obj)
Psi = diag(fa.obj$uniquenesses)
(res.mat = R - L%*%t(L) - Psi)

# residual matrix를 소수 셋째 자리까지만 출력
apply(res.mat, 2, round, digits=3) 

factanal(factors=1, covmat=R, n.obs=nrow(stock))$PVAL # 적절치 않다
factanal(factors=2, covmat=R, n.obs=nrow(stock))$PVAL # 적절하지 않다라고 할 만한 충분한 근거x
factanal(factors=3, covmat=R, n.obs=nrow(stock))$PVAL
# 상관행렬 R을 이용한 인자분석, 인자이 수는 2개
(fa.obj = factanal(factors = 2, covmat = R, n.obs = nrow(stock), rotatic))
# 또
# 결론적으로 두개의 인자로 자료를 잘 설명하는 편은 아니다.


# [Example 9.6]

# ML Method
deca.R = read.table("E9-6.DAT")
R = as.matrix(deca.R)
(ml.obj = factanal(factors=4, covmat=R, n.obs=280))
L = ml.obj$loadings; Psi = diag(ml.obj$uniquenesses)
res.mat = R - L %*% t(L) - Psi
apply(res.mat,2,round,digits=3)
# PC Method
p = ncol(R); SD = eigen(R)
E = SD$vectors; lm = SD$values
(L = E[,1:4]*matrix(c(rep(sqrt(lm[1]),p),rep(sqrt(lm[2]),p), rep(sqrt(lm[3]),p),rep(sqrt(lm[4]),p)),ncol=4))
(Psi = diag(diag(R) - rowSums(L*L)))
res.mat = R - L %*% t(L) - Psi
apply(res.mat,2,round,digits=3)

# [Example 9.8]

R.dfr = read.table('E9-8-corrected.txt')
names(R.dfr) = c("Gaelic", "English", "History", "Arithmetic", "Algebra", "Geomery")
( R = as.matrix(dfr) )

p = 6; n = 220 # 변수의 수와 표본의 크기
# factanal 함수는 적재행렬을 최대가능도 방법으로 추정
(none.fa = factanal(covmat = R, factors = 2, n.obs = n, rotation="none")) # Table 9.5 (회전 X)
(rota.fa = factanal(covmat = R, factors = 2, n.obs = n, rotation="varimax")) # Table 9.6: 값이 조금 다름 (회전 O)
# Loadings 값은 L matrix (상관계수로 바로 해석 가능 - 첫번째 변수와 Factor1과의 상관계수)

(L = none.fa$loadings)
(Psi = diag(none.fa$uniquenesses)) # Vector 형태

R - L %*% t(L) - Psi # residual matrix (잔차행렬)
# why 대각원소가 0이 아닌가... 계산오차 때문. 0으로 봐도 무방

# Figure 9.1 그리기 중요한거 아님 (몰라도 돼)
plot(none.fa$loadings, xlim=c(0, 1),ylim=c(-0.6, 0.9), pch=16, cex=1.2, asp=1, main="Figure 9.1")
abline(h = 0, col = 4); abline(v = 0, col = 4)
text(none.fa$loadings[, 1], none.fa$loadings[, 2] + 0.04, labels = names(R.dfr), col = 4, cex = 0.8)
T = rota.fa$rotmat # rotation matrix T 
arrows(x0 = 0, y0 = 0, x1 = T[1, 1], y1 = T[2, 1], col = 4, lty = 2) # T(1,0)’ (or the first column of T) is the new x-axis
arrows(x0 = 0, y0 = 0, x1 = T[1, 2], y1 = T[2, 2], col = 4, lty = 2) # T(0,1)’ (or the second column of T) is the new y-axis
text(T[1, ] + 0.05, T[2, ], labels = c("F1*", "F2*"), col = 4)

# 원래 자료가 없으면 인자점수를 구할 수 없음. + 
# 적절한 인자의 수를 결정하는 방법에서 가설 검정에 의해 인자의 수를 결정하는 방법
# -> 원래 자료는 없어도 되지만 표본의 수가 필요함.

# [Example 9.11]
deca.R = read.table("E9-6.DAT")
R = as.matrix(deca.R)
obj = princomp(covmat = R); E = obj$loadings
L = data.frame(F1 = obj$sdev[1]*E[,1], F2 = obj$sdev[2] * E[, 2], F3 = obj$sdev[3]*E[, 3], F4 = obj$sdev[4] * E[, 4]) #적재행렬 계산 
varimax(as.matrix(L)) # 적재행렬의 회전 : 인자의 순서가 교재에 있는 것과 다름에 주의
# 인자의 순서가 바뀌어도 상관 X 주성분은 안됌. 해석만 다르게 해주면 된다.
# The Left Plot of Figure 9.3
obj.rota = factanal(covmat = R, factors = 4, rotation = "varimax")
LT = obj.rota$loadings
plot(LT[,1], LT[,2])
text(LT[,1] + 0.02, LT[,2], labels=1:10, col=4, cex=0.8)

# 최대가능도함수에서는 factanal(rotation = varimax), 주성분 분석에서는 varimax()사용

# [Example 9.12]
stock = read.table("T8-4.DAT")
names(stock) = c("JPM","Citi","WFargo","Shell","Exxon")
fa.obj = factanal(stock,factors=2,rotation="varimax",scores="regression")
#stock.z = scale(stock)
#fa.z.obj = factanal(stock.z,factors=2,rotation="varimax",scores="regression")
plot(fa.obj$scores,pch=16); abline(h=0,v=0)
z = c(.50, -1.40, -.20, -.70, 1.40)
t(fa.obj$loadings) %*% solve(cor(stock)) %*% z 



# [Exercise 9.10]
(R <- matrix(c(1, 0.505, 0.569, 0.602, 0.621, 0.603, 
               0.505, 1, 0.422, 0.467, 0.482, 0.450, 
               0.569, 0.422, 1, 0.926, 0.877, 0.878, 
               0.602, 0.467, 0.926, 1, 0.874, 0.894, 
               0.621, 0.482, 0.877, 0.874, 1, 0.937,
               0.603, 0.450, 0.878, 0.894, 0.937, 1), nrow = 6 ))

(fa.obj = factanal(factors = 2, covmat = R, rotation = "none")) # (a) uniquenesses 값 = specific variance
L = fa.obj$loadings 
Psi = diag(fa.obj$uniquenesses)
(res.mat = R - L %*% t(L) - Psi)

# (d) residual matrix
apply(res.mat, 2, round, digits=3)

# [Exercise 9.28]
track.women = read.table("T1-9.DAT", fill = TRUE, header = FALSE)
names(track.women) = c("country","m100","m200","m400","m800","m1500","m3000","marathon")
head(track.women)

rownames(track.women) = track.women$country
track.women1 = track.women[, -1]

R = cor(track.women1) # using R
p = 7

# [PC Method]
SD = eigen(R)
E = SD$vectors; lm = SD$values
( L = E[, 1:3] * matrix(c(rep(sqrt(lm[1]), p), rep(sqrt(lm[2]), p), rep(sqrt(lm[3]), p)), ncol = 3))

# specific variances
( psi = diag(R) - rowSums(L*L))

# communality
rowSums(L*L)

# The proportion of variance
sum(L[, 1]^2) / p # F1
sum(L[, 2]^2) / p # F2
sum(L[, 3]^2) / p # F3

# Residual matrix
res.mat = R - (L%*%t(L) + diag(psi))
round(res.mat, 3)

# outliers
z = apply(track.women[, 2:8], 2, scale)
scores = t(apply(z, 1, function(x) t(L) %*% solve(R) %*% x))
pairs(scores, labels = c("Factor1", "Factor2", "Factor3"))

# [ML Method]
obj = factanal( x = as.matrix(track.women[, 2:8]), factors = 3, scores = 'regression', rotation = 'varimax')
L = obj$loadings; L = matrix(L[1:21], 7, 3); L

Psi = diag(obj$uniquenesses)
res.mat = R - L %*% t(L) - Psi
round(res.mat, 3)

factanal(factors=1, covmat=R, n.obs=nrow(track.women))$PVAL 
factanal(factors=2, covmat=R, n.obs=nrow(track.women))$PVAL 
factanal(factors=3, covmat=R, n.obs=nrow(track.women))$PVAL
factanal(factors=4, covmat=R, n.obs=nrow(track.women))$PVAL

scores = obj$scores
pairs(obj$scores)


