# [Example 9.8]
getwd()
setwd("C:/Users/82106/22_다변량분석_데이터")

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

# Figure 9.1 그리기
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
