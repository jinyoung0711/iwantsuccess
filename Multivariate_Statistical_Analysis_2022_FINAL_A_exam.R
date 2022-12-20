#### Multivariate Statistical Analysis 2022 FINAL exam ####

### [1] 수기로 풀음 ###

### [2] 수기로 풀음 ###

### [3-1] ###
Table4.3 <- read.table("T4-3.DAT.txt") 
names(Table4.3) <- c('x1', 'x2', 'x3', 'x4', 'dsq') 
df <- Table4.3[, 1:4]
C <- cov(df)

# ML Method
factanal(factors=1, covmat=C, n.obs=nrow(df))$PVAL 
factanal(factors=2, covmat=C, n.obs=nrow(df))$PVAL 

# 적절한 인자의 수는 1개이다.


# [ 3-2 ] 
# PC Method #
obj = princomp(df) # 원자료 말고 df 넣기
L = data.frame(PC1=obj$sdev[1]*obj$loadings[,1],PC2=obj$sdev[2]*loadings(obj)[,2])
L = as.matrix(L) ; L

# specific variances 출력
(psi = diag(C) - rowSums(L*L)) # R의 대각원소는 모두 1

# Residual Matrix 출력 
(Res.mat = C - (L%*%t(L) + diag(psi)))

# [3-3]
z = apply(track.women[, 2:8], 2, scale)
scores = t(apply(z, 1, function(x) t(L) %*% solve(R) %*% x))
pairs(scores, labels = c("Factor1", "Factor2", "Factor3"))


# [3-3] 인자점수
Z = scale(df)
R = cor(df)
score = Z %*% solve(R) %*% L
plot(score[,1], score[,2], type="n", xlab="", ylab="")
text(x=score[,1], y=score[,2], cex=0.5)
# 9, 16번이 특이값임을 확인할 수 있다.

# [4-1] #
dfr = read.table("T11-6.DAT")
names(dfr) = c("GPA","GMAT","POP")
dfr.std = dfr
dfr.std[,1:2] = scale(dfr[,1:2])  # 2개의 변수(gpa, gmat)를 표준화

hc.ward = hclust(dist(dfr.std[, 1:2]), method = 'ward.D2')
plot(hc.ward, xlab = '', ylab = 'Distance')

# [4-2] #

ward = as.matrix(cutree(hc.ward, 3))
dfr.std[, 4] = ward
(tbl = table(dfr.std$POP, dfr.std$V4))
sum(diag(tbl))/85

# [4-3] #
hc.average = hclust(dist(dfr.std[, 1:2]), method = 'average')
plot(hc.average, xlab = '', ylab = "Distance")
average = as.matrix(cutree(hc.average, 3))                    
dfr.std[, 5] = average
(tbl = table(dfr.std$POP, dfr.std$V5))
sum(diag(tbl))/85
