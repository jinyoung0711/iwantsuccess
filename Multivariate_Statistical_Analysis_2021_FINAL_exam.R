#### Multivariate Statistical Analysis 2021 FINAL exam ####
### [1] 문제 오류로 전원 만점 처리 ###

### [2] ### : Factor Analysis
track.women = read.table("T1-9.DAT", fill = TRUE, header = FALSE)
names(track.women) = c("country","m100","m200","m400","m800","m1500","m3000","marathon")
head(track.women)

rownames(track.women) = track.women$country
track.women1 = track.women[, -1]

R = cor(track.women1) # using R
p = ncol(R)

### [2-1] ### - Factor Analysis : # facotr = 2, Loading Matrix, Residual Matrix (PC_Method)
SD = eigen(R) 
E = SD$vectors; lm = SD$values
( L = E[, 1:2] * matrix(c(rep(sqrt(lm[1]), p), rep(sqrt(lm[2]), p)), ncol = 2)) 

# specific variances 출력
(psi = diag(R) - rowSums(L*L)) # R의 대각원소는 모두 1

# Residual Matrix 출력 
(Res.mat = R - (L%*%t(L) + diag(psi)))

### [2-1] ### - 교수님 code
obj = princomp(track.women[,2:8], cor=TRUE)
E = obj$loadings[,1:2]
(L = cbind(obj$sdev[1]*E[, 1], obj$sdev[2]*E[, 2])) # loading matrix whe

### [2-2] ### - Factor Analysis : varimax rotation
L.rot = varimax(as.matrix(L))$loadings[1:7, 1:2]
# 첫번째 인자는 중장거리 종목(800m, 1500m, 3000m, marathon)에 대한 해석, 
# 두번째 인자는 단거리 종목(100, 200, 400m)에 대한 해석.

### [2-3] ### - Factor Analysis : communality, Proportion 
rowSums(L.rot*L.rot) # 7개 변수에 대한 공통성 (communality)
colSums(L.rot*L.rot)[1] / 7 # 7개 변수의 전체 분산 중에서 첫번째 인자가 설명하는 비율 : 47%
colSums(L.rot*L.rot)[2] / 7 # 7개 변수의 전체 분산 중에서 두번째 인자가 설명하는 비율 : 44%
# 두개의 인자만으로 전체 분산의 91%를 설명할 수 있다.

### [2-4] ### - Factor Analysis : Factor score, plot
fa.obj = factanal(track.women1, factors = 2, rotation = 'varimax', scores = 'regression')
plot(fa.obj$scores, pch = 16); abline(h=0, v=0)


Z = scale(track.women[, 2:8])
R = cor(track.women[, 2:8])
fs.mat = Z %*% solve(R) %*% L.rot
plot(fs.mat[,1], fs.mat[,2], type="n", xlab="", ylab="")
text(x=fs.mat[,1], y=fs.mat[,2], labels=track.women$country, cex=0.5)

### [2-5] ### - Factor Analysis : L matrix, Residual Matrix (ML_Method)

none.fa = factanal(covmat = R, factors = 2, rotation="none") # Table 9.5 (회전 X)
none.fa$loadings

(L = none.fa$loadings) # L matrix
(Psi = diag(none.fa$uniquenesses)) # Vector 형태

R - L %*% t(L) - Psi # Restidual matrix

### 3번 ###
# K-평균 군집분석 결과 형성된 세 군집의 GPA, GMAT 평균이 centers라는 성분에 저장된다. 
# 이 값을 보면 어떤 군집이 어떤 모집단인지를 알 수 있다. 
# kmeans 함수는 시작점을 랜덤하게 선택하므로 실행할 때마다 결과가 달라지게 된다.
# 이를 방지하려면 set.seed 함수를 이용하거나 kmeans 함수의 centers argument에 시작점 3개를 지정해야 한다.
# 시작점 3개를 1번, 32번, 60번 행에 있는 지원자로 지정하면 소속 군집을 나타내는 1, 2, 3과 합격 여부를 나타내는 1,2, 3의 의미가 같아지게 된다. 
# 이 방법을 적용하면 답은 (26+26+23)/85 ≐ 0.882, 5/31 ≐ 0.161, 2/30 ≐ 0.067 이 된다

# 군집을 어떤 모집단으로 판단할까를 정할 때 GPA와 GMAT를 합친 평균을 보고 정하면 안된다.
# 두 시험의 성적 단위가 크게 다르기 때문이다.

### [3-1] ###
dfr = read.table("T11-6.DAT")
names(dfr) = c("GPA","GMAT","POP")
dfr.std = dfr
dfr.std[,1:2] = scale(dfr[,1:2])  # 2개의 변수(gpa, gmat)를 표준화
obj = kmeans(dfr.std[,1:2], centers=dfr.std[c(1,32,60),1:2])
(tbl = table(dfr$POP, obj$clust))

# 군집분석으로 얻어진 군집이 실제 모집단과 일치하는 비율
sum(diag(tbl))/85
# 실제 합격자 중에서 대기자 군집에 속하게 되는 비율
tbl[1,3]/sum(tbl[1,])
# 대기자 군집에 속하는 지원자 중에서 실제 불합격한 비율 
tbl[2,3]/sum(tbl[, 3])
