#### 다변량분석 2020-2학기 기말시험 풀이 ####
### [1-1] ### - Factor Analysis
L = matrix(c(.578, .852, .712, .763, .908, .753, .513,
             -.685, -.157, -.086, -.329, .218, .474, .283), ncol = 2)
L
communality = rowSums(L*L)[1]
communality

# x 변수의 공통성을 구할 때는 해당 원소들을 제곱해서 합하면 된다.

### [1-2] ### - Factor Analysis : 
colSums(L*L)[1] / 7
# 적재행렬에서 제곱해서 합한 값을 전체 분산의 합으로 나누어주면 된다

### [1-3] ### - Factor Analysis : Residual Matrix
LLT = L %*% t(L)
c(0, c(.601, .484, .649, .386, .102, .069) - LLT[2:7, 1])

# 잔차행렬은 /(R-LL'-/Psi/)이며 대각원소는 0이라는 점을 이용하여 계산.
# 잔차행렬의 대각원소는 0ㅇ어야 하므로 첫 번쨰 원소는 0이다.!!!! 틀리면 감점

#### 다변량분석 2021-2학기 기말시험 풀이 ####
### [2] ###
setwd('C:/Users/82106/22_다변량분석_데이터')

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
# 교수님 답이랑 적재값들의 부호가 다름 -> ㄱㅊ?

# specific variances 출력
(psi = diag(R) - rowSums(L*L)) # R의 대각원소는 모두 1

# Residual Matrix 출력 
(Res.mat = R - (L%*%t(L) + diag(psi)))

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
