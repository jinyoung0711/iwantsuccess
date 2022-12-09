#### 다변량분석 2020-2학기 기말시험 풀이 ####
### [1-1] ###
L = matrix(c(.578, .852, .712, .763, .908, .753, .513,
             -.685, -.157, -.086, -.329, .218, .474, .283), ncol = 2)
L
communality = rowSums(L*L)[1]
communality

# x 변수의 공통성을 구할 때는 해당 원소들을 제곱해서 합하면 된다.

### [1-2] ###
colSums(L*L)[1] / 7
# 적재행렬에서 제곱해서 합한 값을 전체 분산의 합으로 나누어주면 된다

### [1-3] ###
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

### [2-1] ###
SD = eigen(R)
E = SD$vectors; lm = SD$values
( L = E[, 1:2] * matrix(c(rep(sqrt(lm[1]), p), rep(sqrt(lm[2]), p)), ncol = 2))

### [2-2] ###
varimax(as.matrix(L))
head(R)
# 첫번째 인자는 장거리 종목(800m, 1500m, 3000m, marathon에 대해 음의값이 강하고, 
# 두번째 인자는 단거리 종목(100, 200, 400m)에 음의값이 강함을 보이면 될듯

### [2-3] ###
rowSums(L*L) # 7개 변수에 대한 공통성 (communality)
colSums(L*L)[1] / 7 # 7개 변수의 전체 분산 중에서 첫번째 인자가 설명하는 비율
colSums(L*L)[2] / 7 # 7개 변수의 전체 분산 중에서 두번째 인자가 설명하는 비율

### [2-4] ###
fa.obj = factanal(track.women1, factors = 2, rotation = 'varimax', scores = 'regression')
plot(fa.obj$scores, pch = 16); abline(h=0, v=0)

### [2-5] ###

none.fa = factanal(covmat = R, factors = 2, n.obs = nrow(track.women1), rotation="none") # Table 9.5 (회전 X)
none.fa

(L = none.fa$loadings) # L matrix
(Psi = diag(none.fa$uniquenesses)) # Vector 형태

R - L %*% t(L) - Psi # Restidual matrix
