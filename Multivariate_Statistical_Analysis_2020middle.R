# 2020년 중간 

# [1]
paper = read.table("T7-7.dat")
head(paper)
names(paper)[1:4] <- c('BL','EM','SF','BS')

# (A)
X = as.matrix( paper[,1:4] )
(S = cov(X))
det(S)        # 공분산행렬의 행렬식 0에 가까움 -> 선형종속에 가까운 열이 존재

( vhalf = diag(c(sqrt(S[1,1]),sqrt(S[2,2]),sqrt(S[3,3]),sqrt(S[4,4]))) )
( vhalf.inv = solve(vhalf) )
(corr.mat = vhalf.inv %*% S %*% vhalf.inv)

det(corr.mat) # 상관행렬의 행렬식 또한 0에 가까움 -> 선형종속에 가까운 열이 존재

det( t(X) %*% X )

# (1) 상관행렬과 표준편차행렬 
S <- matrix(c(25,-2,4,-2,4,1,4,1,9), nrow = 3, byrow = T) # sigma
S
v_half <- diag(c(5,2,3)) # s의 sqrt(분산) 표준편차를 대각원소로 갖는 행렬 생성
v_half_inv <- solve(v_half) # 역행렬 구하기 solve
Corr_mat = v_half_inv %*% S %*% v_half_inv # 행렬곱 %*%, 상관행렬의 대각원소(=Rho)는 반드시 1이 된다.
S == ( v_half %*% Corr_mat %*% v_half ) # s와 같은 행렬이 만들어짐

# (B) 네 변수가 다변량 정규분포를 따른다고 가정하고 시작
X = as.matrix( paper[,1:4] )
x_bar = colMeans(X)
(S = cov(X)) # symmetric matrix

# P(X1>24)
1 - pnorm(24, mean = x_bar[1], sd = sqrt(S[1,1])) # 0.2146812

# p(x1>24 | x2,x3,x4)
S12 = matrix(c(S[1,2], S[1,3], S[1,4]), ncol=3)
S22 = S[2:4,2:4]

( conditional.mean = x_bar[1] + S12 %*% solve(S22) %*% ( c(8, 7, 1.5) - c(x_bar[2],x_bar[3],x_bar[4]) ) )
( conditional.var = S[1,1] - S12 %*% solve(S22) %*% t(S12) )

1 - pnorm(24, mean = conditional.mean, sd = sqrt(conditional.var)) # 0.4942217

# (C)
qqnorm(paper[,1]);qqline(paper[,1], col = 2) # 주관적 방법
shapiro.test(paper[,1]) # 객관적 방법
# pval 이 0.05보다 작으므로 X1이 정규분포라는 귀무가설 기각

# (D)
par(mfrow=c(2,2))
qqnorm.line = function(x) { qqnorm(x);qqline(x, col=2)}
apply(paper[,1:4], 2, qqnorm.line)

shapiro.pval = function(x) { shapiro.test(x)$p.value}
apply(paper[,1:4], 2, shapiro.pval) # 네 변수 모두 정규성 기각

# 네 변수에 대해 주변 정규성을 점검하였을때 모두 유의하지 않으므로 다변량 정규성 또한 만족하지 않을 것으로 예상할 수 있다

# 첫번째 변수의 정규성이 기각되었으므로 다변량 정규성을 점검하지 않아도 된다(O)
# 하지만, 4개 변수 각각에 단변량 정규성 검정을 실시해서 다변량 정규성을 판단하는 답안(X)

## Solution
# 여기서부터 이 문제를 풀기위해 진짜 필요한 부분

# 점검방법 설명 -> d^2을 계산하여 카이제곱분포 qqplot을 그려 카이제곱분포가 적절한지를 그림으로 살펴본다(O)
pairs(paper[,1:4])         # 산점도 그려보기

X = as.matrix( paper[,1:4] )
x_bar = colMeans(X)
dsq = apply(X, 1, function(v) {t(v - x_bar) %*% solve(cov(X)) %*% (v - x_bar)})

n = nrow(paper)
par(mfrow=c(1,1))
qqplot(qchisq(ppoints(n), df = 4), dsq) # 관측된 dsq자료로부터 카이제곱을 따르는지 알고싶은 것임
qqline(dsq, distribution = function(p) qchisq(p, df=4),
       probs = c(0.1,0.6), col=2)

# 결론: 다변량 정규분포를 따르지 않는다.

mean(dsq > qchisq(0.5, df=4))  # 카이제곱 분포의 중위수를 초과하는 dsq관측값 비율이 0.5와 얼마나 가까운지 살펴본다
# 이 부분은 수업시간에 소개된 내용은 아니지만 참고해두기