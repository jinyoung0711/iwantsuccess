# 2021년 중간

# (1)
# 직교정규기저는 유일하지 않으며, 선형독립인 임의의 세 개의 벡터로부터 직교정규기저를 Gram-Schmidt 방법을 이용해 찾을 수 있다.
# 새로운 좌표는 세개의 선형연립방정식을 풀면 구할 수 있다.
# 보다 간단한 방법은 강의노트 4.2절 contour 부분에서 설명한 고유벡터의 성질을 이용하는 방법이다.
# 임의의 공분산 행렬을 이용하면 되는데, 공분산행렬의 고유벡터를 구한 다음, 벡터 x에 대한 세개의 주성분 점수를 구하면,
# 이 주성분 점수가 고유벡터를 기저로 했을 떄의 새로운 좌표가 된다.


S = matrix(c(6.25, 1, 3, 1, 4, 2, 3, 2, 9), nrow = 3)
E = eigen(S)$vectors
E # 각 열이 orthonormal basis 만족
# 각 열이 새로운 직교정규기저가 되는 세 벡터 b1,b2,b3

y =  t(E) %*% c(2,2,2)               # 새로운 기저를 축으로 했을 때 벡터x의 새로운 좌표
y[1]*E[,1] + y[2]*E[,2] + y[3]*E[,3] # 이 곱은 벡터끼리의 곱, not 행렬곱

# (2)
S = matrix(c(194, -379, -35, -379, 7323, 74, -35, 74, 7), nrow = 3)
S == t(S) # 대칭행렬 만족

obj = eigen(S)
P = obj$vectors

D = diag(obj$values)
D.minus.half = diag(1/sqrt(obj$values))

S.minus.half = P %*% D.minus.half %*% t(P)

S.minus.half %*% S.minus.half
solve(S)

# (3)
# 공분산 행렬과 평균 벡터를 살펴보면 조건부확률이 주변확률보다 더 커질 거라는 걸 예상할 수 있다.
# p(x3 > 176)
S = matrix(c(6.25,1,3,1,4,2,3,2,9), nrow = 3)
1 - pnorm(176, mean = 170, sd=3)

# p(x3 > 176 | x1=170, x2=159)
S12 = matrix(c(3,2), nrow = 1)
S22 = S[1:2,1:2]

conditional.mean = 170 + S12 %*% solve(S22) %*% (c(170,159)-c(165,155))

conditional.var = 9 - S12 %*% solve(S22) %*% t(S12)

1 - pnorm(176, mean = conditional.mean, sd=sqrt(conditional.var))


# (4) 정규성 평가 문제 
track = read.table("C:/Users/SF314-59/Desktop/3-2/전공/다변량분석/CD (updated) of JW/T8-6.dat")
head(track)

# chi-square plot 

# (4-1) 일변량에 대한 Q-Q plot
# plots to check univariate normality: hist and qqplot
par(mfrow = c(2,2)) # 한 화면에 여러 hist를 나타내기 위한 방법
apply(track[,2:4], 2, hist)

par(mfrow = c(2,2))
qqnorm.line = function(x) {qqnorm(x);qqline(x, col=2)} # 함수정의
apply(track[,2:4], 2, qqnorm.line)

# test for normality : shapiro test
shapiro.pval = function(x) {shapiro.test(x)$p.value}
apply(track[,2:4], 2, shapiro.pval) 
# 세개의 단변량 변수 모두 정규성 만족하지 않는다. pval < 0.05


# (4-2) 이변량 산점도 행렬
pairs(track[,2:4]) # 타원을 중심으로 밀집되어있으면 좋음 

# plot using chi-square statistics -> df 자유도 뒤지게 헷갈리네...
X = as.matrix( track[,2:4] ) # apply() 사용하기위해 matrix생성
x_bar = colMeans(X)
dsq = apply(X, 1, function(v) {t(v - x_bar) %*% solve(cov(X)) %*% (v - x_bar)})

n = nrow(track)
par(mfrow=c(1,1))
qqplot(qchisq(ppoints(n), df = 3), dsq) # 관측된 dsq자료로부터 카이제곱을 따르는지 알고싶은 것임
qqline(dsq, distribution = function(p) qchisq(p, df=3),
       probs = c(0.1,0.6), col=2)

# (5) 주성분분석
R = matrix(c(1,0.402,0.396,0.301,0.305,0.339,0.340,0.402,
             1,0.618,0.150,0.135,0.206,0.183,0.396,0.618,
             1,0.321,0.289,0.363,0.345,0.301,0.150,0.321,
             1,0.846,0.759,0.661,0.305,0.135,0.289,0.846,
             1,0.797,0.80,0.339,0.206,0.363,0.759,0.797,1,
             0.736,0.34,0.183,0.345,0.661,0.8,0.736,1), nrow = 7)
obj = princomp(covmat = R) # 상관행렬로 주성분 분석 수행
summary(obj, loadings = T) # 고유벡터행렬 뽑아내는 다른 방법
(obj$sdev)^2               # 고유값 = 주성분변수의 각 표준편차의 제곱
plot(obj)

# 첫번 째 주성분은 7개의 신체특성을 종합적으로 나타내고, 두번 째 주성분은 얼굴 부분의 신체특성과 길이를 나타내는 특성의 차이를 나타냄.