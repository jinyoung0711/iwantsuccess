# A set of Observed Sample
n = 10; B = 400 #B는 반복횟수
x = rnorm(n)

# 95% Confidence Interval forthe Population Mean
# Based on the Central Limit Theorem or t-distribution
# 표본의 수가 적어 CLT는 적용 불가
# x가 정규분포에서 나왔다면 (x - x_bar)/sd(x)가 t분포를 따름
a <- t.test(x)
t.test(x)$conf.inf

# 신뢰구간만 보고 싶다면 t.test(x)$conf.inf

# Bootstrap Confidence Interval
f.mean = function(x) mean(sample(x, length(x), rep = T))
b.mean = replicate(B, f.mean(x))

# 400개의 표본평균을 순서대로 정렬한 뒤 0.25quantile ~ 0.975 quantile
quantile(b.mean, probs = c(0.025, 0.975))
# Bootstrap CI의 장점은 모집단 분포에 대한 가정을 하지 않음 !!!

# EXERCISE 7.13

# (a) 
# n개의 표본으로부터 모집단의 분포함수 CDF를 추정하고,
# 추정은 경험적 분포 함수를 사용
# 경험적 분포함수로부터 n개를 복원 추출해서 표본평균을 구하고 모집단의 nu를 뺴주고 
# 반복해준고 비율을 구하면 된다.

# (b) 어느 구간에 속할지를 추정 이런건 I = logical(n) 방법 진행
x = c(56, 101, 78, 67, 93, 87, 64, 72, 80, 69)
xbar = mean(x)
n = length(x); B = 400
I = logical(n)
a = -5; b = 5
for (i in 1:B) {
  xbar.star = mean(sample(x, n, rep = TRUE))
  I[i] = ((xbar.star - xbar) > a) & ((xbar.star - xbar) < b)
}

cat(mean(I), "+/-", 2*sd(I)/sqrt(B)) # Imperial CDF와 true CDF와 비슷하다면 

# EXERCISE 7.15
x = c(5,4,9,6,21,17,11,20,7,10,21,15,13,16,8)
n = length(x); B = 400

f.var = function(x) var(sample(x, n, rep = T))
b.var = replicate(B, f.var(x))
var(b.var) # estimate of var(S^2)

## 2015년도 중간 2번 (아버지와 아들의 키 표본상관계수)
father <- c(164.2,171.2,176.3,165.0,177.9,180.9,172.2,171.6)
son <- c(171.5,176.1,172.9,171.2,187.4,179.2,165.4,176.5)
dat <- data.frame(father,son)

cor(dat[,1],dat[,2]) #아버지와 아들 키의 표본상관계수
# ?cor

B <- 400

# 아버지와 아들의 키를 같은 쌍으로 뽑기 위해 index를 random으로 rep = T 복원 추출!!
f.cor <- function(x) {
  ind <- sample(1:8,8,rep=T)
  cor(x[,1][ind],x[,2][ind])
}
boot <- replicate(B, f.cor(dat))

#표준오차 방법1
# sqrt(sum((boot-mean(boot))^2)/(B-1))
sd(boot)

## 2020년도 중간 2번 (10개 값 주고, 표본 중앙값의 분산을 붓스트랩으로 추정하는 문제)
x <- c(3.78, 5.91, 0.73, 0.70, 2.18,
       14.47, 6.15, 2.70, 4.78, 0.74)
n <- length(x); B = 400
f.median <- function(x) median(sample(x,n, rep = T))
b.median <- replicate(B, f.median(x))

var(b.median)

## 2021년도 중간 3번 문제 (log-normal분포로부터 10개 추출, 신뢰구간이 모평균을 실제로 포함할 확률과 
## 신뢰구간의 평균 길이 추정 방법1과 방법2 비교)

# 모평균이 2가 아니라 E(X) 주어진 값에 대입하는 것 주의
# 같은 자료에 두 방법 적용하는점 주의 (각각 독립적으로 자료 생성 X )

mu = 2; sigma = 1; n = 10; n.sim = 200; B = 400
true.mean = exp(mu + (sigma^2)/2)
I1 = I2 = logical(n.sim)
len1 = len2 = numeric(n.sim)
for (i in 1:n.sim) {
  x = rlnorm(n, meanlog = mu, sdlog = sigma)
  xbar = mean(x); se = sd(x)/sqrt(n)
  LL = xbar - 2.262*se; UL = xbar + 2.262*se
  I1[i] = (true.mean > LL) & (true.mean < UL)
  len1[i] = UL - LL
  x.star = function(x) mean(sample(x, n, replace = TRUE))
  xbar.star = replicate(B, x.star(x))
  squant = quantile(xbar.star, c(0.025, 0.975))
  I2[i] = (true.mean > squant[1]) & (true.mean < squant[2])
  len2[i] = squant[2] - squant[1]
}
c(mean(I1), sd(I1)/sqrt(n.sim))

c(mean(len1), sd(len1)/sqrt(n.sim))
c(mean(I2), sd(I2)/sqrt(n.sim))
c(mean(len2), sd(len2)/sqrt(n.sim))

# Matrix를 통해 풀음 n*B 한번에 추출

mu = 2; sigma = 1; n = 10; n.sim = 200; B = 400
true.mean = exp(mu + (sigma^2)/2)
I1 = I2 = logical(n.sim)
len1 = len2 = numeric(n.sim)
for (i in 1:n.sim) {
  x = rlnorm(n, meanlog = mu, sdlog = sigma)
  xbar = mean(x); se = sd(x)/sqrt(n)
  LL = xbar - 2.262*se; UL = xbar + 2.262*se
  I1[i] = (true.mean > LL) & (true.mean < UL)
  len1[i] = UL - LL
  x.star = matrix(sample(x, n*B, replace = TRUE), nrow = B)
  xbar.star = apply(x.star, 1, mean)
  squant = quantile(xbar.star, c(0.025, 0.975))
  I2[i] = (true.mean > squant[1]) & (true.mean < squant[2])
  len2[i] = squant[2] - squant[1]
}
c(mean(I1), sd(I1)/sqrt(n.sim))
c(mean(len1), sd(len1)/sqrt(n.sim))
c(mean(I2), sd(I2)/sqrt(n.sim))
c(mean(len2), sd(len2)/sqrt(n.sim))
