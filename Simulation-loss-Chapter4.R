# Generating Discrete Random Variables

# R sample(x, size, replcae = FALSE, prob = NULL) replace 복원 비복원 여부, [rpb : 각각 원소 추출 확률]

# Example 4a
sample(1:4, 1, prob = c(0.2, 0.15, 0.25, 0.4))

# Generating discrete uniform random numbers
sample(1:10, 100, replace = TRUE) # or trunc(10 * runif(100)) + 1 : trunc는 가우스 함수를 의미

# 주사위 6번 실험함
sample ( 1:6, 10, rep = T)
rmultinom(10, 1, prob = rep(1/6, 6)) # 행변수는 횟수, 열변수는 결과
rmultinom(10, 100, prob = rep(1/6, 6)) # 주사위를 100번 던지는 실험을 10번

# Geometric random number rgeom(n, prob) 교재에서는 (처음 성공할 때까지) 시행횟수, R에서는 실패횟수수
# rbinom(n, size = 1, prob), rpois(n, lambda) : labmda can be a vector
# rbinom(n, size, prob) size가 베르누이 시행의 횟수, n은 난수의 개수

## Exercise 4.4 ## 평균과 분산 모두 1에 수렴
m = 10000 # 반복횟수 m
x = numeric(m)
for (i in 1:m) x[i] = sum(1:100 == sample(1:100, 100))
c(mean(x), var(x))

# 예[1]

m1 = -2; m2 = 1; s1 =1 ; s2= 3;
alp = 0.6
curve(dnorm(x,m1,s1), from = -8, to = 8, lty = 2, col = 'blue', ylab ="")
curve(dnorm(x,m2,s2), lty = 2, col = 'blue', add = T)
f.mixture <- function (x) alp * dnorm(x, m1, s1) + (1-alp)*dnorm(x, m2, s2)
curve(f.mixture(x), lwd=2, add = T)

# [예]2
n = 100
x1 = rnorm(n, m1, s1); x2 = rnorm(n, m2, s2)
x = ifelse(runif(n) < alp, x1, x2)
x

## Exercise 4.12 
lm = 5; k = 10; n = 20

# Method 1 
sample(0:k, n, rep = T, prob = dpois(0:k, lambda = lm)) 
# 분포 앞에 r이 붙으면 분포를 갖는 난수 생성
# 분포 앞에 d가 붙으면 분포의 확률 (각각의 점에서의 확률) 
# 분포 앞에 p를 붙이면 누적확률 (CDF)
# 분포 앞에 q가 붙으면 quantile 분위수

# Method 2
X = numeric(n)
i = 1

while (i <= n) {
  a = rpois(1, lm)
  if (a <= k) {X[i] = a; i = i + 1}
}
X

# Exercise 4.16
n = 1000
alp = 1/2
x = numeric(n)
x1 = rgeom(n, 1/2) + 1; x2 = rgeom(n, 1/3) + 1
x = ifelse(runif(n) < alp, x1, x2)
x
plot(x)


# Exercise 4.7

n = 10000; total = numeric(n) ;count = 0; m = 11

for (i in 1:n) {
  res = numeric(m)
  while(sum(res) < m) {
    x <- sum(sample(1:6, 1), sample(1:6, 1))
    res[x] <- 1
    count <- count + 1
  }
  total[i] <- count
  count = 0
}

mean(total)

## EXERCISE 4.7 - (교수님 답안)
coupon_collection = function() {
  TF = vector("logical", 12)
  n.rolls = 0
  while ( !all(TF[2:12])) {
    pos = sum(sample(1:6,2, replace = TRUE))
    TF[pos] = TRUE
    n.rolls = n.rolls + 1
  }
  n.rolls
}

n.sim = 1000
N = replicate(n.sim, coupon_collection()) # replicate() 어떤 함수의 결과가 스칼라 값이 나온다면 사용하기 좋음
c(mean(N), sd(N)/sqrt(n.sim)) # mean(N)은 X_bar, 구간 추정의 경우 sigma / sqrt(n)
( CI = c(mean(N) - 2 * sd(N)/sqrt(n.sim), mean(N) + 2 * sd(N)/sqrt(n.sim)) )


# 3개의 정규분포 mix

# m <- runif(3) * 10 ; s <- runif(3) * 10
m = c(-2,0,2); s=rep(0.5,3)

# alp <- sort(runif(3-1))
alp = c(1/3, 2/3)

curve(dnorm(x, m[1], s[1]), from=-16, to=16, lty=2, col="blue", ylab="")

curve(dnorm(x, m[2], s[2]), lty=2, col="blue", ylab="", add = TRUE)

curve(dnorm(x, m[3], s[3]), lty=2, col="blue", ylab="", add = TRUE)

f.mixture <- function(x) {alp[1]*dnorm(x,m[1],s[1]) + (alp[2]-alp[1])*dnorm(x,m[2],s[2]) + (1-alp[2])*dnorm(x,m[3],s[3])}

curve(f.mixture(x), lty=1, lwd=2, ylab = "", add = TRUE)

n = 1000
x1 <- rnorm(n, m[1], s[1]) ; x2 <- rnorm(n, m[2], s[2]) ; x3 <- rnorm(n, m[3], s[3])
x = ifelse(runif(n)<alp[1], x1, 
           ifelse(runif(n)<alp[2], x2, x3))
hist(x)

## 2016년 중간 2번
#2-1
X <- rpois(1000,4)

mean(X) #X의 추정량
se <- sd(X)/sqrt(1000); se #표준오차
mean(x) + c(-1,1)*qnorm(0.95)*se

quantile(X, probs=c(0.025,0.975)) #95%신뢰구간


#2-2
#일주일 동안(주말 제외 5일간) 발생하는 교통사고 건수는 평균발생률 람다=5인 포아송 과정을 따른다. 
#월,화,수,목,금에 발생하는 교통사고 건수 X1,X2,X3,X4,X5라고 할 때 포아송 과정의 성질에 의해 이것들은 각각 평균발생률이 람다=1인 포아송 과정을 따른다. 평일 중 무작위로 하루 개방하지 않기로 한다면, 일주일동안 (주말과 무작위 평일 하루를 제외한 평일 4일 동안) 발생하는 교통사고 건수 X는 평균발생률 람다=4인 포아송 과정을 따른다. 따라서 일주일 간 발생한 건수를 평균발생률 람다=4로 볼 수 있기 때문에 E(X)=4이다.


simul <- function() {
  n <- 1
  u <- numeric()
  repeat {
    u <- c(u,runif(1))
    if(cumprod(u)[n]<0.1) break
    n=n+1
  }
  n
}
N <- replicate(1000,simul())
table(N[1<=N&N<=9])

## 2021년 중간 1번
# A 지점에서 B 지점으로 이동하기 위해 택시를 타면 X1 시간이 걸리고, 버스를 타면 X2 시간이 걸린다. 
# 비가 오면 택시를 타고, 비가 오지 않으면 버스를 타는 경우 A 지점에서 B 지점으로 가기 위해 걸리는 시간을 Y라고 하자. 
# Y와 같은 분포를 갖는 난수 400개를 생성하여 히스토그램을 그리시오. 
# 비가 올 확률은 0.2이며, X1 분포는 0.5에서 1.5 사이의 균일분포, U(0,5, 1.5)고, X2 분포는 U(0.9, 1.3)다.

x1 = runif(400, 0.5, 1.5)
x2 = runif(400, 0.9, 1.3)
x.mixture = ifelse(runif(400) < 0.2, x1, x2)
hist(x.mixture)