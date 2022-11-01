# Chapter 3 Random Numbers 

# runif() 난수 발생 함수
# Uniform distribution (0,1) 

## 3장 실습문제 1

n = 100; m = 2^31 -1 ; a = 7^5; x0 = 1
x = integer(n) # x가 백터임을 표현해주기 위해 integer(n) 사용
x[1] = (a*x0) %% m
for (i in 2:n) x[i] = ( a*x[i-1] ) %% m
x/m
# %% modulo 연산자는 *과 /보다 우선순위가 높음
# R에서는 벡터가 0을 지원하지 않기 때문에 2:n+1 주의할 것 

xgen <- function(n = 1, m = 2^31 -1 ,a = 7^5, x0 = 1) {
  x = integer(n) 
  x[1] = (a*x0) %% m
  for (i in 2:n) x[i] = ( a*x[i-1] ) %% m
  x/m
}

xgen(10)

## 3장 실습문제 2
u = xgen(1000)
plot(u) # 시간에 따른 규칙이 없어보임.. -> 독립성 문제 X
hist(u) # 주관적인 해석이긴 하지만 Frequency가 비슷하고 Sampling variability (표집분포) 에 따른 균일분포로 해석 가능

## 3장 실습문제 3

duplicated(u)

## 3장 실습문제 4

system.time(xgen(10000000))
system.time(runif(10000000))

# Example 3a: 원주율의 추정
n = 10000
count = 0
for (i in 1:n) {
  u <- runif(2)
  if (u[1]^2 + u[2]^2 <= 1) count = count + 1
}
4*count / n

# Example 3a-1 : for문 사용 X

u1 = runif(n); u2 = runif(n)
4*mean (u1^2 + u2^2 <= 1)

## 3장 실습문제 7번

integrate(function(x) exp(-x^2), -Inf, Inf)
sqrt(pi)

m <- 10000
y <- runif(m)

2*mean( (1/y^2) * exp ( -1 * (1/y-1)^2 ))

''
m = 1000
N = integer(m)
for ( i in 1:m) {
  N[i] = 1
  csum = runif(1)
  while (csum <= 1) {
    N[i] = N[i] + 1
    csum = csum + runif(1)
  }
}
mean(N); hist(N)
''

## 3장 실습문제 12번
## U1 ~ UN 독립인 U(0,1), N = Minimum(n : sigma i ~ N Ui > 1)일 때, E[N] 추정

jin_fun <- function(m = 1000) {
  N = integer(m)
  for ( i in 1:m ) {
    N[i] = 1
    csum = runif(1)
    while (csum <= 1) {
      N[i] = N[i] + 1
      csum = csum + runif(1)
    }
  }
  mean(N); hist(N)
  sd(N)
  
  CI <- c(mean(N) - 1.96 * sd(N) / sqrt(m), mean(N) + 1.96 * sd(N) / sqrt(m) )  
  CI
}

jin_fun()

# 2016 기출 1번
## U1 ~ UN 독립인 U(0,1), N = Minimum(n : cumprod i ~ N Ui < 0.1)일 때, 나타내기

simul <- function() {
  n <- 1
  u <- numeric()
  repeat {
    u <- c(u, runif(1))
    if(cumprod(u)[n] < 0.1) break
    n = n + 1
  }
  n
}
N <- replicate(1000, simul())
table(N)

#직각삼각형
tri2<-function(n){
  for (i in 1:n){
    interval<-sort(runif(2))
    length<-sort(c(interval[1], interval[2]-interval[1], 1-interval[2]))
    if(length[3]^2==length[1]^2+length[2]^2){
      count[i]=TRUE
    }
  }
  cat("삼각형 개수 : ", sum(count2), "\n")
  cat("삼각형이 될 확률 추정 : ", mean(count2), "\n")
  cat("삼각형이 될 확률 추정값 표준 오차 : ", sd(count2), "\n")
  cat("삼각형이 될 확률 추정 신뢰구간 : ", c(mean(count2)-sd(count2)/sqrt(n), mean(count2)+sd(count2)/sqrt(n)))
}

n<-1000000
count2<-vector("logical", n)
tri2(n)