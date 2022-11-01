# Example 8a
# E[N]의 추정량의 표준오차가 큼을 확인할 수 있다.
alp = 0.9; B = 0.8; n.sim = 100
N = numeric(n.sim)
for (i in 1:n.sim) {
  S0 = 0; N[i] = 1
  repeat {
    S1 = alp * S0 + (1-alp) * rnorm(1)
    if (abs(S1) > B) break
    N[i] = N[i] + 1; S0 = S1
  }
}

c(mean(N), sd(N)/sqrt(n.sim))

# [실습1] Estimating the reliability function of the bridge system 
# 시스템 신뢰도의 추정량을 반복해서 구한 결과로 추정량의 표준오차를 구함
# nI번 반복해서 하나의 추정량을 얻는데, 이 작업을 nJ번 반복해서 nJ개의 추정량을 구함
# nJ개의 추정량을 이용해 추정량의 표준오차를 구함
p = rep(0.5, 5) # component reliability
phi = function(s) max(s[1] * s[3] * s[5], s[2] * s[3] * s[4], s[1] * s[4], s[2]* s[5])
nJ = 100; nI = 200
# Use of antithetic variable vs. No use of antithetic variable
# w는 적용했을 때, 적용하지 않았을 때 구분하기 위해 필요
theta.antithetic = theta.noantithetic = numeric(nJ)
for ( j in 1:nJ ) {
  x = w = numeric(nI)
  for (i in seq(1, nI, 2)) {
    u = runif(5)
    s = as.numeric(u < p); x[i] = phi(s)
    s = as.numeric(1 - u < p); x[i + 1] = phi(s)
    w[i] = x[i]; w[i + 1] = phi(as.numeric(runif(5) < p)) 
  }                           
  theta.antithetic[j] = mean(x)
  theta.noantithetic[j] = mean(w)
}

sd(theta.antithetic)/sd(theta.noantithetic)

# [실습2] Estimating the reliability function of the bridge system
# nI번 반복해서 추정량을 한 번만 구하고 추정량의 표준오차에 관한 성질을 이용해 표준오차를추정 
p = rep(0.5,5) # component reliability
phi = function(s) max(s[1] * s[3] * s[5], s[2] * s[3] * s[4], s[1] * s[4], s[2] * s[5])
nI = 200
# Use of antithetic variable vs. No use of antithetic variable
x = w = numeric(nI); y = numeric(0.5 * nI)
for (i in seq(1, nI, 2)) {
  u = runif(5)
  s = as.numeric(u < p); x[i] = phi(s)
  s = as.numeric(1 - u < p); x[i + 1] = phi(s)
  w[i] = x[i]; w[i + 1] = phi(as.numeric(runif(5) < p))
}
for (i in 1:(0.5 * nI)) y[i] = (x[2 * i - 1] + x[2 * i]) / 2
c(mean(x), sd(y) / sqrt(0.5 * nI)) # 대조변수를 썼을 때의 추정량과 표준오차
c(mean(w), sd(w) / sqrt(nI)) # 대조변수를 쓰지 않았을 때의 추정량과 표준오차
(sd(y) / sqrt(0.5 * nI)) / (sd(w) / sqrt(nI)) # 두 추정량의 표준오차의 비

# Exercise 8.5 직접 해보기 ... 코드는 강의노트에 있음


# [숙제] Exercise 8.12


