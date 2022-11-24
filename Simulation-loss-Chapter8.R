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

# [Exercise 8.5] 직접 해보기 

# [Exercise 8.5] - 교수님 코드
n = 10000 # n번씩 추가로 모의실험해서 정지시간(stopping time)을 결정
generate.y = function(n) {
  z = rnorm(n)
  x1 = (z^3) * exp(z); z2 = -(z^3)*exp(-z)
  y = (x1+x2)/2
}

y = generate.y(n)
repeat {
  if (4 * sd(y) / sqrt(length(y)) <= 0.1) break
  y.add = generate.y(n)
  y = c(y, y.add)
  # y를 이어 붙이는 대신에 분산을 갱신해 나가면 실행시간을 단축할 수 있다.
}

c(mean(y), sd(y)/sqrt(length(y)))


# [Exercise 8.12] - 수업시간 quiz
# y를 x^2으로 둘고 풀기
B = 100


Y = U^2 

# [Antithetic variable]

X = numeric(100)
fun = function(x) integrate(exp(x^2), lower = 0, upper = 1) 

for (i in 1:100) {
  U = runif(1)
  X[i] = exp(U^2) * (1 + exp(1 - 2 * U)) / 2
}
X
c(mean(X), sd(X)/sqrt(100))

# [Control Variate]
X = numeric(100)
fun = function(x) integrate(exp(x^2), lower = 0, upper = 1) 



# [8.12]
n = 100
u = runif(n)
y = u^2
x = exp(u^2)

# b
c = cov(x,y)/var(y); c

control_variable = x - c*(y-mean(y)) # use control variable
control_variable

# c
Antithetic_variable = exp(u^2) * (1 + exp(1 - 2 * u)/ 2)
Antithetic_variable

# d 
c(mean(control_variable), sd(control_variable)/sqrt(n))
c(mean(Antithetic_variable), sd(Antithetic_variable)/sqrt(n))

#### [ week 10 ] ####

# [Example 8l]
n = 1000
y = -log(runif(n)) # rexp를 사용해서 만들수도 있으나 antithetic variable를 이용하기 위해 runif 사용
x = rnorm(n, mean = y, sd = 2)
theta1 = mean(x > 1) # raw simualtion approcach (조건부 기대값을 이용하지 않고 x를 이용해서 추정)
theta2 = mean(1 - pnorm((1 - y) / 2)) # 표준정규분포의 cdf -> pnorm()

# 평균을 바로 구하지 않고 분산이 얼마나 감소하느냐를 알고 싶으면 !
n = 1000
u = runif(n)
y = -log(u)
# y = -log(runif(n)) # rexp를 사용해서 만들수도 있으나 antithetic variable를 이용하기 위해 runif 사용
x = rnorm(n, mean = y, sd = 2)
I = as.numeric(x > 1) # the raw simulation apporoach
EI.y = 1 - pnorm((1 - y) / 2) # conditional expectation
c(mean(I), sd(I)/sqrt(n))
c(mean(EI.y), sd(EI.y)/sqrt(n))

# antithetic variable approach + conditional expectation
y2 = -log(1 - u)
pair = 1 - pnorm((1 - y2) / 2)
EI.y.anti = rowMeans(cbind(EI.y, pair))
c(mean(EI.y.anti), sd(EI.y.anti)/sqrt(n))
# ->  antithetic variable approach가 효과적임을 볼 수 있음..!!

# [Example 8r - 8.4절 실습문제 풀이]
n.max = 5000; n.values = c(5, 10, 100, 500, 1000, 5000)
u1 = runif(n.max)
v1 = 2 * u1 - 1; v2 = runif(n.max, min = -1, max = 1)
for (n in n.values) {
  raw = 4 * mean(v1[1:n]^2 + v2[1:n]^2 <= 1)
  cond = 4 * mean(sqrt(1 - v1[1:n]^2))
  tmp = sqrt(1 - ((u1[1:n] + 0:(n-1))/n)^2)
  str.only = 4 * mean(tmp)
  str.anti = 2 * mean(tmp + sqrt(1 - ((1:n - u1[1:n])/n)^2))
  if (n == 5) cat("n      \t raw      \t cond      \t +strat      \t +antithetic\n")
  cat(n," \t",format(raw, nsmall=6L),"\t",format(cond, nsmall=6L),"\t",
      format(str.only, nsmall=6L),"\t",format(str.anti, nsmall=6L), "\n")
}

# [Example 8r] - 수업시간 quiz
n_max = 5000
n_values = c(5,10,100,500,1000,5000)
u1 = runif(n_max); u2 = runif(n_max)

for (n in n_values) {
  v1 = 2 * u1[1:n] - 1; v2 = 2 * u2[1:n] - 1
  raw = 4 * mean(v1^2 + v2^2 <= 1)
  cond = 4  * mean(sqrt(1 - v1^2))
  tmp = sqrt(1 - ((u1[1:n] + 0:(n-1))/n)^2)
  str.only = 4 * mean(tmp)
  str.anti = 2 * mean(tmp + sqrt(1 - ((1:n - u1[1:n])/n)^2))
  if (n == 5) cat("n \t raw \t condition\t stratified\t str+anti\n")
  cat(n," \t",format(raw, nsmall = 6L),"\t",format(cond, nsmall = 6L),"\t",
      format(str.only, nsmall = 6L),"\t",format(str.anti, nsmall = 6L), "\n")
}

## [8.6 실습 1] ##

get.SN = function(mu, sigma, A = 5, B = 5) {
  S = 0
  repeat {
    S = S + rnorm(1, mu, sigma)
    if (S < -A | S > B) break
  }
  return(S)
}

one.run = function(mu = -0.1, sigma = 0.3, A = 5, B = 5) {
  if (get.SN(mu, sigma, A, B) > B) raw.hat = 1 else raw.hat = 0 # straight simulation estimation
  S = get.SN(-mu, sigma, A, B) # Importance sampling
  if (S > B) imp.hat = exp(2 * mu * S / sigma^2) else imp.hat = 0
  return(list(imp.hat = imp.hat, raw.hat = raw.hat))
}

m.values = c(10,100,500,1000,2000,5000)
imp.hat = raw.hat = numeric()
for (i in 1:max(m.values)) {
  lst = one.run(mu = -0.1, sigma = 0.3, A = 5, B = 5)
  imp.hat[i] = lst$imp.hat; raw.hat[i] = lst$raw.hat
}

options(digits = 4) # 줄 맞춰서 출력하기 위해 필요
for (m in m.values) {
  if (m == 10) cat(" m \tMean_Imp_Est\t SE_Imp_Est \tMean_Raw_Est\t SE_Raw_Est\n")
  cat(format(m,width=4),"\t",format(mean(imp.hat[1:m]),width=8),"\t",
    format(sd(imp.hat[1:m])/sqrt(m),width=8),"\t",
    format(mean(raw.hat[1:m]),width=8),"\t",
    format(sd(raw.hat[1:m])/sqrt(m),width=8),"\n")
}

## [8.6 실습 2] ## 

get.SN = function(mu, sigma, A = 10, B = 8) {
  S = 0
  repeat {
    S = S + rnorm(1, mu, sigma)
    if (S < -A | S > B) break
  }
  return(S)
}

one.run = function(mu = 0.1, sigma = 0.5, A = 10, B = 8) {
  if (get.SN(mu, sigma, A, B) == -A) raw.hat = 1 else raw.hat = 0 # 도달하게 될 확률이라서 코드 수정 
  if (get.SN(mu, sigma, A, B) == B) raw.hat = 1 else raw.hat = 0 # 인듯한데 
  S = get.SN(-mu, sigma, A, B)
  if (S < -A) imp.hat = exp(2 * mu * S / sigma^2) else imp.hat = 0
  return(list(imp.hat = imp.hat, raw.hat = raw.hat))
}

m.values = c(10, 100, 500, 1000, 2000, 5000)
imp.hat = raw.hat = numeric()

for (i in 1:max(m.values)) {
  lst = one.run(mu = 0.1, sigma = 0.5, A = 10, B = 8)
  imp.hat[i] = lst$imp.hat; raw.hat[i] = lst$raw.hat
}

options(digits = 4) # 줄 맞춰서 출력하기 위해 필요
for (m in m.values) {
  if (m == 10) cat(" m \tMean_Imp_Est\t SE_Imp_Est \tMean_Raw_Est\t SE_Raw_Est\n")
  cat(format(m, width=4),"\t",format(mean(imp.hat[1:m]),width = 8),"\t",
      format(sd(imp.hat[1:m])/sqrt(m),width = 8),"\t",
      format(mean(raw.hat[1:m]), width = 8),"\t",
      format(sd(raw.hat[1:m])/sqrt(m),width = 8),"\n")
}

# [2020 기말 1-1]
S0 = 100; a = 10; mu = 0.001; sigma = 0.008
n.sim = 100
Ta = numeric(n.sim)
for (i in 1:n.sim) {
  n = 1
  S1 = S0
  repeat {
    S2 = S1 * exp(rnorm(1, mean = mu, sd = sigma))
    if (abs(S2 - S0) >= a) break
    S1 = S2; n = n + 1
  }
  Ta[i] = n
}
c(mean(Ta), sd(Ta)/sqrt(n.sim))

# [2020 기말 1-2]
S0 = 100; mu = 0.001; sigma = 0.008
n.sim = 1000
Imp = numeric(n.sim)
for (i in 1:n.sim) {
  xvec = rnorm(90, mean = -mu, sd = sigma)
  S = S0 * exp(cumsum(xvec))
  if (min(S) < 80) Imp[i] = exp(2*mu*sum(xvec)/sigma^2) else Imp[i] = 0
}
c(mean(Imp), sd(Imp)/sqrt(n.sim))

# 흔히 붙는 조건
# 하번이라도 일정금액으로 내려가면 (원유가 한번이라도 50$ 밑으로 내려가면 ~)
# [Sec 8.8 실습] - 방법1
exotic = function(v = 100, t=90, K=100, s=30, b=95,
                  mu=0.0001, sigma=0.01) {
  Pofs = v*exp(rnorm(1, s*mu, sqrt(s*sigma^2)))
  Poft = Pofs*exp(rnorm(1, (t-s)*mu, sqrt((t-2)*sigma^2)))
  if (Pofs > v & Poft > K) R = Poft - K else R = 0
  return(R)
}

n.sim = 1000
R.vec = replicate(n.sim, exotic())
c(mean(R.vec), sd(R.vec/sqrt(n.sim)))
  
# Practice exercise in Section 8.8 - 방법2

BSF = function(v, t, K, mu, sigma) { # Black-Scholes formula of C(K,t,v) - 아무 조건이 없는 vanlia, standard 
  b = (t*mu - log(K/v))/(sigma*sqrt(t)) # b is a local variable
  v*exp(t*(mu + sigma^2/2))*pnorm(sigma*sqrt(t) + b) - K*pnorm(b)
}

payoff = function(v, t, K, mu, sigma, s, b, n) {
  R.raw = numeric(n)
  X = rnorm(n, s*mu, sqrt(s)*sigma) 
  Y = rnorm(n, (t-s)*mu, sqrt(t-s)*sigma)
  Ps = v*exp(X); Pt = Ps*exp(Y)
  vec = Ps > b & Pt > K
  R.raw[vec] = Pt[vec] - K
  return(R.raw)
}

n = 10000
R.raw = payoff(v=100, t=90, K=100, mu=0.0001, sigma=0.01, s=30, b=95, n=n)
c(mean(R.raw),sd(R.raw)/sqrt(n))
boxplot(R.raw)
BSF(v=100, t=90, K=100, mu=0.0001, sigma=0.01)

mean(R.raw == 0) # 0 -> option 행사 못함.. 이익이 0이 되는 확률 
