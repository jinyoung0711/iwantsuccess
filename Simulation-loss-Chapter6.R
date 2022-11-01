# Exercise 6.1 : A single-Server Queueing System

n.sim = 1000
rate = 10; # 도착시간 간격은 지수분포
shape = 3; scale = 1/40; # 서비스 시간은 감마분포
T = 9
time.over = time.spend = numeric(n.sim) # 고객들의 평균체류시간과 직원(server)의 과외근무시간
for (i in (1:n.sim)) {
  t = NofA = NofD = 0
  n = 0 # SS (System State) is the number of customers in the system
  tA = rexp(1, rate) # tA는 현재 시각 이후 다음 고객이 도착하는 시각.
  tD = Inf # tD는 현재 서비스 받고 있는 고객의 서비스 완료 시각. 초기값은 Inf
  A = D = numeric() # 출력변수 : A[i]와 D[i]는 i번째 고객의 도착시각과 departure time
  repeat {
    if (tA <= tD & tA <= T) {
      t = tA; NofA = NofA + 1; n = n + 1
      tA = t + rexp(1, rate)
      if (n == 1) tD = t + rgamma (n = 1, shape, scale = scale)
      A[NofA] = t
    }
    if (tD < tA & tD <= T) {
      t = tD; n = n - 1; NofD = NofD + 1
      if (n == 0) tD = Inf else tD = t + rgamma (n = 1, shape, scale = scale)
      D[NofD] = t
    }
    if (min(tA, tD) > T & n > 0) {
      t = tD; n = n - 1; NofD = NofD + 1
      if (n > 0) tD = t + rgamma(n = 1, shape, scale = scale)
      D[NofD] = t
    }
    if (min(tA, tD) > T & n == 0) break
  } # end of repeat
  time.spend[i] = mean(D-A); time.over[i] = max(t - T, 0)
} # end of for (i in (1:n.sim))
cat('고객 한명이 시스템에 체류하는 시간의 평균 =', mean(time.spend))

# Exercise 6.2 the amount of idle time the server would experience in a day
n.sim = 1000
rate = 10
shape = 3; scale = 1/40
T = 9
time.over = time.spend = numeric(n.sim)
time.idle = numeric(n.sim)
for (i in (1:n.sim)) {
  t = NofA = NofD = t.idle = 0
  n = 0
  tA = rexp(1,rate)
  tD = Inf
  A = D = numeric()
  repeat {
    if (tA <= tD & tA <= T){
      if (n == 0) time.idle[i] = time.idle[i] + (tA - t) # here, t is eqaul to 0 or tD
      t = tA; NofA = NofA + 1; n = n + 1
      Tt = t + rexp(1,rate); tA = Tt
      if (n == 1) tD = t + rgamma(n=1,shape,scale=scale)
      A[NofA] = t
    }
    if (tD < tA & tD <= T){
      t = tD; n = n - 1; NofD = NofD + 1
      if (n == 0) tD = Inf else tD = t + rgamma(n=1,shape,scale=scale)
      D[NofD] = t
    }
    if (min(tA,tD) > T & n > 0){
      t = tD; n = n - 1; NofD = NofD + 1
      if (n > 0) tD = t + rgamma(n=1,shape,scale=scale)
      D[NofD] = t
    }
    if (min(tA,tD) > T & n == 0){
      if (t < T) time.idle[i] = time.idle[i] + (T - t) # t is the last departure time
      break
    }
  }
  time.spend[i] = mean(D - A); time.over[i] = max(t - T,0)
}
cat(mean(time.idle), "+/-", 2*sd(time.idle)/sqrt(n.sim), "\n")



# Apply the Insurance Risk Model in Section 6.6 to Exercise 6.11

n.sim = 100 # 모의실험 반복횟수
n0 = 1; a0 = 25000; T = 365; c = 11000
lm = 10; nu = 0; mu = 0 # 보험금 청구율, 보험 가입율, 해지율
generate.Y = function() rexp(1, rate = 1/1000) # 청구금액을 생성하는 함수
I = numeric(length = n.sim)
for ( i in 1:n.sim ) {
  t = 0; a = a0; n = n0 # Initialize 이 문제에서 n은 변하지 않음음
  total.rate = nu + n * mu + n * lm
  tE = rexp(1, rate = total.rate) # tE는 사건 발생 시간 간격을 의미 (평균 1 / ((n * lambda) + v +(n * mu)) 지수분포)
  repeat {
    if (tE > T) {I[i] = 1; break}
    if (tE <= T) {
      a = a + n * c * (tE - t)
      t = tE
      J = sample( 1:3, 1, prob = c(nu, n * mu, n * lm))
      if ( J == 1 ) n = n + 1
      if ( J == 2 ) n = n - 1
      if ( J == 3 ) { Y = generate.Y(); if ( Y > a ) {I[i] = 0; break} else a = a - Y}
      tE = t + rexp(1, rate = total.rate)
    }
  } # end of repeat
}  # end of for
c( mean(I), 2 * sd(I) / sqrt(n.sim) ) # 자본금이 바닥나지 않을 확률의 추정값 출력. 추정값은 얼마나 정확한가 ? ( 구간추정 같이 해주기 )

# Exercise 6.12 (시각 T 이전에 자본금이 음이 되었다는 사실을 알 때, 자본금이 음이 된 정확한 시각과, 부족 금액의 분포를 추정)
n.sim = 100 # 모의실험 반복횟수
n0 = 1; a0 = 25000; T = 365; c = 11000
lm = 10; nu = 0; mu = 0 # 보험금 청구율, 보험 가입율, 해지율
generate.Y = function() rexp(1, rate = 1/1000) # 청구금액을 생성하는 함수수
I = t.broke = amount.broke = numeric(length = n.sim) # t.broke와 amount.broke는 시간과 자본금이 음이 된 것을 의미
for (i in 1:n.sim) {
  t = 0; a = a0; n = n0 # Intialize
  total.rate = nu + n * mu + n * lm; tE = rexp(1, rate = total.rate)
  repeat {
    if (tE > T) { I[i] = 1; break}
    if (tE <= T) {
      a = a + n*c*(tE - t); t = tE
      J = sample(1:3, 1, prob = c(nu, n*mu, n *lm))
      if (J == 1) n = n + 1
      if (J == 2) n = n - 1
      if (J == 3) {
        Y = generate.Y()
        {if (Y > a) {I[i] = 0; t.broke[i] = t; amount.broke[i] = Y- a; break; } #청구금액이 자본금을 넘는 경우 추가
          else a = a - Y} # end of {if (Y > a)}
      } # end of (J == 3)
      tE = t + rexp(1, rate = total.rate)
    } #end of if (tE <= T)
  } # end of repeat
} # end of for

lst = list (time = t.broke [I == 0], amount = amount.broke[I == 0]) # 바닥이 난 경우만 따로 뽑아서 계산을 해야함...
c(mean(lst$time), sd(lst$time)/sqrt(length(lst$time)))
c(mean(lst$amount), sd(lst$amount)/sqrt(length(lst$amount)))

# Exercise 6.17 for문 사용
n.sim = 1000
N = 20; K = 100; S.zero = 100; mu = -0.05; sigma = 0.3
alp = mu + sigma^2 / 2; alp
E = numeric(length = n.sim)
for (i in (1:n.sim)) {
  P = numeric(length = N + 1) # to avoid P[0], we define P[1:(N+1)], not P[0:N]. because R's index start from 1
  P[N+1] = S.zero
  m = N-1
  flag = FALSE # option을 행사할 수 있는 조건이 만족되면 TRUE, 아니면 FALSE
  repeat {
    m.plus = m + 1
    P[m.plus] = P[m.plus + 1] * exp(rnorm(1, mu, sigma)) 
    if (P[m.plus] > K) flag = TRUE # check condition (i)
    if (flag & m > 0) { # check condition (ii), condition (ii)는 m이 0일 때 점검하지 않음
      b = ( (1:m) * mu - log(K/P[m.plus])) / sqrt(sigma * sqrt(1:m)) # check condition (ii)
      op = P[m.plus] * exp((1:m) * alp) * pnorm(sigma * sqrt(1:m) + b) - K * pnorm(b)
      flag = all(P[m.plus] > K + op) # 1부터 m까지 모두 만족되어야 함
    }
    if (flag) break else m = m-1 # 조건 (i)와 (ii)가 모두 만족되면 실행 완료
    if (m < 0) break
  }
  if (flag) E[i] = P[m.plus] - K else E[i] = 0
}

c(mean(E), sd(E)/ sqrt(n.sim), mean(E>0)) # c(기대이익, 표준오차, 옵션행사확률)

# Exercise 6.17 - for 사용 x, 함수화하여 replicate

set.seed(1234)

call_option1 = function(N = 20, K = 100, S.zero = 100, mu = -0.05, sigma = 0.3) {
  
  alpha = mu + sigma^2 / 2
  P = numeric(length = N + 1) # to avoid P[0], we define P[1:(N+1)], not P[0:N]
  P[N+1] = S.zero
  m = N - 1 
  flag = FALSE # 옵션을 행사할 수 있는 조건이 만족되면 TRUE, 아니면 FALSE
  repeat {
    m.plus = m + 1
    P[m.plus] = P[m.plus + 1] * exp(rnorm(1, mu, sigma))
    if (P[m.plus] > K) flag = TRUE # check condition (i)
    if (flag & m > 0) { # check condition (ii), 조건 (ii)는 m이 0일 떄 점검하지 않음
      b = ((1:m) * mu - log(K / P[m.plus])) / (sigma * sqrt(1:m)) # check condition (ii)
      op = P[m.plus] * exp ((1:m) * alpha) * pnorm(sigma * sqrt(1:m) + b) - K * pnorm(b)
      flag = all(P[m.plus] > K + op) # 1부터 m까지 모두 만족되어야 함 
    }
    if (flag) break else m = m - 1 # 조건 (i), (ii)가 모두 만족되면 실행 완료
    if (m < 0) break # 만기가 지났으면 실행 완료  
  }
  if (flag) E = P[m.plus] - K else E = 0
} 

# Exercise 6.17 숙제 (만기까지 기다렸다가 행사하는 전략(최적전략)의 기대이익 구하기)

call_option2 = function(N = 20, K = 100, S.zero = 100, mu = -0.05, sigma = 0.3){
  for (i in 1:N+1){
    St = S.zero * exp(rnorm(1, mu, sigma))
    S.zero = St
  }
  if(St > K) E = St - K else E = 0
  return(E)
}
-0.05 + 0.3^2 * 0.5

n.sim = 10000

# alpha -0.005로 alpha < 0인 경우

E1 = replicate(n.sim, call_option1())

c(mean(E1), sd(E1) / sqrt(n.sim), mean(E1 > 0)) # 기대이익, 표준오차, 옵션행사확률을 추정하여 출력

E2 = replicate(n.sim, call_option2())

c(mean(E2), sd(E2) / sqrt(n.sim), mean(E2 > 0)) # 기대이익, 표준오차, 옵션행사확률을 추정하여 출력

## alpha 0.05로 alpha > 0인 경우 

E1 = replicate(n.sim, call_option1(mu = 0.05, sigma = 0.3))

c(mean(E1), sd(E1) / sqrt(n.sim), mean(E1 > 0)) # 기대이익, 표준오차, 옵션행사확률을 추정하여 출력

E2 = replicate(n.sim, call_option2(mu = 0.05, sigma = 0.3))

c(mean(E2), sd(E2) / sqrt(n.sim), mean(E2 > 0)) # 기대이익, 표준오차, 옵션행사확률을 추정하여 출력

# browser()를 이용한 debugging

a = 0
for (i in 1:3) {
  for (j in 1:3) {
    a = i + j
    #    browser()
  }
}

# Exercise 6.17 HOMEWORK

set.seed(12)

n.sim = 10000

N = 20; K = 100; S.zero = 100; mu = -0.05; sigma = 0.3; alp = mu + 0.5 * sigma^2
E1 = E2 = numeric(length = n.sim)
for (i in (1:n.sim) ) {
  S = S.zero * exp(cumsum(rnorm(N, mu, sigma)))
  E2[i] = max(S[N] - K, 0)
  P = numeric(length = N + 1) 
  P[N + 1] = S.zero
  m = N - 1
  flag = FALSE
  repeat {
    m.plus = m + 1 
    P[m.plus] = S[N - m]
    if (P[m.plus] > K) flag = TRUE
    if (flag & m > 0) {
      b = ((1:m) * mu - log(K / P[m.plus]))/(sigma * sqrt(1:m))
      op = P[m.plus] * exp((1:m) * alp) * pnorm(sigma * sqrt(1:m) + b) - K * pnorm(b)
      flag = all(P[m.plus] > K + op)
    }
    if (flag) break else m = m - 1
    if (m < 0) break
  }
  if (flag) E1[i] = P[m.plus] - K else E1[i] = 0
}

xbar1 = mean(E1)
LL1 = xbar1 - 2 * sd(E1) / sqrt(n.sim); UL1 = xbar1 + 2 * sd(E1) / sqrt(n.sim)

xbar2 = mean(E2)
LL2 = xbar2 - 2 * sd(E2) / sqrt(n.sim); UL2 = xbar2 + 2 * sd(E2) / sqrt(n.sim)

cat("alpha < 0의 기대이익의 점추정량과 신뢰구간은 다음과 같다. [ 점추정량 :" , xbar1, ",신뢰구간 : (",LL1, ",", UL1, ")]\n")

cat("alpha > 0일 때의 최적전략의 기대이익의 점추정량과 신뢰구간은 다음과 같다. [ 점추정량 :" , xbar2, ",신뢰구간 : (",LL2, ",", UL2, ")]\n")

# 2015년 3번 
#3-1
nsim <- 100

S0 <- 100
a <- 110
mu <- 0
sig <- 0.04
Ta <- numeric()
for(i in 1:nsim) {
  n <- 1
  x <- rnorm(1,mu,sig)
  repeat {
    x <- x+rnorm(1,mu,sig)
    Sn <- S0*exp(x)
    if(abs(Sn-S0)>=a) {
      Ta[i] <- n
      break
    }
    n = n+1
  }
}
Ta
mean(Ta) #E(Ta)
sd(Ta)/sqrt(n) #Ta평균의 표준오차


#3-2
Ta <- numeric()
SS <- list()
for(i in 1:2) {
  S <- numeric()
  n <- 1
  x <- rnorm(1,mu,sig)
  repeat {
    x <- x+rnorm(1,mu,sig)
    S <- c(S,S0*exp(x))
    if(abs(S[n]-S0)>=a) {
      Ta[i] <- n
      break
    }
    n = n+1
  }
  SS[[i]] <- S
}
plot(c(0,max(Ta)), c(1,max(abs(c(S0,SS[[2]])-S0))), type="n")
for (i in seq_along(Ta)) {
  lines(0:Ta[i],abs(c(S0,SS[[i]])-S0),lty=3,col=2+i)
}
abline(h=a, lty=2)

## 2020 중간 1번
option_earning = function(N = 90, S.zero = 100, mu = 0.001, sigma = 0.01, K = 100) {
  S = S.zero; earning = 0; t = 1
  repeat {
    S = S * exp(rnorm(1, mean = mu, sd = sigma))
    if (S > 110) {earning = S - K; break}
    t = t + 1
    if (t > N) {earning = max(S-K, 0); break}
  }
  return(earning)
}

x = replicate(400, option_earning() )
c(mean(x), sd(x)/ sqrt(20))
R
# 2021년 중간 2번
# 2번 
N = 200; S.zero = 100; mu = -0.001; sigma = 0.015; n.sim = 400
earn1 = earn2 = numeric(N)
for (i in 1:n.sim) {
  S = S.zero * exp(cumsum(rnorm(N, mean = mu, sd = sigma)))
  if (max(S) > 110) earn1[i] = S[which(S > 110)[1]] - S.zeroRtR
  else earn1[i] = S[N] - S.zero
  if (max(S) > 105 | min(S) < 90) earn2[i] = S[which(S > 105 | S < 90)[1]] - S.zero
  else earn2[i] = S[N] - S.zero
}

c(mean(earn1), sd(earn1)/n.sim)

c(mean(earn2), sd(earn2)/n.sim)

c(mean(earn2 - earn1), sd(earn2 - earn1) / n.sim)

# 두번 쨰 전략이 수익이 더 큼을, 손실이 더 적음을 알 수 있다.
ㄴㄴ