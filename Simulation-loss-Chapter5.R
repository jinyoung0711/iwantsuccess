# Chapter 5 Generating Continuous Random Variables

# beta(2,4) 난수를 rejection method로 생성
n = 1000; x = numeric(n)
for ( i in 1:n ) {
  repeat {
    u1 = runif(1); u2 = runif(1)
    if (u2 < (256/27) * u1 * (1 - u1)^3) { x[i] = u1; break}
  }
}

# Generating a Poisson process ( use property 2 )
lambda = 1
T = 10; t = 0 ; I = 0L
S = numeric()
repeat {
  t = t + rexp(1, lambda)
  if (t > T) break
  I = I + 1; S[I] = t
}

cat("Total number of events is ", I, "\nArrival times are \n", signif(S,3), "\n")

# Generating a Poisson process ( use property 1 and 3 )
lambda = 1; T = 10
n = rpois(1, lambda * T)
S = sort(T * runif(n))
cat("Total number of events is ", n, "\nArrival times are \n", signif(S,3), "\n")

# Poisson process function (1)

hpp = function (lambda, t0 = 0, T) {
  t = t0 ; I = 0L
  S = numeric()
  repeat {
    t = t + (-log(runif(1)))/lambda # t = t + rexp(1, lambda)
    if (t > T) break
    I = I + 1; S[I] = t
  }
  list(n_events = I, arrival_times = S)
}

hpp()

# Thinning algorithm function
thin <- function( max_lambda, t0 = 0, T, 
                  lt = function(t) {} ) {
  t = t0; I = 0L
  S = numeric()
  repeat {
    t = t + (-log(runif(1))/max_lambda)
    if (t > T) break
    if (runif(1) < lt(t)/max_lambda) {
      I = I + 1 ; S[I] = t}
  }
  list(n_events = I, arrival_times = S)
}

thin()

# Exercise 5.25 - (a)
thin( max_lambda = 7, T = 10,
      lt = function(t) 3 + 4/(t+1))


# Exercise 5.25 - (b)
( output1 <- hpp ( lambda = 3, T = 10) )
( output2 <- thin ( max_lambda = 4, lt = function(t) 4/(t+1)) )
n_events = output1$n_events + output2$n_events
n_events

S = sort(c(output1$arrival_times, output2$arrival_times))
S

# Exercise 5.26
output1 <- thin ( max_lambda = 1, t0 = 0, T = 5,
                  lt = function(t) t/5)
output2 <- thin ( max_lambda = 26, t0 = 5, T = 10,
                  lt = function(t) 1 + 5 * t(t - 5))

n_events = output1$n_events + output2$n_events
n_events

S = c(output1$arrival_times, output2$arrival_times)
S
# 연속형분포중에 무기억성 분포는 지수분포
# 이산형분포중에 무기억성 분포는 기하분포


# Exercise 5.24

n = 1000
X = numeric(n)
for ( i in 1:n) {
  N = rpois(1,5)
  Y = sample(20:40, N, rep = TRUE)
  X[i] = sum(Y)
}

cat(mean(X), "+-", 2*sd(X)/sqrt(n), '\n')

# 2015년 중간 1번
## 1번
n.sim = 1000
Y = numeric(length = n.sim)
n = 10
X = numeric(n)
for (i in 1:n.sim){
  X = rbeta(n = 10, shape1 = 2, shape2 = 3)
  Y[i] = sum(X)
}

# 2020년 중간 3번
a1 = thin(max_lambda = 20, t0 = 0, T = 4, function(t) 5 * t)
a2 = hpp(20, t0 = 4, T = 6)
a3 = thin(max_lambda = 20, t0 = 6, T = 10, function(t) 50 - 5 * t)
people = a1$n_events + a2$n_events + a3$n_events
people
n.sim = 400
day_sell = replicate(n.sim, sum(runif(rpois(1, lambda = people), min = 4, max = 20)))
c(mean(day_sell), sd(day_sell)/20, mean(day_sell > 1500))  # 균일분포는 U(4, 20)

# Another 

out1 = thin(max_lambda = 20, t0=0, T=4, lt=function(t) {5*t})
out2 = hpp(lambda = 20, t0=4, T=6)
out3 = thin(max_lambda = 20, t0=6, T=10, lt = function(t) {50-5*t})

n_events = out1$n_events + out2$n_events + out3$n_events

profit = replicate(400, sum(runif(n_events, min = 4, max = 20)))

c(mean(profit),sd(profit)/sqrt(400),mean(profit >= 1500))