# Example 10d

N = 5000
y = w1 = w2 = numeric(N)
y[1] = (0.02 + 0.1)/2
w1[1] = rexp(1, rate=y[1]); w2[1] = rexp(1, rate=y[1])
for ( t in 2:N ) {
  repeat {
    ay = rgamma(1, 3, w1[t-1] + w2[t-1])
    if (ay > 0.02 & ay < 0.1) { y[t] = ay; break}
  }
  w1[t] = rgamma(1, 26, y[t] + 0.5)
  w2[t] = rgamma(1, 19, y[t] + 0.5)
}

# 출발점이 어디냐에 따라 불안정할 수 있음. 
# t가 지날수록 안정 -> 분포가 안정적
plot(w1) # 분포는 안정적. -> 불안정하다는 것은 띠 모양이 형성
plot(w2)

# 이거 코드 추가하기 c( ) ~

# Gibbs Sampler : Home Runs by Bonds and Griffey
N = 5000
y = w1 = w2 = numeric(N)
y[1] = (0.02 + 0.1)/2
w1[1] = rexp(1, rate = y[1]; w2[1] = rexp(1, rate=y[1]))


# [실습 10.3] 
# 이정도 문제까지는 나올 수 있다고 하심.. 유도 과정은 빡세지만
x = c(91, 504, 557, 609, 693, 727, 803, 857, 929, 970, 1043, 1089, 1195, 1384, 1713) 
n = length(x); xbar = mean(x); s.sq = var(x)
a = 0.5*( n-1 ); b = (n-1) * var(x)/2; tau.sq = var(x); theta.zero = mean(x)
N = 5000

# [실습 10.3 - a]
# SIR Algorithm
theta.prior = rnorm(N, theta.zero, sqrt(tau.sq)) # sampling from prior
sigma.sq.prior = 1/rgamma(N, a, b)
w = numeric(N)
for (i in 1:N) w[i] = prod(dnorm(x, theta.prior[i], sqrt(sigma.sq.prior[i])))
pos = sample(1:N, size=1000, rep=TRUE, prob=w) # resample 1000 values
theta.sir = theta.prior[pos]
sigma.sq.sir = sigma.sq.prior[pos]
log(quantile(theta.sir, c(0.05, 0.95))) # 90% interval for the posterior mean of theta

# Gibbs Sample Algorithm


# Gibbs Sampler: Example 7.3 in Robert and Casella (2009) x = c(91, 504, 557, 609, 693, 727, 803, 857, 929, 970, 1043, 1089, 1195, 1384, 1713)n = length(x); xbar = mean(x) a = (n-1)/2; b = (n-1)*var(x)/2; tau.sq = var(x); theta.zero = mean(x) N = 5000
theta = sigma.sq = numeric(N) theta[1] = rnorm(1,mean=theta.zero,sd=sqrt(tau.sq)) sigma.sq[1] = 1/rgamma(1,a,b) cond.mean = function(s2) s2*theta.zero/(s2+n*tau.sq) + n*tau.sq*xbar/(s2+n*tau.sq)cond.sd = function(s2) sqrt(s2*tau.sq/(s2 + n*tau.sq)) cond.b = function(t) 0.5*sum((x-t)^2) + b
cond.a = 0.5*n + a
for (t in 1:(N-1)) {
  theta[t+1] = rnorm(1,cond.mean(sigma.sq[t]),cond.sd(sigma.sq[t])) sigma.sq[t+1] = 1/rgamma(1,cond.a,cond.b(theta[t+1])) }
log(quantile(theta,c(0.05,0.95))) log(sqrt((quantile(sigma.sq,c(0.05,0.95)))))
log(quantile(theta[(N-999):N],c(0.05,0.95))) # use only the last 1000 valueslog(sqrt((quantile(sigma.sq[(N-999):N],c(0.05,0.95))))) log(t.test(x,conf.level=0.9)$conf.int) # Non-Bayesian interval based on t-dist
c(log(sqrt(((n-1)*var(x)/qchisq(0.95,df=n-1)))), log(sqrt(((n-1)*var(x)/qchisq(0.05,df=n-1)))))# Non-Bayesian interval based on chi-squared dist