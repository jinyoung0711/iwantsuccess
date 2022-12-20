# Example 10d
# Gibbs Sampler : Home Runs by Bonds and Griffey
N = 5000
y = w1 = w2 = numeric(N)
y[1] = (0.02 + 0.1)/2
w1[1] = rexp(1, rate=y[1]); w2[1] = rexp(1, rate=y[1])
for ( t in 2:N ) { # w1, w2 반복생성
  repeat {
    ay = rgamma(1, 3, w1[t-1] + w2[t-1]) # conditional distribution of y
    if (ay > 0.02 & ay < 0.1) { y[t] = ay; break}
  }
  w1[t] = rgamma(1, 26, y[t] + 0.5) # conditional distribution of w1
  w2[t] = rgamma(1, 19, y[t] + 0.5) # conditional distribution of w2
}

# 출발점이 어디냐에 따라 불안정할 수 있음. 
# t가 지날수록 안정 -> 분포가 안정적
plot(w1) # 분포는 안정적. -> 불안정하다는 것은 띠 모양이 형성
plot(w2)

c(25 + 0.5 * mean(w1[ (N-999):N]), 18 + 0.5 * mean(w2[ (N-999):N])) # Estimate; The last 1000 values
c(0.5 * sd(w1[ (N-999):N])/sqrt(1000), 0.5 * sd(w2[ (N-999):N])/sqrt(1000)) # Standard Errors
c(25 + 0.5 * mean(w1[ seq(5, N, 5) ]), 18 + 0.5 * mean(w2[ seq(5, N, 5) ])) # Estimate (5단위)
c(0.5 * sd(w1[ (N-999):N])/sqrt(1000), 0.5 * sd(w2[ (N-999):N])/sqrt(1000)) # Standard Errors

# Metropolis-Hastings_Example
target = function(p) p^(2*nAA + nAa)*(1-p)^(nAa + 2*naa)
N = 5000
nAA=50; nAa=21; naa=29
x = numeric(N)
x[1] = runif(1)
for (i in 1:(N - 1)) {
  y = runif(1)
  alp = target(y)/target(x[i])
  if (runif(1) < alp) x[i + 1] = y else x[i + 1] = x[i]
}
hist(x)


# 실습 10.2절 해답
# target f is beta(2.7,6.3) distribution
a = 2.7; b = 6.3; N = 5000; n.sim = 100
target.f = function(x) x^(a-1)*(1-x)^(b-1)
pval.1 = pval.2 = pval.3 = numeric(n.sim)
for (i in 1:n.sim) { # 반복실험으로 K-S 검정의 p값의 평균을 구하고자 함
  x = numeric(N)
  x[1] = runif(1)
  for (n in 1:(N-1)) {
    y = runif(1)
    if (runif(1)<target.f(y)/target.f(x[n])) x[n+1] = y else x[n+1] = x[n]}
  pval.1[i] = ks.test(jitter(x[1:100]),"pbeta",a,b)$p.value
  pval.2[i] = ks.test(jitter(x[4901:5000]),"pbeta",a,b)$p.value
  pval.3[i] = ks.test(jitter(x[seq(4010,5000,10)]), "pbeta", a, b)$p.value
}
c(mean(pval.1),mean(pval.2),mean(pval.3))

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

# Gibbs Sampler: Example 7.3 in Robert and Casella (2009) 
x = c(91, 504, 557, 609, 693, 727, 803, 857, 929, 970, 1043, 1089, 1195, 1384, 1713)
n = length(x); xbar = mean(x); a = (n-1)/2; b = (n-1)*var(x)/2; tau.sq = var(x); theta.zero = mean(x) 
N = 5000
theta = sigma.sq = numeric(N) 
theta[1] = rnorm(1,mean=theta.zero,sd=sqrt(tau.sq)) 
sigma.sq[1] = 1/rgamma(1,a,b) 

cond.mean = function(s2) s2*theta.zero/(s2+n*tau.sq) + n*tau.sq*xbar/(s2+n*tau.sq)
cond.sd = function(s2) sqrt(s2*tau.sq/(s2 + n*tau.sq)) 
cond.b = function(t) 0.5*sum((x-t)^2) + b
cond.a = 0.5*n + a

for (t in 1:(N-1)) {
  theta[t+1] = rnorm(1,cond.mean(sigma.sq[t]),cond.sd(sigma.sq[t])) 
  sigma.sq[t+1] = 1/rgamma(1,cond.a,cond.b(theta[t+1])) 
}

log(quantile(theta,c(0.05,0.95))) 
log(sqrt((quantile(sigma.sq,c(0.05,0.95)))))
log(quantile(theta[(N-999):N],c(0.05,0.95))) # use only the last 1000 values
log(sqrt((quantile(sigma.sq[(N-999):N],c(0.05,0.95))))) 
log(t.test(x,conf.level=0.9)$conf.int) # Non-Bayesian interval based on t-dist
c(log(sqrt(((n-1)*var(x)/qchisq(0.95,df=n-1)))), 
  log(sqrt(((n-1)*var(x)/qchisq(0.05,df=n-1)))))# Non-Bayesian interval based on chi-squared dist

# Sampling Importance Resampling vs Gibbs Sampler: Exmpl 7.3 in Robert and Casella (2009)
x = c(91, 504, 557, 609, 693, 727, 803, 857, 929, 970, 1043, 1089, 1195, 1384, 1713)
n = length(x); xbar = mean(x); s.sq = var(x); N = 5000
a = 0.5*(n-1); b = (n-1)*var(x)/2; tau.sq = var(x); theta.zero = mean(x)

# SIR algorithm with m=N=5000
theta.prior = rnorm(N,theta.zero,sqrt(tau.sq)) # sampling from prior
sigma.sq.prior = 1/rgamma(N,a,b)
w = numeric(N)
for (i in 1:N) w[i] = prod(dnorm(x,theta.prior[i],sqrt(sigma.sq.prior[i])))
pos = sample(1:N,size=1000,rep=TRUE,prob=w) # resample 1000 pairs of values
theta.sir = theta.prior[pos]
sigma.sq.sir = sigma.sq.prior[pos]
log(quantile(theta.sir,c(0.05,0.95))) # 90% interval for the posterior mean of theta
log(sqrt((quantile(sigma.sq.sir,c(0.05,0.95)))))

# Gibbs sampler algorithm using the last 1000 values
theta = sigma.sq = numeric(N)
theta[1] = rnorm(1,mean=theta.zero,sd=sqrt(tau.sq))
sigma.sq[1] = 1/rgamma(1,a,b)
cond.mean = function(s2) s2*theta.zero/(s2 + n*tau.sq) + n*tau.sq*xbar/(s2 + n*tau.sq)
cond.sd = function(s2) sqrt(s2*tau.sq/(s2 + n*tau.sq))
cond.b = function(t) 0.5*sum((x-t)^2) + b
cond.a = 0.5*n + a
for (t in 1:(N-1)) {
  theta[t+1] = rnorm(1,cond.mean(sigma.sq[t]),cond.sd(sigma.sq[t]))
  sigma.sq[t+1] = 1/rgamma(1,cond.a,cond.b(theta[t+1]))
}
log(quantile(theta[(N-999):N],c(0.05,0.95)))
log(sqrt((quantile(sigma.sq[(N-999):N],c(0.05,0.95)))))
ks.test(theta.sir,theta[seq(5,5000,5)]) # 작지 않은 p값 예상
ks.test(theta.prior[seq(5,5000,5)],theta[seq(5,5000,5)]) # 작은 p값 예상
# Non-Bayesian 90% confidence interval
log(t.test(x,conf.level=0.9)$conf.int) # t-dist used
c(log(sqrt(((n-1)*s.sq/qchisq(0.95,df=n-1)))),
  log(sqrt(((n-1)*s.sq/qchisq(0.05,df=n-1))))) # chi-squared dist used

