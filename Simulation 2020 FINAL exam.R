#### 2020 ####

## [1-1] ##
S.zero = 100; a = 10; mu = 0.001; sig = 0.008
n.sim = 100
Ta = numeric(n.sim)

for (i in 1:n.sim) {
  n = 1
  S1 = S.zero
  repeat {
    S2 = S1 * exp(rnorm(1, mean = mu, sd = sig))
    if (abs(S2 - S.zero) >= a) break
    S1 = S2; n = n + 1
  }
  Ta[i] = n
}

c(mean(Ta), sd(Ta)/sqrt(n.sim))

## [1-2] ##
S.zero = 100; mu=0.001; sig=0.008
n.sim = 1000
Imp = numeric(n.sim)
for (i in 1:n.sim) {
  xvec = rnorm(90, mean = -mu, sd = sig)
  S = S0*exp(cumsum(xvec)) 
  if (min(S) < 80) Imp[i] = exp(2 * mu * sum(xvec)/sig^2) else Imp[i] = 0
}
c(mean(Imp), sd(Imp)/sqrt(n.sim))

## [2] ##
c = 10; N = 2000
x = matrix(nrow=N, ncol=2)
x[1, ] = runif(2, 0, 10)
trunc.exp = function(rate,c) {
  repeat {
    y = rexp(1,rate)
    if (y < c) return(y)
  }
}
for (i in 2:N) {
  x[i, 1] = trunc.exp(x[i - 1, 2], c)
  x[i, 2] = trunc.exp(x[i, 1], c)
}
par(mfrow = c(1,2)); plot(x[, 1]); plot(x[, 2])


## [3-1] : 9.2절과 관련 ##
# 모수 α가 표본에서 추정되므로, 한 개의 p값을 구하기 위해 반복실험할 때 추출된 표본에서 모수를 다시 추정해야 한다. 

pareto_pval = function(x, B = 200) {
  n = length(x)
  alpha.hat = mean(x)/(mean(x) - 1)
  Fofx = 1 - (sort(x))^(-alpha.hat)
  d = max(1:n/n - Fofx, Fofx - 0:(n-1)/n)
  Dstar = numeric(B)
  for (i in 1:B) {
    x.star = (1 - runif(n))^(-1/alpha.hat)
    alpha.star = mean(x.star)/(mean(x.star) - 1)
    Fofxstar = 1 - (sort(x.star))^(-alpha.star)
    Dstar[i] = max(1:n/n - Fofxstar, Fofxstar - 0:(n-1)/n)
  } 
  return(mean(Dstar > d))
}

## [3-2] ##

n = 30; alpha = 3; n.sim = 100
pval = numeric(n.sim)
for (i in 1:n.sim) {
  x = (1 - runif(n))^(-1/alpha)
  pval[i] = pareto_pval(x)
}

ks.test(pval, "punif")
boxplot(pval)
c(mean(pval), sd(pval)/sqrt(n))
# H0를 기각하지 못하기 때문에 pval은 uniform 분포를 따름.