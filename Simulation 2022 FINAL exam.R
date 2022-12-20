## 1번 ##

exp.pval = function(n, B) {
  x = rexp(n, rate=1)
  d = ks.test(x, 'pexp', rate=1/mean(x))$statistic
  xbar = mean(x)
  DNstar = numeric(B)
  for (i in 1:B) {
    xstar = rexp(n, rate = 1/xbar)
    DNstar[i] = ks.test(xstar, 'pexp', rate = 1/mean(xstar))$statistic
  }
  mean(DNstar >= d)
}  

## [1-2] ##
rate = 1; n = 30; n.sim = 100
phat.ks = numeric(n.sim)
for ( i in 1:n.sim) {
  phat.ks[i] = exp.pval(30, 100)  
}

c(mean(phat.ks), sd(phat.ks)/sqrt(n.sim))
boxplot(phat.ks)

## [2-1] ##
S.zero = 100; mu = 0.001; sig = 0.008
n.sim = 1000
Imp = numeric(n.sim)
for (i in 1:n.sim) {
  xvec = rnorm(90, mean = -mu, sd = sig)
  S = S.zero * exp(cumsum(xvec)) 
  if (min(S) < 80) Imp[i] = exp(2 * mu * sum(xvec)/sig^2) else Imp[i] = 0
}
c(mean(Imp), sd(Imp)/sqrt(n.sim))

## [2-2] ##

S.zero = 100; a = 80; mu = 0.001; sig = 0.008
n.sim = 1000
money = numeric(n.sim) # 금액

for (i in 1:n.sim) {
  n = 1 
  S1 = S.zero
  repeat {
    S2 = S1 * exp(rnorm(1, mean = mu, sd = sig))
    if (S2 < a) money[i] = 0; break
    S1 = S2; n = n + 1
    if (n == 90) money[i] = 105
  }
}

exotic = function(v = 100, t=90, K=105, s=30, b=80,
                  mu=0.001, sigma=0.008) {
  Pofs = v*exp(rnorm(1, s*mu, sqrt(s*sigma^2)))
  Poft = Pofs*exp(rnorm(1, (t-s)*mu, sqrt((t-2)*sigma^2)))
  if (Pofs > v & Poft > K) R = Poft - K else R = 0
  return(R)
}

n.sim = 1000
R.vec = replicate(n.sim, exotic())
c(mean(R.vec), sd(R.vec/sqrt(n.sim)))


## 3번 ##

N = 20000
X = matrix(0, nrow=N, ncol=3)
X[1, ] = rexp(3)
c = 15
for (t in 1:(N - 1)) {
  X[t + 1, 1] = rexp(1) + max(c - sum(X[t, 2:3]), 0)
  X[t + 1, 2] = rexp(1) + max(c - sum(X[t + 1, 1], X[t, 3]), 0)
  X[t + 1, 3] = rexp(1) + max(c - sum(X[t + 1, 1:2]), 0)
}
minx = apply(X[seq(101, N, 10), ], 1, min)
c(mean(minx), sd(minx)/sqrt((N - 100)/10))
