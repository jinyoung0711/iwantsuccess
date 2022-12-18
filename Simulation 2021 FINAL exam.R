## [1] ##

norm.pval = function(x, B = 400) {
  n = length(x)
  mu.hat = mean(x); sig.hat = sd(x)
  Fofx = pnorm(sort(x), mean = mu.hat, sd = sig.hat)
  d = max(1:n/n - Fofx, Fofx - 0:(n-1)/n)
  Dstar = numeric(B)
  for (i in 1:B) {
    x.star = rnorm(n, mean = mu.hat, sd = sig.hat)
    mu.star = mean(x.star); sig.star = sd(x.star)
    Fofxstar = pnorm(sort(x.star), mean = mu.star, sd = sig.star)
    Dstar[i] = max(1:n/n - Fofxstar, Fofxstar - 0:(n-1)/n)
  }
  return(mean(Dstar > d))
}

mu = 2; sig = 1; n = 20; n.sim = 100

phat = phat.ks = phat.shapiro = numeric(n.sim)

for (i in 1:n.sim) {
  x = rnorm(n, mean = mu, sd = sig)
  phat[i] = ks.test(x, "pnorm", mean = mean(x), sd = sd(x))$p.value # (1) 잘못 가정했을 때의 p값
  phat.ks[i] = norm.pval(x)
  phat.shapiro[i] = shapiro.test(x)$p.value
}

c(ks.test(phat, "punif")$p.value, 
  ks.test(phat.ks, "punif")$p.value, 
  ks.test(phat.shapiro, "punif")$p.value)

oldpar = par(mfrow=c(1,3))
boxplot(phat); boxplot(phat.ks); boxplot(phat.shapiro)

## [2] ##

## [3] ##

problem3 = function(N, M, T) {
 
}