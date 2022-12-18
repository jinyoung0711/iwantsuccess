#### 2017 ####

n = 20; theta = 5; N = 1000
phat = numeric(N)
for (i in 1:N) {
  y = rexp(n, rate=1/theta)
  phat[i] = ks.test(y, "pexp", 1/mean(y))$p.value
}

c(mean(phat), sd(phat)/sqrt(N))

ks.test(phat, "punif")

boxplot(phat)

# H0가 참일 때 p값은 균일분포를 따름.
# 분포 모수가 알려져 있지 않을 때 ks.test를 사용하면 p값이 과대추정된다.
# H0가 참임에도 불구하고 p값의 평균이 0.5보다 크며, boxplot에서도 균일분포를 따르지 않음을 확인할 수 있다.
# -> 과대추정

n = 20; theta = 5; N = 200; B = 1000
p.sim = numeric(N); Dstar = numeric(B)
for ( i in 1:N ) {
  y = rexp(n, rate=1/theta)
  theta.hat = mean(y)
  Fofy = pexp(sort(y), rate = 1/theta.hat)
  d = max((1:n)/n -Fofy, Fpfy = (0:(n-1))/n)
  for ( j in 1:B ) {
    y.star = rexp(n, rate=1/theta.hat)
    theta.star = mean(y.star)
    Fofystar = pexp(sort(y.star), 1/theta.star)
    Dstar[j] = max((1:n)/n - Fofystar, Fofystar - (0:(n-1))/n) 
  }
  p.sim[i] = mean(Dstar >= d)
}

c(mean(p.sim), sd(p.sim)/sqrt(N))
