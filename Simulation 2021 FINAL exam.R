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

## [2-1] ##
N = 20000
X = matrix(0, nrow=N, ncol=3)
X[1, ] = rexp(3)
c = 9
for (t in 1:(N - 1)) {
  X[t + 1, 1] = rexp(1) + max(c - sum(X[t, 2:3]), 0)
  X[t + 1, 2] = rexp(1) + max(c - sum(X[t + 1, 1], X[t, 3]), 0)
  X[t + 1, 3] = rexp(1) + max(c - sum(X[t + 1, 1:2]), 0)
}
minx = apply(X[seq(101, N, 10), ], 1, min)
c(mean(minx), sd(minx)/sqrt((N - 100)/10))

## [3-1] : Markov Chain ## -> 이길 확률과 질 확률이 같음. 순서에 따라 내기 확률이 다르다면 어떻게 코드 짜야할까 ? 변형 해보기 

problem3 = function(N, M, T) {
 x = rep(M, N) # Money N개 생성
 sel = 1:N # 내기에 참여하는 사람의 index
 for (i in 1:T) {
   ij = sample(sel, 2) # 내기에 참여하는 두명 랜덤추출
   i = ij[1]; j = ij[2] 
   if (x[i]*x[j] > 0) {x[i] = x[i] + 1; x[j] = x[j] - 1} else { # x[i]*x[j] > 0 : 두 사람 모두 재산이 0이 아님 -> 내기 진행 가능 
     if (x[i] == 0) sel = sel[-match(i, sel)] # i번째 사람이 금액이 0원이 된다면 제거
     if (x[j] == 0) sel = sel[-match(j, sel)] # j번째 사람이 금액이 0원이 된다면 제거
   }
   if (length(sel) <= 1) break # 재산이 0이 아닌 사람이 한명만 남았다면 프로그램 종료
 }
 return(x)
}

## [3-2] ## 
problem3(N = 20, M = 10, T = 1000)
problem3(N = 20, M = 10, T = 10000)

## [3-3] ## 
# N과 M이 고정되고 T가 커지면 N명의 재산 분포는 한사람에게 쏠릴 것이다.(Markov chain의 absorbing state 참조)
problem3(N = 20, M = 10, T = 100000) # 모의실험 결과로 확인 가능
