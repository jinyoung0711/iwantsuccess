#### [Sec9.1 : Goodness-of-Fit Tests]

# Example 9b and more
draw.FnF = function(x, F, ...) { # draw the ecdf Fn and the assumed df F
  plot(ecdf(x), main = 'Fn(x) and F(x)')
  curve(F(x, ...), add = T)
}

y = c(66, 72, 81, 94, 112, 116, 124, 140, 145, 155)
draw.FnF(y, pexp, rate=0.01)
ks.test(y, 'pexp', rate=0.01)
y = rexp(10, rate=0.01) # H0 : (F는 평균 100인 지수분포)가 참일 때 그림과 검정결과
draw.FnF(y, pexp, rate=0.01)
ks.test(y, 'pexp', rate=0.01)

# [Exercise 9.1 - (a)]
y = c(141, 291, 132)
chisq.test(y, p=c(.25, .5, .25))$p.value

# [Exercise 9.1 - (b)]
chisq.test(y, p=c(.25, .5, .25),
           simulate.p.value=TRUE, B=10000)$p.value

# [Exercise 9,2 - (a)]
y = c(158, 172, 164, 181, 160, 165)
chisq.test(y, p = rep(1/6, 6))$p.value

# [Exercise 9.2 - (b)]
chisq.test(y, p = rep(1/6, 6),
           simulate.p.value = TRUE, B = 10000)$p.value

# [Example 9c]
n = 30
N = c(6, 2, 1, 9, 7, 5) # 하루 사고 건수 5건과 8건을 묶어서 한 범주로 만들었음에 주의
m = (sum(N[1:5] * 0:4) + 4*5 + 1*8) # m means Y_bar 
phat = dpois(0:4, m); phat[6] = 1 - ppois(4, m)
chisq.test(N, p = phat) # 자유도에 주의, 잘못된 자유도 떄문에 적절하지 않은 p값
# 추정된 모수의 수만큼 조정된 자유도로 다음과 같이 p값을 계산해야함.
t = sum((N - n*phat)^2/(n*phat))
1 - pchisq(5, 4) # n이 클 때의 근사적 p값

# [Exercise 9.4]
draw.FnF = function(x, F, ...) {
  plot(ecdf(x), main='Fn(x) and F(X)')
  curve(F(x,...), add=T)
}

y = c(164, 142, 110, 153, 103, 52, 174, 88, 178, 184, 58, 62, 132, 128)
draw.FnF(y, punif, min=50, max=200)
ks.test(y, punif, min=50, max=200)
ks.test(y, punif, min=50, max=200, exact=TRUE)$p.value # exact arg 물어보기 : 표본크기가 작을 때 쓸수 있는 방법
ks.test(y, punif, min=50, max=200, exact=FALSE)$p.value

#### [Sec 9.2 : Goodness-of-Fit Tests When Some Parameters Are Unspecified] ####
# 2017 Mid-term Exam : Question [2]
n = 20; theta = 5; N = 1000
phat = numeric(N)
for (i in 1:N) {
  y = rexp(n, rate=1/theta)
  phat[i] = ks.test(y, "pexp", 1/mean(y), exact = TRUE)$p.value # 표본평균을 사용 
# phat.exact[i] = ks.test(y, 'pexp', 1/mean(y), exact = TRUE)$p.value : 해당 코드는 없어도 댐 
}

c(mean(phat), sd(phat)/sqrt(N))
ks.test(phat, "punif") # p < 0.05이니까 기각 !!!!!!!
boxplot(phat) # 기각되는 것을 볼 수 있다. Uniform distribution을 따르려면 0.5기준 0.25 - 0.75

# c(mean(phat.exact), sd(phat.exact)/sqrt(N))
# ks.test(phat.exact, "punif")
# boxplot(phat.exact) # 기각되는 것을 볼 수 이씀. Uniform distribution을 따르려면 0.5기준 0.25 - 0.75

# 9.2절 실습문제
n = 20; x = rexp(n, rate=1)

ks.test(x, 'pexp', rate=1/mean(x))$p.value # 맞는게 아님 ... 참 고장률이 이것과 같은 지수분포를 검정하라와 같은 말 
# 귀무가설이 참임을 검정 -> p값의 분포가 U ~ (0,1)을 따름을 보여야함.
# ks.test의 exact 인자는 표본의 크기가 큰가, 작은가를 의미하고, 표본이 크다고 판단되면 R에서 자동적으로 exact = TRUE 적용됨.
# 잘못된 방법 -> pval이 U ~ (0,1) 균일분포를 따르지 않음
f1 = function(n) {
  x = rexp(n, rate=1)
  ks.test(x, 'pexp', rate=1/mean(x))$p.value
}

pval1 = replicate(1000, f1(20))
boxplot(pval1)
c(mean(pval1), sd(pval1)/sqrt(1000))

# [옳은 방법] - check 하기...
f2 = function(n, B) {
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

pval.2 = replicate(100, f2(n=20, B=400))
hist(pval.2)
ks.test(pval.2, "punif") # 동일한 값들이 존재해서 나오는 warning message
c(mean(pval.2), sd(pval.2)/sqrt(100)) # 여기 100 들어가는거 맞나 ??????
boxplot(pval.2)

#### [Sec 9.3 : The Two-Sample Problem] ####

## [Exercise 9.10] ##

x = c(65.2, 67.1, 69.4, 78.4, 74, 80.3)
y = c(59.4, 72.1, 68.0, 66.2, 58.5)
trt = rep(1:2, c(6,5))
plot(c(x, y), trt)
wilcox.test(x, y)$p.value # exact p-value
wilcox.test(x, y, exact = TRUE)$p.value # the same p-value

## [Exercise 9.11] ##
# 9.11 - (a)
wilcox.test(x,y, exact=FALSE)$p.value

# 9.11 - (b)
n = length(x); m = length(y)
W = wilcox.test(x,y)$statistic; r = W + n * (n+1) / 2
# 또는 xy = c(x,y); r = sum(match(x, sort(xy)))
N = 1000 # 모의실험으로 p값을 구하기 위한 반복횟수
R = replicate(N, sum(sample(1:(n+m), n))) # for문을 사용해도 됨
2*min(mean(R <= r), mean(R >= r)) # 모의실험에 의한 p값

# 9.11 - (b)
sum(rank(c(x,y))[trt == 1]) # this is r, observed rank sum of X's
wilcox.test(x, y)$statistic + 6*7/2 # the same
# 나머지는 강의노트 참고