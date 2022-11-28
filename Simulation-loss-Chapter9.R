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

# [Exercise 9.1]
y = c(141, 291, 132)
chisq.test(y, p=c(.25, .5, .25))$p.value
chisq.test(y, p=c(.25, .5, .25),
           simulate.p.value=TRUE, B=10000)$p.value

# [Exercise 9.4]
draw.FnF = function(x, F, ...) {
  plot(ecdf(x), main='Fn(x) and F(X)')
  curve(F(x,...), add=T)
}

y = c(164, 142, 110, 153, 103, 52, 174, 88, 178, 184, 58, 62, 132, 128)
draw.FnF(y, punif, min=50, max=200)
ks.test(y, punif, min=50, max=200)
ks.test(y, punif, min=50, max=200, exact=TRUE)$p.value
ks.test(y, punif, min=50, max=200, exact=FALSE)$p.value

# [Sec 9.2]
# 2017 Mid-term Exam : Question [2]
n = 20; theta =5; N =1000
phat = phat.exact = numeric(N)
for (i in 1:N) {
  y = rexp(n, rate=1/theta)
  phat[i] = ks.test(y, "pexp", 1/mean(y), exact = TRUE)$p.value
# phat.exact[i] = ks.test(y, 'pexp', 1/mean(y), exact = TRUE)$p.value : 해당 코드는 없어도 댐 
}

c(mean(phat), sd(phat)/sqrt(N))
ks.test(phat, "punif")
boxplot(phat) # 기각되는 것을 볼 수 이씀. Uniform distribution을 따르려면 0.5기준 0.25 - 0.75

# c(mean(phat.exact), sd(phat.exact)/sqrt(N))
# ks.test(phat.exact, "punif")
# boxplot(phat.exact) # 기각되는 것을 볼 수 이씀. Uniform distribution을 따르려면 0.5기준 0.25 - 0.75
