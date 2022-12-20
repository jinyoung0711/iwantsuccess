#### [Exercise 11.32] ####
getwd()
setwd('C:/Users/82106/22_다변량분석_데이터')
hemo = read.table("T11-8.DAT")
names(hemo) = c("group", "x1", "x2")

# (b)
library(MASS)
obj = lda(group ~ x1 + x2, data = hemo, prior = c(.5, .5), cv = TRUE) # cv = TRUE 지정시 class 생김
obj
conf.mat = table(hemo$group, obj$class)

# (c)

obj = lda(group ~ x1 + x2, data = hemo, prior = c(.5, .5)) 
# 새로운 관측값을 예측할때는 cv = TRUE 지정 X 
# 전체 자료를 다 이용해서 만든 분류 규칙을 이용해서 예측해야함.
newdata = data.frame()
names(newdata) = c("x1", "x2")
predict(lda.obj.newdata)

#### [Example 11.11] ####
library(MASS)
admission = read.table("T11-6.DAT")
names(admission) = c("gpa", "gmat", "admit")
plot(admission[, c("gpa", "gmat")], col = admission$admit)

# 세 모집단의 모평균이 같은가를 검정
attach(admission)
summary(manova(cbind(gpa, gmat) ~ admit)); detach()
# 세 모집단이 정규분포를 따르고 같은 공분산행렬을 갖는다고 가정
(obj = lda(admit ~ gpa + gmat, data =admission, prior = c(1/3, 1/3, 1/3)))
# 교차타당성 방법으로 오분류율 추정  CV error estimate
obj = lda(admit ~ gpa + gmat, data = admission, prior = c(1/3, 1/3, 1/3), CV = TRUE)
table(admission$admit, obj$class)

#### [Example 11.12] ####
pairs(iris[ , 1:4], col=iris$Species)
library(MASS)
(obj = lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris))
obj.cv = lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris, CV=T)
table(iris$Species, obj.cv$class)
obj.cv = qda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris, CV=T)
table(iris$Species,obj.cv$class)
newdata = data.frame(5.8, 3.1, 3.8, 1.2); names(newdata) <- names(iris)[1:4]
predict(obj,newdata)

# Example 11.14 - Figure 11.12 그리기
getwd()
setwd("C:/Users/82106/22_다변량분석_데이터")

library(MASS)
crude = read.table("T11-7.DAT")
crude[,2] = sqrt(crude[,2]); crude[,3] = sqrt(crude[,3]); crude[,4] = 1/crude[,4]
names(crude) = c("X1","X2","X3","X4","X5","zone")
crude$zone = factor(crude$zone,levels=c("Wilhelm","SubMuli","Upper")) # 수준 순서를 변경
pairs(crude[,1:5],col=crude$zone)
table(crude$zone)

# actual error rate

(obj.cv = lda(zone ~ X1 + X2 + X3 + X4 + X5,data=crude, CV = T)) # What is the default prior?
mat = table(crude$zone, obj.cv$class)
(sum(mat) - sum(diag(mat)))/sum(mat)

# 
obj = lda(zone ~ X1 + X2 + X3 + X4 + X5,data=crude)
(l1 = obj$scaling[,1]); (l2 = obj$scaling[,2])
X.mat = as.matrix(crude[,1:5])
X.cen = scale(X.mat,center=T,scale=F) # 교재에서 변수들을 중심화 했음.
LD1 = X.cen %*% l1; LD2 = X.cen %*% (-l2) # 교재에 있는 값과 일치시키기 위해 LD2의 부호를 바꿈
plot(LD1,LD2,col=crude$zone,pch=(as.numeric(crude$zone)+14),main="Figure 11.2") 

# [Example 11.24]
# Exercise 11.24: Bankruptcy Data
bank = read.table("T11-4.DAT")
names(bank) = c("x1","x2","x3","x4","y")
bank$y = factor(bank$y,labels=c("bankrupt","sound"))
# (a)
pairs(bank, lower.panel=panel.smooth, upper.panel=panel.cor,col=bank$y)
plot(bank$x1,bank$x2,col=bank$y)

# (b)
colMeans(bank[bank$y=="bankrupt",c(1,2)]); colMeans(bank[bank$y=="sound",c(1,2)])
cov(bank[bank$y=="bankrupt",c(1,2)]); cov(bank[bank$y=="sound",c(1,2)])

# (c)
library(MASS)
p1 = p2 = 0.5 # 사전확률이 같다고 가정 후 qda 실시
obj.qda = qda(y~x1+x2,data=bank,prior=c(p1,p2))
# for the meaning of obj.qda$scaling, cf. equation (12.3) in the book ‘MASS’.

# (d)
(cm = table(bank$y,predict(obj.qda)$class)) # Confusion Matrix for Apparent Error Rate
p1*cm[1,2]/sum(cm[1,]) + p2*cm[2,1]/sum(cm[2,]) # Apparent Error rate 돋보기 오분류율. (써서는 안되는 값)
(cm = table(bank$y,qda(y~x1+x2,data=bank,prior=c(0.5,0.5),CV=T)$class)) # for CV Error Rate
# 훈련용이랑 모델용이랑 나눠서 적합
p1*cm[1,2]/sum(cm[1,]) + p2*cm[2,1]/sum(cm[2,])

# (e) : 사전확률 값을 바꿨을 때 CONFISUION MATRIX 어떻게 변하는지 해석해보기,,(수업시간에 함)
# bankrupt : bankrupt 줄어들고 bankrupt : sound는 늘어남.
# sound : bankrupt은 줄어들고, sound : sound는 늘어남.
p1=0.05; p2=0.95
obj.qda = qda(y~x1+x2,data=bank,prior=c(p1,p2))
(cm = table(bank$y,predict(obj.qda)$class)) # Confusion Matrix for the Apparent Error Rate
p1*cm[1,2]/sum(cm[1,]) + p2*cm[2,1]/sum(cm[2,]) # 100번중에 3번 오분류
(cm = table(bank$y,qda(y~x1+x2,data=bank,prior=c(0.5,0.5),CV=T)$class)) # for the CV Error Rate
p1*cm[1,2]/sum(cm[1,]) + p2*cm[2,1]/sum(cm[2,])

# 변수의 개수가 많아진다고해서 판별규칙의 성능이 좋아진다는 보장 X 
# (f)
obj.lda = lda(y~x1+x2,data=bank,prior=c(p1,p2))
(cm = table(bank$y,predict(obj.lda)$class))
p1*cm[1,2]/sum(cm[1,]) + p2*cm[2,1]/sum(cm[2,])
(cm = table(bank$y,lda(y~x1+x2,data=bank,prior=c(p1,p2),CV=T)$class))
p1*cm[1,2]/sum(cm[1,]) + p2*cm[2,1]/sum(cm[2,])

# (h)
(cm = table(bank$y,qda(y~x1+x2+x3+x4,data=bank,prior=c(0.5,0.5),CV=T)$class))
p1*cm[1,2]/sum(cm[1,]) + p2*cm[2,1]/sum(cm[2,])
(cm = table(bank$y,qda(y~x1+x2+x3,data=bank,prior=c(0.5,0.5),CV=T)$class))
p1*cm[1,2]/sum(cm[1,]) + p2*cm[2,1]/sum(cm[2,]) # x4 is not so helpful.

# 시험문제를 낸 적도 있음
# Result 11.1
# 오분류비용을 어떻게 하는게 좋은가... 관련해서 C(1|2)와 같은 항들을 무시하면 안됌


hemo = read.table("T11-8.DAT")
names(hemo) = c("group","x1","x2")
pch.vec = (hemo[,1] - 2)*(-15) + 1 # Group 1이 정상 그룹. pch=16에 대응하는 점의 모양이 속이 찬 원.
plot(hemo[,2:3],pch=pch.vec,xlim=c(-0.7,0.4),ylim=c(-0.4,0.4))
(mean1 = c(mean(hemo[hemo$group==1,2]),mean(hemo[hemo$group==1,3]))) # group 1의 표본평균벡터, lda 함수를 써도 됨
points(x=mean1[1],y=mean1[2],pch=4,cex=1.4,col="blue")
(mean2 = c(mean(hemo[hemo$group==2,2]),mean(hemo[hemo$group==2,3])))
points(x=mean2[1],y=mean2[2],pch=4,cex=1.4,col="blue")
points(x=-0.210,y=-0.044,pch=8,cex=1.4,col="red") # 분류할 필요가 있는 새로운 관측값
require(MASS)
(lda.obj = lda(group ~ x1 + x2, data=hemo, prior=c(0.5,0.5))) # The default prior is "proportions"
l.vec = lda.obj$scaling 
m = (lda.obj$means[1,] %*% l.vec + lda.obj$means[2,] %*% l.vec)/2 # m 은 식 (11.20)에 나오는 � 을 가리킴
# 직선의 식은 l[1]*x1 + l[2]*x2 - m = 0 이므로 기울기는 -l[1]/l[2], 절편은 m/l[2]
abline(a = m/l.vec[2], b = -l.vec[1]/l.vec[2],col=3)

lda.obj = lda(group ~ x1 + x2, data = hemo, prior = c(0.5, 0.5))
newdata = data.frame(-0.210, -0.044)
names(newdata) = c('x1', 'x2')
predict(lda.obj, newdata)

salmon = read.table("T11-2.DAT")
names(salmon) = c("region","gender","fresh","marine")
plot(salmon[,3:4],pch=salmon$region,col=salmon$gender) # 연어의 産地를 점 모양으로, 성별을 색으로 구분
plot(salmon[,3:4],pch=((salmon$region - 1)*15 + 1)) # 연어의 산지만 구분
# Linear Discriminant Analysis: Multivariate Normal with Equal Covariance Assumed
lda.obj = lda(region ~ fresh + marine, data=salmon, prior=c(0.5,0.5), CV=TRUE)
table(salmon$region,lda.obj$class) # CV estimate of actual error rate = (1 + 6)/100 = 0.07
# Quadratic Discriminant Analysis: Multivariate Normal with Unequal Covariance Assumed
qda.obj = qda(region ~ fresh + marine, data=salmon, prior=c(0.5,0.5), CV=TRUE)
table(salmon$region,qda.obj$class) # CV estimate of actual error rate = (3 + 5)/100 = 0.08 

# • manova함수를 쓰면 두 모집단이 아닌 여러 모집단의 평균 벡터에 대한 검정도 실시할 수 있다.

#### [Exercise 11.34 : Cereal Data] ####
cereal = read.table("T11-9_corrected.DAT")
names(cereal) = c("brand","manufacturer","calories","protein","fat","sodium","fiber",
                  "carbohydrates","sugar","potassium","group")
data = subset(cereal, select = -c(brand, group))

## (1) Actual Error Rate 추정 ##

# holdout procedure에 따라 AER를 추정하기 위해 lda함수에서 CV = TRUE라고 지정해야 한다. AER을 추정할 때 
# 사전확률과 각 모집단에서 추출한 표본크기를 같이 고려해야 한다. 
# 단순히 잘못 추정된 개체의 수 13개를 전체 표본크기 43으로 나누어주면 안된다.
library(MASS)
p1 = p2 = p3 = 1/3
obj.cv = lda(manufacturer ~ ., data = data, prior = c(p1, p2, p3), CV = TRUE)

(cm = table(data$manufacturer, obj.cv$class))
p1*(cm[1, 2] + cm[1, 3])/sum(cm[1,]) + p2*(cm[2, 1] + cm[2, 3])/sum(cm[2,]) +
  p3*(cm[3, 1] + cm[3, 2])/sum(cm[3, ])


#### (2) discriminant functions 계수 해석 ####
# 여기서 discriminant functions는 Minimum ECM Rule의 판별함수, 즉 교재 (11-51)식을 의미한다.
# 아래 출력에서 첫 열은 General Mills, 두 번째열은 Kellogg이다. 
# Kellogg 제품에서 protein과 fiber 계수가 제일 크며 fat의 계수가 제일 작으므로 영양가 높은(nutritional) 제품이라고 할 수 있다. 
# 다만 sugar에 관한 계수도 같이 높아 소비자의 입맛도 고려했음을 알 수있다.
X1 = subset(data, manufacturer == "G", select = -manufacturer); n1 = nrow(X1)
X2 = subset(data, manufacturer == "K", select = -manufacturer); n2 = nrow(X2)
X3 = subset(data, manufacturer == "Q", select = -manufacturer); n3 = nrow(X3)
S_pooled = ((n1 - 1)*cov(X1) + (n2 - 1)*cov(X2) + (n3 - 1)*cov(X3))/(n1 + n2 + n3 - 3)
cbind(solve(S_pooled) %*% colMeans(X1), solve(S_pooled) %*% colMeans(X2),
      solve(S_pooled) %*% colMeans(X3))

# lda함수 실행결과 얻어지는 scaling 행렬의 각 열은 Fisher’s sample linear discriminants로서 Minimum ECM Rule의 판별함수와 다르다. 
# scaling 행렬을 얻으려면 자동설정값인 CV = FALSE로 지정해야한다.
obj.lda = lda(manufacturer ~ ., data = data, prior = c(p1, p2, p3))
obj.lda$scaling

#### (3) Fisher’s linear discriminants를 이용해 이차원 그림 그리기 ####
# 각 변수를 중심화(centering), 즉, 평균을 0으로 만들어주는 것이 좋다. 이 때 분산을 1로 만들면 안 된다. 
# linear discriminants를 계산할 때 공분산행렬의 역행렬을 곱해줌으로써 각 변수의 분산을 이미 고려했기 때문이다.
# 영양가 높은 제품의 첫 판별자 값이 큰 값을 갖도록 하기 위해 부호를 바꾸어 주었다. 
# 이 그림에서도 Kellogg 제품이 nutritional 함을 알 수 있다.
l1 = obj.lda$scaling[,1]; l2 = obj.lda$scaling[,2]
X = subset(data, select = -manufacturer)
X.cen = scale(X, center=T, scale=FALSE)
LD1 = X.cen %*% (-l1); LD2 = X.cen %*% l2
plot(LD1, LD2, col=cereal$group)
legend("topright", legend = c("G", "K", "Q"), pch = 1, col = 1:3)
