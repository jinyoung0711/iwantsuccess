# [Exercise 11.32]
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