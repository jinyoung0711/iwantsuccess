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

