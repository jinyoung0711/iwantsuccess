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
  