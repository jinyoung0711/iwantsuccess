# Example 12.3: Agglomerative Clustering Using Single Linkage
D = matrix(c(0,9,3,6,11,9,0,7,5,10,3,7,0,9,2,6,5,9,0,8,11,10,2,8,0),ncol=5)
hc.single = hclust(as.dist(D),method="single") # as.dist() -> 일반 행렬을 거리 행렬로 바꿔줌
plot(hc.single,xlab="",ylab="Distance")
cutree(hc.single,3) # 군집의 수를 3개로 했을 때 각 개체의 소속 군집 출력

# Agglomerative Clustering with University Data in Table 12.9
univ = read.table("T12-9.DAT")
names(univ) = c("name","sat","top10","accept","sfratio","expenses","grad")
X = scale(univ[,2:7]) # 변수의 특성이 달라 표준화가 필요
plot(hclust(dist(X),method="single"),xlab="",ylab="Distance",labels=univ$name)
plot(hclust(dist(X),method="complete"),xlab="",ylab="Distance",labels=univ$name)
plot(hclust(dist(X),method="average"),xlab="",ylab="Distance",labels=univ$name)
plot(hclust(dist(X),method="ward"),xlab="",ylab="Distance",labels=univ$name)

# Example 12.12: K-means Clustering of 22 Public Utilities (비계층적 군집방법의 대표적인 예 : K-means)
util = read.table("T12-4.DAT") # 22 utility companies with 8 variables of characteristics
colnames(util) = c(paste("x",1:8,sep=""),"company")
util.std = util
util.std[,1:8] = scale(util[,1:8]) # 8개의 변수를 표준화
kmeans(util.std[,1:8],4) # the number of clusters, k = 4 -> 군집의 수를 미리 정해줘야함
kmeans(util.std[,1:8],5) # the number of clusters, k = 5 -> 군집의 수를 미리 정해줘야함
kmeans(util.std[,1:8],5,nstart=10) # 10번 수행해서 제일 나은 결과 출력

# 적절한 군집의 수 k 정하기
# within SS가 안정되기 시작하는 k 또는 pseudo F 통계량 값이 최대가 되는 k를 찾는다.
WSS = pseudoF = numeric()
n = nrow(util.std)
for (k in 2:8) {
  obj = kmeans(util.std[,1:8],k)
  WSS[k] = obj$tot.withinss
  R.sq = 1 - obj$tot.withinss/obj$totss
  pseudoF[k] = (R.sq/(k-1))/((1-R.sq)/(n-k))
}
oldpar = par(mfrow=c(1,2))
plot(2:8,WSS[2:8],type="b",xlab="number of clusters",ylab="within SS")
plot(2:8,pseudoF[2:8],type="b",xlab="number of clusters",ylab="pseudo F statistic")
par(oldpar)

# P Seudo F statistic이 최대가 되는 값 k=4 or k=7이 적절해 보임.

# Example 12.14: Airline Distance Data - MDS with Distance Matrix
airline = read.table("T12-7_corrected.DAT.txt")
D = as.dist(airline[,1:12])
names(D) = c(as.character(airline[,13]))
(obj = cmdscale(D,k=2)) # cmdscale() : 전통적 다차원 척도법에 사용(Numeric data로만 이루어짐)
plot(obj[,1],obj[,2],type="n",axes=F,xlab="",ylab="")
text(x=obj[,1],y=obj[,2],names(D),cex=0.9)
plot(-obj[,1],-obj[,2],type="n",axes=F,xlab="",ylab="") # 동서와 남북을 바꿈
text(x=-obj[,1],y=-obj[,2],names(D),cex=0.9)

# MDS가 제대로 적용되어 있는지 check -> stress function -> q가 커지면 작아짐
# 차원의 수 q에 대한 stress를 구해서 그래프로 그리기
# metric MDS 방법을 적용하는 cmdscale의 stress는 직접 계산해야 함.
stress = numeric()
for (k in 1:5) { # k는 q를 의미
  obj = cmdscale(D,k=k)
  D.q = dist(obj)
  stress[k] = 100*sqrt(sum((D - D.q)^2)/sum(D^2)) # stress in percent
}
oldpar = par(mfrow=c(1,2))
plot(stress,type="b",xlab="q",main="stress (in %) for metric MDS")

# Example 12.15: Utility Data - MDS with Raw Data
util = read.table("T12-4.DAT") # 22 utility companies with 8 variables of characteristics
colnames(util) = c(paste("x",1:8,sep=""),"company")
X = scale(util[,1:8])

# metric MDS vs nonmetric MDS
# 이 자료는 metric MDS가 더 적절 -> 순서뿐 아니라 얼마나 크고 작은가 크기의 절대적인 값도 의미가 있는 정량적 자료이기 때문 
obj = cmdscale(dist(X),k=2)
label.vec = util$company
oldpar = par(mfrow=c(1,2))
plot(obj[,1],obj[,2],type="n",axes=F,xlab="",ylab="",main="metric MDS")
text(x=obj[,1],y=obj[,2],label.vec,cex=1.0)
obj = isoMDS(dist(X),k=2,trace=F)
label.vec = util$company
plot(obj$points[,1],obj$points[,2],type="n",axes=F,xlab="",ylab="",main="nonmetric MDS")
text(x=obj$points[,1],y=obj$points[,2],label.vec,cex=1.0)
par(oldpar)

# 차원의 수 q에 대한 stress를 구해서 그래프로 그리기
D = dist(X)
stress = numeric()
for (k in 1:6) {
  obj = cmdscale(dist(X),k=k)
  D.q = dist(obj)
  stress[k] = 100*sqrt(sum((D - D.q)^2)/sum(D^2)) # stress in percent
}
oldpar = par(mfrow=c(1,2))
plot(stress,type="b",xlab="q",main="stress (in %) for metric MDS")
stress = numeric()
for (k in 1:6) stress[k] = isoMDS(dist(X),k=k,trace=F)$stress
plot(stress,type="b",xlab="q",main="stress (in %) for nonmetric MDS")
par(oldpar)

# Mosaic Plots
T12.8 = read.table("T12-8.DAT") # 분할표 자료
dimnames(T12.8) = list(paste("P",0:6,sep=""),c("A","B","C","D"))
oldpar = par(mfrow=c(1,2))
mosaicplot(T12.8,main="Type by Site",col=T,xlab="Site",ylab="Type")
mosaicplot(t(T12.8),main="Site by Type",col=T,xlab="Type",ylab="Site")
par(oldpar)

# raw data가 주어질 때는 table과 xtabs 참조해서 분할표 형태로 직접 만들어줘야함.

# Mosaic Plot for Multi-way Table
mosaicplot(Titanic, col=TRUE)
mosaicplot(~ Sex + Class + Survived, data=Titanic, col=TRUE)

plot(corresp(T12.8, nf = 2))

# 실습 Mosaic Plots
library(MASS)
par(mfrow = c(1,1))

caith # 해당 자료에서 fair는 금발을 의미.

mosaicplot(caith, col = TRUE, main = "Eye Color vs Hair color", 
           xlab = "Eye Color", ylab = "Hair Color")

mosaicplot(t(caith), col = TRUE, main = "Eye Color vs Hair color",
           ylab = "Eye Color", xlab = "Hair Color")

# plot(corresp(caith))
plot(corresp(caith, nf = 2)) # 대응분석을 해서 그림 그림.
# blue, light는 비슷한 분포, fair와 비슷한 분포

# library(MASS)
x1 = mvrnorm(n=10, mu=c(1,1), Sigma=matrix(c(1,0.5,0.5,1), nrow=2))
x2 = mvrnorm(n=10, mu=c(3,5), Sigma=matrix(c(1,0,0,1), nrow=2))
x3 = mvrnorm(n=10, mu=c(5,1), Sigma=matrix(c(1,-0.5,-0.5,1), nrow=2))

# Multivariate normal distribution is elliptically shaped.
x.mat = rbind(x1, x2, x3)
group = rep(1:3, c(10, 10, 10)) # This is not known to the analyst.
plot(x.mat, col=group, asp=1, xlab="x1", ylab="x2")

plot(x.mat, col=1, pch=16, asp=1, xlab="x1", ylab="x2")

# Assume we do not know which group the observations belong to.
plot(hclust(dist(x.mat), method="single"), labels=group) # Linkage method

plot(hclust(dist(x.mat), method="complete"), labels=group) # Linkage method

plot(hclust(dist(x.mat), method="average"), labels=group) # Linkage method

plot(hclust(dist(x.mat), method="ward.D2"), labels=group) # Ward's method

# Findings: Ward's method is better for the elliptically shaped data.
# What if the order of observations are changed?
shuffled.row = sample(1:30, 30)
x.shuffle = x.mat[shuffled.row,]
group.shuffle = group[shuffled.row]
plot(hclust(dist(x.shuffle), method="ward.D2"), labels=group.shuffle) # no change on the result

# Biplot : 주성분이 의미가 없다면 Biplot 또한 무의미
# 대학자료는 변수의 특성이 너무 다르기 때문에 표준화를 진행하는 것이 많다.
univ <- read.table("T12-9.DAT")
names(univ) = c("name", "SAT", "Top10", "Accept", "SFRatio", "Expenses", "Grad")
row.names(univ) = univ$name
X = univ[, -1]
X.std = scale(X)
biplot(princomp(X.std))

library(MASS)
x1=mvrnorm(n=10,mu=c(1,1),Sigma=matrix(c(1,0.5,0.5,1),nrow=2))
x2=mvrnorm(n=10,mu=c(3,5),Sigma=matrix(c(1,0,0,1),nrow=2))
x3=mvrnorm(n=10,mu=c(5,1),Sigma=matrix(c(1,-0.5,-0.5,1),nrow=2)) # Multivariate normal distribution is elliptically shaped.
x.mat=rbind(x1, x2, x3)
group=rep(1:3,c(10,10,10)) # This is not known to the analyst.
plot(x.mat,col=group,asp=1,xlab="x1",ylab="x2")
plot(x.mat,col=1,pch=16,asp=1,xlab="x1",ylab="x2")

# Assume we do not know which group the observations belong to.
plot(hclust(dist(x.mat),method="single"),labels=group)# Linkage method
plot(hclust(dist(x.mat),method="complete"),labels=group)# Linkage method
plot(hclust(dist(x.mat),method="average"),labels=group)# Linkage method
plot(hclust(dist(x.mat),method="ward.D2"),labels=group)# Ward's method

# Findings: Ward's method is better for the elliptically shaped data.
# What if the order of observations are changed?
shuffled.row=sample(1:30,30)
x.shuffle=x.mat[shuffled.row,]
group.shuffle=group[shuffled.row]
plot(hclust(dist(x.shuffle),method="ward.D2"),labels=group.shuffle)# no change on the result