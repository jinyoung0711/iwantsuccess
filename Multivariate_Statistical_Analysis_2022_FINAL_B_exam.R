#### [1-1] ####
data = iris
head(data)
summary(data$Species)

(mean1 = colMeans(data[data$Species=="setosa",c(1,2,3,4)])) # setosa 평균벡터
(mean2 = colMeans(data[data$Species=="versicolor",c(1,2,3,4)])) # versicolor 평균벡터
(mean3 = colMeans(data[data$Species=="virginica",c(1,2,3,4)])) # virginica 평균벡터

(cov1 = cov(data[data$Species=="setosa",c(1,2,3,4)])) # setosa 공분산행렬
(cov2 = cov(data[data$Species=="versicolor",c(1,2,3,4)])) # versicolor 공분산행렬
(cov3 = cov(data[data$Species=="virginica",c(1,2,3,4)])) # virginica 공분산행렬

#### [1-2] ####

library(MASS)
p1 = p2 = p3 = 1/3

lda.obj = lda(Species ~ ., data = iris, prior=c(p1, p2, p3), CV=TRUE)
table(data$Species,lda.obj$class) # CV estimate of actual error rate = (1 + 2)/150 = 0.02

#### [1-3] ####
qda.obj = qda(Species ~ ., data = iris, prior=c(p1, p2, p3), CV=TRUE)
table(data$Species,qda.obj$class) # CV estimate of actual error rate = (1 + 3)/150 = 0.02666667

#### [1-4] ####
# actual error rate가 더 낮은 lda 방법이 더 적합하다고 생각한다.

#### [2-1] ####
track.women = read.table("T1-9.DAT", fill = TRUE, header = FALSE)
names(track.women) = c("country","m100","m200","m400","m800","m1500","m3000","marathon")
head(track.women)

rownames(track.women) = track.women$country

# ML Method
obj = factanal( x = as.matrix(track.women[, 2:8]), factors = 2, scores = 'regression', rotation = 'none')
L = obj$loadings; 

apply(L^2, 1, sum)/7 # 공통성이 변수의 분산에서 차지하는 비율

apply(L^2, 2, sum)/7 # 전체 분산에서 두 인자가 설명하는 비율

#### [2-2] ####

# [PC Method]
obj = princomp(track.women[,2:8], cor=TRUE)
E = obj$loadings[,1:2]
(L = cbind(obj$sdev[1]*E[,1], obj$sdev[2]*E[,2])) # 적재행렬

apply(L^2, 1, sum)/7 # 공통성이 변수의 분산에서 차지하는 비율

apply(L^2, 2, sum)/7 # 전체 분산에서 두 인자가 설명하는 비율

#### [2-3] ####
(L.rotate = varimax(L)$loadings[1:7, 1:2]) # varimax 기준으로 인자 회전; 적재행렬

apply(L.rotate^2, 2, sum)/7 # 전체 분산에서 두 인자가 설명하는 비율

#### [3-1] ####
dfr = read.table("T11-6.DAT")
names(dfr) = c("GPA","GMAT","POP")
dfr.std = dfr
dfr.std[,1:2] = scale(dfr[,1:2])  # 2개의 변수(gpa, gmat)를 표준화

hc.ward = hclust(dist(dfr.std[, 1:2]), method = 'ward.D2')
plot(hc.ward, xlab = '', ylab = 'Distance', labels = dfr.std$POP)

#### [3-2] ####
ward = as.matrix(cutree(hc.ward, 3))
dfr.std[, 4] = ward
(tbl = table(dfr.std$POP, dfr.std$V4))
#### [3-3] ####

hc.average = hclust(dist(dfr.std[, 1:2]), method = 'average')
plot(hc.average, xlab = '', ylab = "Distance", labels = dfr.std$POP)
average = as.matrix(cutree(hc.average, 3))                    
dfr.std[, 5] = average
(tbl = table(dfr.std$POP, dfr.std$V5))
