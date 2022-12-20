#### Multivariate Statistical Analysis 2020 FINAL exam ####
### [1-1] ### - Factor Analysis
# 해당 문제에서는 correlation matrix, loading matrix를 줌.
L = matrix(c(.578, .852, .712, .763, .908, .753, .513,
             -.685, -.157, -.086, -.329, .218, .474, .283), ncol = 2)
L

# MURDER 변수의 공통성(communality)을 구하시오.
# x 변수의 공통성을 구할 때는 해당 원소들을 제곱해서 합하면 된다.
communality = rowSums(L*L)[1]
communality

### [1-2] ### - Factor Analysis : 
# Factor1이 설명하는 분산의 비율을 구하시오.
colSums(L*L)[1] / 7
# 전체 분산의 합 -> 변수의 개수
# 적재행렬에서 제곱해서 합한 값을 전체 분산의 합으로 나누어주면 된다
# communality -> rowSums(), explained by the x factor -> colSums() 주의하기.

### [1-3] ### - Factor Analysis : Residual Matrix
# Residual matrix의 첫 번째 열을 구하시오.
LLT = L %*% t(L)
c(0, c(.601, .484, .649, .386, .102, .069) - LLT[2:7, 1])

# 잔차행렬은 (R-LL'-Psi)이며 대각원소는 0이라는 점을 이용하여 계산.
# 잔차행렬의 대각원소는 0이어야 하므로 첫 번쨰 원소는 0이다!!!! 틀리면 감점

### [3-1] ###
# 겉보기오분류율은 분류규칙을 만들 때 쓴 자료를 평가할 때도 써서 얻어진다. 
# 만들어진 분류규칙은 만들 때 쓴 자료에 최적화되어 있으므로 겉보기오분류율은 독립적인 자료에 대한 오분류율을 과소추정하게 된다. 
# 교차타당성 방법에 의한 오분류율은 이런 문제가 없으므로 더 정확하다.

### [3-2] ###
# 사전확률이 주어졌을 때, 오분류율을 계산하는 방법.
p1 = 0.05; p2 = 0.95
p1*(18/(18+3)) + p2*0

### [4-1] ###
# K=3인 K-means 방법으로 군집을 형성해서 각 군집에 어떤 개체가 속하는지 알아보았다.
cereal = read.table("T11-9_corrected.DAT") # -> check
names(cereal) = c("brand", "manufacturer", "calories", "protein", "fat", "sodium",
                  "fiber", "carbohydrates", "sugar", "potassium", "group")
data = subset(cereal, select = -c(brand, group)) 
# Manafacturer를 모른다고 가정, 제품의 특성을 나타내는 변수만 사용, 따라서 Brand와 Group열 제거
X = data[,2:9] # Manafacturer 해당하는 열 제거
X.std = scale(X) # scale 진행
obj.kmeans = kmeans(X.std, 3) # K = 3; K-means 진행
cereal$brand[obj.kmeans$cluster == 1]
cereal$brand[obj.kmeans$cluster == 2]
cereal$brand[obj.kmeans$cluster == 3]

### [4-2] ###
library(MASS)
p1 = p2 = p3 = 1/3 # 사전확률은 1/3로 동일하다고 가정
obj.lda = lda(manufacturer ~ ., data = data, prior = c(p1, p2, p3), CV = TRUE)
cereal$brand[obj.lda$class == "G"]
cereal$brand[obj.lda$class == "K"] 
cereal$brand[obj.lda$class == "Q"] 

### [4-3] ###
# 두 분석 결과가 일치하는지 확인
# 판별분석의 confusion matrix와 달리 대각원소가 클 필요는 없다. 
# 군집분석으로 얻어지는 소속군집에 순서가 있지 않기 때문이다. 
# 각 행을 봤을 때 어느 한 열에 많이 몰려 있으면 두 분석의 결과가 비슷함을 의미한다. 
# 그런데 어떤 행이든 어느 한 열에 몰려 있지 않으므로 두 분석 결과는 다소 다르다고 결론내릴 수 있다.

table(obj.kmeans$cluster, obj.lda$class)

# 군집분석은 8개의 제품특성이 서로 비슷한 것들끼리 같은 집단으로 묶으려 하고, 
# 판별분석은 훈련자료에서 이미 알려진 세 개의 제조사로 분류를 잘하는 규칙을 만들어 집단을 구성한다. 
# 두 분석의 목적이 다르다. 
# 세 제조사의 제품 특성이 확연히 다르고, 같은 제조사의 제품은 비슷한 특성을 가졌다면 두 분석결과가 비슷할 것이다.

# 판별분석의 오분류율이 0.345로 높은 것에서 알 수 있듯이 (이 오분류율은 숙제 해답에 있는 값임) 제품특성으로는 분류가 잘 되지 않는다. 
# 이것은 제조사별로 제품 특성이 확연히 다르지 않다는 것을 의미한다. 이런 이유로 두 분석결과가 다소 다르다고 생각된다.