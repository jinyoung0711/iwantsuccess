# [09-01 class]

# (1) Classification trees using training data
install.packages("tree")
library(tree)
library(ISLR)
attach(Carseats)

str(Carseats)
head(Carseats)
dim(Carseats)
quantile(Sales)

High <- as.factor(ifelse(Sales <= 8, 'NO', 'Yes'))
table(High) # NO : 236, Yes : 164
Carseats <- data.frame(Carseats, High)
head(Carseats)

tree.carseats <- tree(High ~ . -Sales, Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 2)

tree.carseats

# (2) Classification tree using test error rate
set.seed(3)
train <- sample(1:400, 200)
length(unique(train)) # uinque한 값 200개 -> 비복원추출
test <- setdiff(1:400, train)
High.test <- High[test]
Carseats.test <- Carseats[test,]

tree.carseats <- tree(High ~ . - Sales, Carseats, subset = train) # only use train
summary(tree.carseats)
tree.pred <- predict(tree.carseats, Carseats.test, type = "class") # 실제로 무엇으로 분류가 되었는지. type = "class"
table(tree.pred, High.test)

# (3) CV + pruning for classification tree
set.seed(6)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
class(cv.carseats)
names(cv.carseats)                       
cv.carseats 
# k = alpha 값이 커지면 size가 줄어듬 -> 느슨해짐(터미널 노드가 적은 모델)
# k = alpha = 1인 터미널 노드가 12개인 모델이 가장 좋아보임 

prune.carseats <- prune.misclass(tree.carseats, best = 12)
plot(prune.carseats)
text(prune.carseats, pretty=0, cex=0.8)

tree.pred <- predict(prune.carseats, Carseats.test, type='class')
table(tree.pred, High.test)

# (4) Regression Tree using training data with Boston dataset
library(MASS)
set.seed(1)
head(Boston)
dim(Boston) # 506 14 
train <- sort(sample(1:nrow(Boston), nrow(Boston)/2))
test <- setdiff(1:nrow(Boston), train)

tree.boston <- tree(medv ~., Boston, subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty=0, cex=0.8)

# (5) CV + pruning for Regression Tree
cv.boston <- cv.tree(tree.boston) # 10 fold CV : default
summary(cv.boston)
cv.boston

prune.boston <- prune.tree(tree.boston, best = 9)
plot(prune.boston)
text(prune.boston, cex=0.8)

yhat <- predict(tree.boston, Boston[test, ])
y <- Boston[test,]$medv
plot(yhat, y)
abline(0,1)

mse.test <- mean((yhat - y)^2)
mse.test

# (6) Bagging 
library(randomForest)
library(MASS)
set.seed(1)
head(Boston)
dim(Boston) # 506 14 
train <- sort(sample(1:nrow(Boston), nrow(Boston)/2))
test <- setdiff(1:nrow(Boston), train)

bag.boston <- randomForest(medv~., data=Boston, subset=train,
                           mtry=13, importance=T, ntree=500)
bag.boston

y <- Boston[test, ]$medv
yhat.bag <- predict(bag.boston, newdata=Boston[test,])
plot(yhat.bag, y)
abline(0,1, col='red')

mean((yhat.bag-y)^2) # MSE (tree보다 좋은 성능을 보임)
importance(bag.boston)
varImpPlot(bag.boston)

# (7) Random Forests
set.seed(1)
rf.boston <- randomForest(medv~., data=Boston, subset=train,
                          mtry=6, importance=T)
yhat.rf <- predict(rf.boston, Boston[test,])

plot(yhat.rf, y)
abline(0,1, col='red')

mean((yhat.bag-y)^2) # MSE (bagging보다 좋은 성능을 보임)
importance(rf.boston)
varImpPlot(rf.boston)

# (8) Boosting : 점진적으로 TRUE에 가까워지게 함
library(gbm)
set.seed(1)
boost.boston <- gbm(medv~., Boston[train,], distribution="gaussian",
                    n.trees=5000, interaction.depth=1, shrinkage=0.5)

summary(boost.boston)

yhat.boost <- predict(boost.boston, Boston[test,], n.trees=5000)
mean((yhat.boost-y)^2) # RF보다 좋은 성능

