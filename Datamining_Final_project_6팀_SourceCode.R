#### data load #### 

df <- read.csv("./heart.csv", header=T)
head(df)
dim(df)
sum(is.na(df)) #결측치 없음.
str(df)
# Sex, ChestPainType, FastingBS, RestingECG, 
# ExerciseAngina, ST_Slope, Heartdisease -> 범주형변수
summary(df)


#### load library ####
library(ggplot2)
library(e1071)
library(leaps)
library(caret)
library(ROCR)
library(randomForest)
library(pROC)

#### 1. EDA ####
col1=adjustcolor("red", 0.4) ; col2=adjustcolor("blue", 0.4)
col12=c(col1,col2)

## 1. Age (나이) ##
min(df$Age) ; max(df$Age)
age_cut<-cut(x=df$Age, breaks=c(20, 30, 40, 50, 60, 70, Inf), right=F)
table(age_cut) # 40대, 50대, 60대의 관측치가 많음.
table(df$HeartDisease, age_cut)

ggplot(df, aes(x=factor(HeartDisease), fill=age_cut))+
  geom_bar(position="dodge")+
  ggtitle("Plot of Age by HeartDisease\n")+
  theme(plot.title = element_text(hjust = 0.5))
### 20대의 경우, 환자가 아닌 사람의 관측치만 존재함.
### 50대부터, 환자가 아닌 사람보다 환자인 사람들이 더 많으며, 
### 60대의 경우 환자의 수가 환자가 아닌 사람의 수보다 약 3배 이상임.

## 2. Sex (성별) ##
table(df$HeartDisease, df$Sex)

ggplot(df, aes(x=factor(HeartDisease), fill=Sex))+
  geom_bar(position="dodge")+
  ggtitle("Plot of Sex by HeartDisease\n")+
  theme(plot.title = element_text(hjust = 0.5))
### 918개의 관측치 중 여성 관측치는 193개로, 전체 관측치의 21%에 해당함.
### 여성의 경우, 환자가 아닌 사람 수가 환자 수보다 약 3배 많고
### 남성의 경우, 환자 수가 환자가 아닌 사람 수보다 약 2배 많음.

## 3. ChestPainType (가슴 통증 종류) ##
table(df$HeartDisease, df$ChestPainType)

ggplot(df, aes(x=factor(HeartDisease), fill=ChestPainType))+
  geom_bar(position="dodge")+
  ggtitle("Plot of ChestPainType by HeartDisease\n")+
  theme(plot.title = element_text(hjust = 0.5))
### ChestPainType이 ASY인 경우가 전체 관측치의 54%에 해당하며
### ASY인 경우만, 환자의 수가 환자가 아닌 사람의 수보다 많고, 4배 가까이 많음.

## 4. RestingBP (안정 시 혈압) ##
hist(df$RestingBP, breaks=20, main="Histogram of RestingBP")
boxplot(df$RestingBP, main="Boxplot of RestingBP")
sum(df$RestingBP==0)
df[df$RestingBP==0,]
#RestingBP가 0인 값이 하나 있음. -> 제거 필요

### RestingBP 값이 0인 관측치가 1개 존재하여 (450번) 제거가 필요함.
### 0인 값을 제거한 후 histogram, boxplot 다시 확인하여 
### 추가 이상치 제거 필요한지 확인 필요함.

## df<-df[which(df$RestingBP==0),]
## 0인 값 제거 이후 사용될 EDA
min(df$RestingBP) ; max(df$RestingBP)
boxplot(RestingBP ~ HeartDisease, df, col=col12, 
        main="Plot of RestingBP by HeartDisease\n")
legend("topright", legend=unique(df$HeartDisease), fill=col12) 


## 5. Cholesterol (콜레스테롤 수치 mg/dl) ##
hist(df$Cholesterol, breaks=50, main="Histogram of Cholesterol") 
boxplot(df$RestingBP, main="Boxplot of Cholesterol") 
sum(df$Cholesterol==0)
sum(df$Cholesterol==0 & df$HeartDisease==1)
### 0인 값들 이상치 -> 172개 제거 필요함.
### 0인 값을 제거한 후 histogram, boxplot 다시 확인하여 추가 이상치 제거 필요한지 확인 필요함.
### Cholesterol 값이 0이면서 HeartDisease가 1인 관측치가 152개임.
### 심장병(HeartDisease) 환자 중 Cholesterol 측정이 불필요할 정도로 뚜렷한 증상을 보인 환자들이 있었다고 유추 가능.

## df<-df[which(df$Cholesterol!=0), ] ; nrow(df)
min(df$Cholesterol) ; max(df$Cholesterol)
boxplot(Cholesterol ~ HeartDisease, df, col=col12, 
        main="Plot of Cholesterol by HeartDisease\n")
legend("topright", legend=unique(df$HeartDisease), fill=col12) 


## 6. FastingBS (공복혈당 > 120 mg/dl (T, F)) ##
table(df$HeartDisease, df$FastingBS)
ggplot(df, aes(x=factor(HeartDisease), fill=factor(FastingBS)))+
  geom_bar(position="dodge")+
  ggtitle("Plot of FastingBS by HeartDisease\n")+
  theme(plot.title = element_text(hjust = 0.5))
### FastingBS == 1 ->  if FastingBS > 120 mg/dl
### FastingBS가 1인 경우 환자의 수가 환자가 아닌 사람 수보다 약 4배 더 많음.

## 7. RestingECG (안정시 심전도 결과) ##
table(df$HeartDisease, df$RestingECG)
ggplot(df, aes(x=factor(HeartDisease), fill=RestingECG))+
  geom_bar(position="dodge")+
  ggtitle("Plot of RestingECG by HeartDisease\n")+
  theme(plot.title = element_text(hjust = 0.5))
### LVH, Normal, ST 모두 환자가 아닌 사람 수보다 환자의 수가 많으며, 
### RestingECG에 따른 HeartDisease의 뚜렷한 변화가 관찰되지 않음.
 
## 8. MaxHR (최대심박수) ##
hist(df$MaxHR, main="Histogram of MaxHR")
### 정규분포와 유사한 형태를 지니고 있음.
boxplot(df$MaxHR, main="Boxplot of MaxHR")
min(df$MaxHR) ; max(df$MaxHR)
nrow(df[which(220-df$Age<df$MaxHR),]) 
### 최대 심박수 일반적인 공식 : 220-나이
### 측정된 최대 심박수가 해당 나이의 평균 최대심박수보다 높은 경우 87가지.

# # 전처리 이후 사용 가능
# boxplot(MaxHR ~ HeartDisease, df, col=col12, 
#         main="Plot of MaxHR by HeartDisease\n")
# legend("topright", legend=unique(df$HeartDisease), fill=col12) 
# # HeartDisease==0인 경우 가 1인 경우에 비하여 MaxHR 평균, 중앙값이 더 큼.
# mean(df$MaxHR[which(df$HeartDisease==0)]) ; mean(df$MaxHR[which(df$HeartDisease==1)])
# median(df$MaxHR[which(df$HeartDisease==0)]) ; median(df$MaxHR[which(df$HeartDisease==1)])


## 9. ExerciseAngina (운동으로 인한 협심증 여부) ##
table(df$HeartDisease, df$ExerciseAngina)
ggplot(df, aes(x=factor(HeartDisease), fill=ExerciseAngina))+
  geom_bar(position="dodge")+
  ggtitle("Plot of ExerciseAngina by HeartDisease\n")+
  theme(plot.title = element_text(hjust = 0.5))

### ExerciseAngina가 N인(없는) 경우는 환자가 아닌 사람이 환자에 비해 2배 가까이 많고
### ExerciseAngina가 Y인(있는) 경우는 환자가 환자가 아닌 사람에 비해 약 6배 많음.
### -> 상관성 높다고 볼 수 있음.

## 10. Oldpeak(안정과 관련된 ST감소) ##
hist(df$Oldpeak, breaks=30, main="Histogram of Oldpeak")
boxplot(df$Oldpeak,main="Boxplot of Oldpeak")
sum(df$Oldpeak==0) ; sum(df$Oldpeak==0) / nrow(df)
### Oldpeak의 값이 0인 관측치가 368개임. (전체 관측치가 918개) -> 40% 차지함.
### Oldpeak가 0인 것, 0이 아닌 것으로 나누어, 범주형 변수로의 변환도 가능할 것 같음.

## 11. ST_Slope (the slope of the peak exercise ST segment) ##
table(df$HeartDisease, df$ST_Slope)
ggplot(df, aes(x=factor(HeartDisease), fill=ST_Slope))+
  geom_bar(position="dodge")+
  ggtitle("Plot of ST_Slope by HeartDisease\n")+
  theme(plot.title = element_text(hjust = 0.5))

### up일 때의 HeartDisease 값 분포가 다른 경우들(Down, Flat)과 
### 반대 양상을 보임을 확인할 수 있음.

## 12. HeartDisease ##
table(df$HeartDisease)


#### Label encoding ####
df$Sex<-as.numeric(factor(df$Sex, levels=unique(df$Sex)))
df$ChestPainType<-as.numeric(factor(df$ChestPainType, levels=unique(df$ChestPainType)))
df$FastingBS<-as.numeric(factor(df$FastingBS, levels=unique(df$FastingBS)))
df$RestingECG<-as.numeric(factor(df$RestingECG, levels=unique(df$RestingECG)))
df$ExerciseAngina<-as.numeric(factor(df$ExerciseAngina, levels=unique(df$ExerciseAngina)))
df$ST_Slope<-as.numeric(factor(df$ST_Slope, levels=unique(df$ST_Slope)))


#### 2. 전처리 ####

## IQR_function ##
IQR_fun <- function(x) {
  UpperQ = fivenum(x)[4]
  LowerQ = fivenum(x)[2]
  IQR = UpperQ - LowerQ
  
  upperOutlier = x[ which( x > UpperQ + IQR*1.5) ]
  lowerOutlier = x[ which( x < LowerQ - IQR*1.5) ]
  
  df <<- df[!((x > UpperQ + IQR*1.5) | ( x < LowerQ - IQR*1.5)), ]
  return(length(upperOutlier) + length(lowerOutlier)) # 이상치로 판정되는 데이터의 개수를 의미
}

## [Drop RestingBP Outlier] ##
sum(df$RestingBP==0)
df <- df[!(df$RestingBP == 0 ), ] # 918개 중 1개 제거 -> 917개 
sum(df$RestingBP==0); 

str(df$RestingBP)
IQR_fun(df$RestingBP) # 917개 중 27개 Outlier 제거 -> 890개
str(df$RestingBP); 

par(mfrow=c(1,3))

boxplot(RestingBP ~ HeartDisease, df, col=col12, 
        main="Plot of RestingBP by HeartDisease\n")
legend("topright", legend=unique(df$HeartDisease), fill=col12) 

## [Drop Cholesterol Outlier] (콜레스테롤 수치 mg/dl) ##
df <- df[!(df$Cholesterol == 0 ), ] # 164개 데이터 제거 완료 -> 726개 데이터 존재
sum(df$Cholesterol==0)

IQR_fun(df$Cholesterol) # 22개 Outlier 제거 -> 704개 데이터 존재
str(df$Cholesterol)

boxplot(Cholesterol ~ HeartDisease, df, col=col12, 
        main="Plot of Cholesterol by HeartDisease\n")
legend("topright", legend=unique(df$HeartDisease), fill=col12) 

## [Drop MaxHR Outlier] ##
IQR_fun(df$MaxHR) # 0개 Outlier 제거
str(df$MaxHR)

## [Drop Oldpeak Outlier] ##
IQR_fun(df$Oldpeak) # 12개 Outlier 제거 -> 692개 데이터 존재
str(df$Oldpeak)

boxplot(Oldpeak ~ HeartDisease, df, col=col12, 
        main="Plot of Oldpeak by HeartDisease\n")
legend("topright", legend=unique(df$HeartDisease), fill=col12) 

#### Model selection [1] : using K-fold CV ####
k <- 10 
set.seed(1)
folds <- base::sample(1:k, nrow(df), rep=TRUE)
table(folds)
str(df)
cv.error <- matrix(0, k, 11)
colnames(cv.error) <- paste(1:11)
cv.error

for (j in 1:10) {
  best.fit <- regsubsets(HeartDisease~., data=df[folds!=j,], nvmax=11) # training dfa
  test.mat <- model.matrix(HeartDisease~., data=df[folds==j,])
  for (i in 1:11) {
    coeff <- coef(best.fit, id=i)
    y.pred <- test.mat[,names(coeff)] %*% coeff # test dfa
    cv.error[j,i] <- mean((df$HeartDisease[folds==j]-y.pred)^2) # test MSE
  }
}

mean.cv.error <- apply(cv.error, 2, mean)
mean.cv.error

par(mfrow=c(1,1))
plot(mean.cv.error, type='b') # plot 기준으로 8개가 적절하다고 판단

reg.best <- regsubsets(HeartDisease~., data=df, nvmax=11)
coef(reg.best, 8) 
### 선택된 변수는 Age, Sex, ChestPainType, Cholesterol, MaxHR, ExerciseAngina, Oldpeak, ST_Slope


#### Model selection [2] : using Stepwise Selection ####

## Forward Stepwise Selection ##
df$HeartDisease <- factor(df$HeartDisease, levels=unique(df$HeartDisease))
regfit.fwd <- regsubsets(HeartDisease~., data = df, nvmax=11, method="forward")
res.fwd <- summary(regfit.fwd)

# adjr2, cp, bic에서 최적의 변수의 개수
fwd.adjr2_best <- abs(which.max(res.fwd$adjr2))
fwd.cp_best <- abs(which.min(res.fwd$cp))
fwd.bic_best <- abs(which.min(res.fwd$bic))

# 결과보기 
par(mfrow = c(2,2))
plot(res.fwd$rss, xlab = "Num of variables", ylab = "rss", type="l")

plot(res.fwd$adjr2, xlab = "Num of variables", ylab = "adjr2", type="l")
points(fwd.adjr2_best, res.fwd$adjr2[fwd.adjr2_best], col="red", cex=2, pch=20)

plot(res.fwd$cp, xlab = "Num of variables", ylab = "cp", type="l")
points(fwd.cp_best, res.fwd$cp[fwd.cp_best], col="red", cex=2, pch=20)

plot(res.fwd$bic, xlab = "Num of variables", ylab = "bic", type="l")
points(fwd.bic_best, res.fwd$bic[fwd.bic_best], col="red", cex=2, pch=20)

cat("최적의 Cp : ", fwd.cp_best, "\n")
cat("최적의 BIC : ", fwd.bic_best, "\n")
coef(regfit.fwd, fwd.bic_best)
# AIC, Cp보다 BIC가 변수 증가에 더 민감하기에, 평가 지표로 BIC를 선택하였음.
# Age, Sex, ChestPainType, ExerciseAngina, ST_Slope

## Downward Stepwise Selection ##

regfit.bwd <- regsubsets(HeartDisease~., data = df, nvmax=11, method="backward")
res.bwd <- summary(regfit.bwd)


# adjr2, cp, bic에서 최적의 변수의 개수
bwd.adjr2_best <- abs(which.max(res.bwd$adjr2))
bwd.cp_best <- abs(which.min(res.bwd$cp))
bwd.bic_best <- abs(which.min(res.bwd$bic))


# 결과보기
par(mfrow = c(2,2))
plot(res.bwd$rss, xlab = "Num of variables", ylab = "rss", type="l")

plot(res.bwd$adjr2, xlab = "Num of variables", ylab = "adjr2", type="l")
points(bwd.adjr2_best, res.bwd$adjr2[bwd.adjr2_best], col="red", cex=2, pch=20)

plot(res.bwd$cp, xlab = "Num of variables", ylab = "cp", type="l")
points(bwd.cp_best, res.bwd$cp[bwd.cp_best], col="red", cex=2, pch=20)

plot(res.bwd$bic, xlab = "Num of variables", ylab = "bic", type="l")
points(bwd.bic_best, res.bwd$bic[bwd.bic_best], col="red", cex=2, pch=20)

cat("최적의 Cp : ", bwd.cp_best, "\n")
cat("최적의 BIC : ", bwd.bic_best, "\n")
coef(regfit.bwd, bwd.bic_best)
# AIC, Cp보다 BIC가 변수 증가에 더 민감하기에, 평가 지표로 BIC를 선택하였음.
# Sex, ChestPainType, MaxHR, ExerciseAngina, Oldpeak, ST_Slope 

## FWD 와 BWD 중에 어떤 방법을 선택
# fwd의 최적의 변수의 개수 = 5, bwd의 최적의 변수의 개수 = 6
coef(regfit.fwd, fwd.bic_best)
coef(regfit.bwd, bwd.bic_best)
# -> 두 결과값 중 작은 값인 Forward 방법을 선택한다.
# 따라서 이 때 선택된 변수들 :
# Age, Sex, ChestPainType, ExerciseAngina, ST_Slope

#### Model selection [3] : using best subset ####

regfit.full=regsubsets(HeartDisease~., df, nvmax=11)
reg.summary=summary(regfit.full)
reg.summary$rsq #R-square 값
reg.summary$adjr2 #adjusted R-square 값
reg.summary$cp #멜로우즈 Cp 값
reg.summary$bic #BIC 값

plot(reg.summary$rss, xlab="변수 개수", ylab="RSS", type="l") #잔차제곱합
plot(reg.summary$adjr2, xlab="변수 개수", ylab="adjustedR2", type="l") #adjusted R-square
plot(reg.summary$cp, xlab="변수 개수", ylab="Cp", type="l") #멜로우즈Cp 

# -> 모수 개수 p와 유사할수록 좋은 모델
min=numeric(10)
for(i in 1:10){
  min[i]=abs(reg.summary$cp[i]-(i+1))
}
cp_best=which.min(min)
points(cp_best, reg.summary$cp[cp_best], col="blue", cex=2, pch=20)

plot(reg.summary$bic, xlab="변수 개수", ylab="BIC", type="l")
bic_best=which.min(reg.summary$bic); cat("최적의 BIC : ", bic_best, "\n")
points(bic_best, reg.summary$bic[bic_best], col="blue", cex=2, pch=20)

cat("최적의 Cp : ", cp_best, "\n")
cat("최적의 BIC : ", bic_best, "\n")
coef(regfit.full, bic_best)
# AIC, Cp보다 BIC가 변수 증가에 더 민감하기에, 평가 지표로 BIC를 선택하였음.
# Age, Sex, ChestPainType, ExerciseAngina, ST_Slope

### Stepwise[fwd]와 best subset방법으로 얻은 변수는 같다.
### Stepwise[bwd]보다 Stepwise[fwd]가 값이 적어 좋지만 유의미한 차이를 보이기위해 bwd를 통해 df도 생성하였다.
### 따라서 K-fold, Stepwise[bwd], best_subset(Stepwise[fwd]) 데이터 프레임을 구분하여 생성하였다.

k_fold.df <- df[, c(1, 2, 3, 5, 8, 9, 10, 11, 12)]
head(k_fold.df)

bwd.df <- df[, c(2, 3, 8, 9, 10, 11, 12)]
head(bwd.df)

best_subset.df <- df[, c(1, 2, 3, 6, 8, 12)]
head(best_subset.df)

#### 3-1. 모델 구축 ####
k_fold.df$HeartDisease <- as.factor(k_fold.df$HeartDisease)

k_fold.k_fold.result <- createFolds(k_fold.df$HeartDisease, k=10, returnTrain = FALSE, list = TRUE)
k_fold.best.result<-createFolds(best_subset.df$HeartDisease, k=10, returnTrain = FALSE, list = TRUE)
k_fold.bwd.result<-createFolds(bwd.df$HeartDisease, k=10, returnTrain = FALSE, list = TRUE)

#### model [1] : SVM model (Support Vector Machine) - Using k_fold.df ####

## 10-fold CV - linear ##
set.seed(1)
k_fold.linear.tune.out <- tune(svm, HeartDisease~., data=k_fold.df, kernel = 'linear', 
                               ranges=list(cost=c(0.1, 1, 10, 100, 1000)))

summary(k_fold.linear.tune.out) 
k_fold.linear.svm.bestmod <-k_fold.linear.tune.out$best.model
k_fold.linear.svm.bestmod

## SVM 모델 적합 + 10-fold 교차검증 + cv로 구한 최적의 cost 값 적용함.
str(k_fold.df)

auc_svm1 = accuracy_svm1 = specificity_svm1 = sensitivity_svm1 = numeric()

for (i in 1:10) { 
  set.seed(1)
  svm.fit1 <- svm(HeartDisease~., data = k_fold.df[-k_fold.k_fold.result[[i]],], kernel="linear", cost=0.1, scale=T)
  true <- as.numeric(k_fold.df$HeartDisease[k_fold.k_fold.result[[i]]])
  predict <- as.numeric(predict(svm.fit1, new = k_fold.df[k_fold.k_fold.result[[i]],]))
  
  mat = table(predict, true) # confusion matrix
  accuracy_svm1[i] = sum(true == predict) / sum(mat) # 정확도 : (TP + TN) / (TP + TN + FP + FN)
  sensitivity_svm1[i] = sum(mat[2,2]) / sum(mat[,2]) # 민감도 : (TP) / (TP + FP)
  specificity_svm1[i] = sum(mat[1,1]) / sum(mat[,1]) # 특이도 : (FN) / (FN + TN)
  auc_svm1[i] <- roc(true, predict)$auc
}

accuracy_svm1 ; cat("10-fold로 평가된 평균 Accuracy : ", mean(accuracy_svm1), " \n")
sensitivity_svm1 ; cat("10-fold로 평가된 평균 sensitivity : ", mean(sensitivity_svm1), " \n")
specificity_svm1 ; cat("10-fold로 평가된 평균 specificity : ", mean(specificity_svm1), " \n")
auc_svm1 ; cat("10-fold로 평가된 평균 auc : ", mean(auc_svm1), " \n")

## 10-fold CV - radial ##
set.seed(1)
k_fold.radial.tune.out <- tune(svm, HeartDisease~., data=k_fold.df, kernel = 'radial', 
                 ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1,2,3,4)))

summary(k_fold.radial.tune.out) 
k_fold.radial.svm.bestmod <-k_fold.radial.tune.out$best.model
k_fold.radial.svm.bestmod

auc_svm2 = accuracy_svm2 = specificity_svm2 = sensitivity_svm2 = numeric()

for (i in 1:10) { 
  set.seed(1)
  svm.fit <- svm(HeartDisease~., data = k_fold.df[-k_fold.k_fold.result[[i]],], kernel="radial", cost=1, scale=T)
  true <- as.numeric(k_fold.df$HeartDisease[k_fold.k_fold.result[[i]]])
  predict <- as.numeric(predict(svm.fit, new = k_fold.df[k_fold.k_fold.result[[i]],]))
  
  mat = table(predict, true) # confusion matrix
  accuracy_svm2[i] = sum(true == predict) / sum(mat) # 정확도 : (TP + TN) / (TP + TN + FP + FN)
  sensitivity_svm2[i] = sum(mat[2,2]) / sum(mat[,2]) # 민감도 : (TP) / (TP + FP)
  specificity_svm2[i] = sum(mat[1,1]) / sum(mat[,1]) # 특이도 : (FN) / (FN + TN)
  auc_svm2[i] <- roc(true, predict)$auc
}

accuracy_svm2 ; cat("10-fold로 평가된 평균 Accuracy : ", mean(accuracy_svm2), " \n")
sensitivity_svm2 ; cat("10-fold로 평가된 평균 sensitivity : ", mean(sensitivity_svm2), " \n")
specificity_svm2 ; cat("10-fold로 평가된 평균 specificity : ", mean(specificity_svm2), " \n")
auc_svm2 ; cat("10-fold로 평가된 평균 auc : ", mean(auc_svm2), " \n")

#### model [1] : SVM model (Support Vector Machine) - Using bwd.df ####

## 10-fold CV - linear ##
set.seed(1)

bwd.linear.tune.out <- tune(svm, HeartDisease~., data=bwd.df, kernel = 'linear', 
                               ranges=list(cost=c(0.1, 1, 10, 100, 1000)))

summary(bwd.linear.tune.out) 
bwd.linear.svm.bestmod <-bwd.linear.tune.out$best.model
bwd.linear.svm.bestmod


auc_svm3 = accuracy_svm3 = specificity_svm3 = sensitivity_svm3 = numeric()

for (i in 1:10) { 
  set.seed(1)
  svm.fit <- svm(HeartDisease~., data = bwd.df[-k_fold.bwd.result[[i]],], kernel="linear", cost=0.1, scale=T)
  true <- as.numeric(bwd.df$HeartDisease[k_fold.bwd.result[[i]]])
  predict <- as.numeric(predict(svm.fit, new = bwd.df[k_fold.bwd.result[[i]],]))
  
  mat = table(predict, true) # confusion matrix
  accuracy_svm3[i] = sum(true == predict) / sum(mat) # 정확도 : (TP + TN) / (TP + TN + FP + FN)
  sensitivity_svm3[i] = sum(mat[2,2]) / sum(mat[,2]) # 민감도 : (TP) / (TP + FP)
  specificity_svm3[i] = sum(mat[1,1]) / sum(mat[,1]) # 특이도 : (FN) / (FN + TN)
  auc_svm3[i] <- roc(true, predict)$auc
}

accuracy_svm3 ; cat("10-fold로 평가된 평균 Accuracy : ", mean(accuracy_svm3), " \n")
sensitivity_svm3 ; cat("10-fold로 평가된 평균 sensitivity : ", mean(sensitivity_svm3), " \n")
specificity_svm3 ; cat("10-fold로 평가된 평균 specificity : ", mean(specificity_svm3), " \n")
auc_svm3 ; cat("10-fold로 평가된 평균 auc : ", mean(auc_svm3), " \n")


## 10-fold CV - radial ##
set.seed(1)
bwd.radial.tune.out <- tune(svm, HeartDisease~., data=bwd.df, kernel = 'radial', 
                               ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1,2,3,4)))

summary(bwd.radial.tune.out) 
bwd.radial.svm.bestmod <-bwd.radial.tune.out$best.model
bwd.radial.svm.bestmod

auc_svm4 = accuracy_svm4 = specificity_svm4 = sensitivity_svm4 = numeric()

for (i in 1:10) { 
  set.seed(1)
  svm.fit <- svm(HeartDisease~., data = bwd.df[-k_fold.bwd.result[[i]],], kernel="radial", cost=0.1, scale=T)
  true <- as.numeric(bwd.df$HeartDisease[k_fold.bwd.result[[i]]])
  predict <- as.numeric(predict(svm.fit, new = bwd.df[k_fold.bwd.result[[i]],]))
  
  mat = table(predict, true) # confusion matrix
  accuracy_svm4[i] = sum(true == predict) / sum(mat) # 정확도 : (TP + TN) / (TP + TN + FP + FN)
  sensitivity_svm4[i] = sum(mat[2,2]) / sum(mat[,2]) # 민감도 : (TP) / (TP + FP)
  specificity_svm4[i] = sum(mat[1,1]) / sum(mat[,1]) # 특이도 : (FN) / (FN + TN)
  auc_svm4[i] <- roc(true, predict)$auc
}

accuracy_svm4 ; cat("10-fold로 평가된 평균 Accuracy : ", mean(accuracy_svm4), " \n")
sensitivity_svm4 ; cat("10-fold로 평가된 평균 sensitivity : ", mean(sensitivity_svm4), " \n")
specificity_svm4 ; cat("10-fold로 평가된 평균 specificity : ", mean(specificity_svm4), " \n")
auc_svm4 ; cat("10-fold로 평가된 평균 auc : ", mean(auc_svm4), " \n")

#### model [1] : SVM model (Support Vector Machine) - Using best_subset.df ####

## 10-fold CV - linear ##
set.seed(1)

best_subset.linear.tune.out <- tune(svm, HeartDisease~., data=best_subset.df, kernel = 'linear', 
                            ranges=list(cost=c(0.1, 1, 10, 100, 1000)))

summary(best_subset.linear.tune.out) 
best_subset.linear.svm.bestmod <-best_subset.linear.tune.out$best.model
best_subset.linear.svm.bestmod

auc_svm5 = accuracy_svm5 = specificity_svm5 = sensitivity_svm5 = numeric()

for (i in 1:10) { 
  set.seed(1)
  svm.fit <- svm(HeartDisease~., data = best_subset.df[-k_fold.best.result[[i]],], kernel="linear", cost=100, scale=T)
  true <- as.numeric(best_subset.df$HeartDisease[k_fold.best.result[[i]]])
  predict <- as.numeric(predict(svm.fit, new = best_subset.df[k_fold.best.result[[i]],]))
  
  mat = table(predict, true) # confusion matrix
  accuracy_svm5[i] = sum(true == predict) / sum(mat) # 정확도 : (TP + TN) / (TP + TN + FP + FN)
  sensitivity_svm5[i] = sum(mat[2,2]) / sum(mat[,2]) # 민감도 : (TP) / (TP + FP)
  specificity_svm5[i] = sum(mat[1,1]) / sum(mat[,1]) # 특이도 : (FN) / (FN + TN)
  auc_svm5[i] <- roc(true, predict)$auc
}

accuracy_svm5 ; cat("10-fold로 평가된 평균 Accuracy : ", mean(accuracy_svm5), " \n")
sensitivity_svm5 ; cat("10-fold로 평가된 평균 sensitivity : ", mean(sensitivity_svm5), " \n")
specificity_svm5 ; cat("10-fold로 평가된 평균 specificity : ", mean(specificity_svm5), " \n")
auc_svm5 ; cat("10-fold로 평가된 평균 auc : ", mean(auc_svm5), " \n")


## 10-fold CV - radial ##
set.seed(1)
best_subset.radial.tune.out <- tune(svm, HeartDisease~., data=best_subset.df, kernel = 'radial', 
                            ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1,2,3,4)))

summary(best_subset.radial.tune.out) 
best_subset.radial.svm.bestmod <-best_subset.radial.tune.out$best.model
best_subset.radial.svm.bestmod

auc_svm6 = accuracy_svm6 = specificity_svm6 = sensitivity_svm6 = numeric()

for (i in 1:10) { 
  set.seed(1)
  svm.fit <- svm(HeartDisease~., data = best_subset.df[-k_fold.best.result[[i]],], kernel="radial", cost=0.1, scale=T)
  true <- as.numeric(best_subset.df$HeartDisease[k_fold.best.result[[i]]])
  predict <- as.numeric(predict(svm.fit, new = best_subset.df[k_fold.best.result[[i]],]))
  
  mat = table(predict, true) # confusion matrix
  accuracy_svm6[i] = sum(true == predict) / sum(mat) # 정확도 : (TP + TN) / (TP + TN + FP + FN)
  sensitivity_svm6[i] = sum(mat[2,2]) / sum(mat[,2]) # 민감도 : (TP) / (TP + FP)
  specificity_svm6[i] = sum(mat[1,1]) / sum(mat[,1]) # 특이도 : (FN) / (FN + TN)
  auc_svm6[i] <- roc(true, predict)$auc
}

accuracy_svm6 ; cat("10-fold로 평가된 평균 Accuracy : ", mean(accuracy_svm6), " \n")
sensitivity_svm6 ; cat("10-fold로 평가된 평균 sensitivity : ", mean(sensitivity_svm6), " \n")
specificity_svm6 ; cat("10-fold로 평가된 평균 specificity : ", mean(specificity_svm6), " \n")
auc_svm6 ; cat("10-fold로 평가된 평균 auc : ", mean(auc_svm6), " \n")

#### model [2] : Random Forest - Using k_fold.df ####

## 10-fold (mtry(트리 적합 시 사용할 변수 개수) 선택)

k_fold.cv.rf <- rfcv(subset(k_fold.df, select=-c(HeartDisease)), k_fold.df$HeartDisease, cv.fold=10)
k_fold.cv.rf$error.cv
which.min(k_fold.cv.rf$error.cv)
par(mfrow=c(1,1))
with(k_fold.cv.rf, plot(n.var, error.cv, log="x", type="o"))

## RF 모델 적합 + 10-fold 교차검증 + rfcv로 구한 최적의 mtry 값 적용함.

auc_rf1 = accuracy_rf1 = specificity_rf1 = sensitivity_rf1 = numeric()

for (i in 1:10) { 
  set.seed(1)
  rf1.fit <- randomForest(HeartDisease~., data = k_fold.df[-k_fold.k_fold.result[[i]],], 
                          mtry=which.min(k_fold.cv.rf$error.cv), 
                          importance=T, proximity = TRUE)
  true <- as.numeric(k_fold.df$HeartDisease[k_fold.k_fold.result[[i]]])
  predict <- as.numeric(predict(rf1.fit, new = k_fold.df[k_fold.k_fold.result[[i]],]))
  
  mat = table(predict, true) # confusion matrix
  accuracy_rf1[i] = sum(true == predict) / sum(mat) # 정확도 : (TP + TN) / (TP + TN + FP + FN)
  sensitivity_rf1[i] = sum(mat[2,2]) / sum(mat[,2]) # 민감도 : (TP) / (TP + FP)
  specificity_rf1[i] = sum(mat[1,1]) / sum(mat[,1]) # 특이도 : (FN) / (FN + TN)
  auc_rf1[i] <- roc(true, predict)$auc # AUC
}

accuracy_rf1 ; cat("10-fold로 평가된 평균 Accuracy : ", mean(accuracy_rf1), " \n")
sensitivity_rf1 ; cat("10-fold로 평가된 평균 sensitivity : ", mean(sensitivity_rf1), " \n")
specificity_rf1 ; cat("10-fold로 평가된 평균 specificity : ", mean(specificity_rf1), " \n")
auc_rf1 ; cat("10-fold로 평가된 평균 auc : ", mean(auc_rf1), " \n")


#### model [2] : Random Forest - Using bwd.df ####

## 10-fold (mtry(트리 적합 시 사용할 변수 개수) 선택)

bwd.cv.rf <- rfcv(subset(bwd.df, select=-c(HeartDisease)), bwd.df$HeartDisease, cv.fold=10)
bwd.cv.rf$error.cv
which.min(bwd.cv.rf$error.cv)
par(mfrow=c(1,1))
with(bwd.cv.rf, plot(n.var, error.cv, log="x", type="o"))

## RF 모델 적합 + 10-fold 교차검증 + rfcv로 구한 최적의 mtry 값 적용함.

auc_rf2 = accuracy_rf2 = specificity_rf2 = sensitivity_rf2 = numeric()
for (i in 1:10) { 
  set.seed(1)
  rf2.fit <- randomForest(HeartDisease~., data = bwd.df[-k_fold.bwd.result[[i]],], 
                          mtry=which.min(bwd.cv.rf$error.cv), 
                          importance=T, proximity = TRUE)
  true <- as.numeric(bwd.df$HeartDisease[k_fold.bwd.result[[i]]])
  predict <- as.numeric(predict(rf2.fit, new = bwd.df[k_fold.bwd.result[[i]],]))
  
  mat = table(predict, true) # confusion matrix
  accuracy_rf2[i] = sum(true == predict) / sum(mat) # 정확도 : (TP + TN) / (TP + TN + FP + FN)
  sensitivity_rf2[i] = sum(mat[2,2]) / sum(mat[,2]) # 민감도 : (TP) / (TP + FP)
  specificity_rf2[i] = sum(mat[1,1]) / sum(mat[,1]) # 특이도 : (FN) / (FN + TN)
  auc_rf2[i] <- roc(true, predict)$auc # AUC
}

accuracy_rf2 ; cat("10-fold로 평가된 평균 Accuracy : ", mean(accuracy_rf2), " \n")
sensitivity_rf2 ; cat("10-fold로 평가된 평균 sensitivity : ", mean(sensitivity_rf2), " \n")
specificity_rf2 ; cat("10-fold로 평가된 평균 specificity : ", mean(specificity_rf2), " \n")
auc_rf2 ; cat("10-fold로 평가된 평균 auc : ", mean(auc_rf2), " \n")

#### model [2] : Random Forest - Using best_subset.df ####
best_subset.cv.rf <- rfcv(subset(best_subset.df, select=-c(HeartDisease)), 
                          best_subset.df$HeartDisease, cv.fold=10)
best_subset.cv.rf$error.cv
which.min(best_subset.cv.rf$error.cv)
par(mfrow=c(1,1))
with(best_subset.cv.rf, plot(n.var, error.cv, log="x", type="o"))

## RF 모델 적합 + 10-fold 교차검증 + rfcv로 구한 최적의 mtry 값 적용함.

auc_rf3 = accuracy_rf3 = specificity_rf3 = sensitivity_rf3 = numeric()
for (i in 1:10) { 
  set.seed(1)
  rf3.fit <- randomForest(HeartDisease~., data = best_subset.df[-k_fold.best.result[[i]],], 
                          mtry=which.min(best_subset.cv.rf$error.cv), 
                          importance=T, proximity = TRUE)
  true <- as.numeric(best_subset.df$HeartDisease[k_fold.best.result[[i]]])
  predict <- as.numeric(predict(rf3.fit, new = best_subset.df[k_fold.best.result[[i]],]))
  
  mat = table(predict, true) # confusion matrix
  accuracy_rf3[i] = sum(true == predict) / sum(mat) # 정확도 : (TP + TN) / (TP + TN + FP + FN)
  sensitivity_rf3[i] = sum(mat[2,2]) / sum(mat[,2]) # 민감도 : (TP) / (TP + FP)
  specificity_rf3[i] = sum(mat[1,1]) / sum(mat[,1]) # 특이도 : (FN) / (FN + TN)
  auc_rf3[i] <- roc(true, predict)$auc # AUC
}

accuracy_rf3 ; cat("10-fold로 평가된 평균 Accuracy : ", mean(accuracy_rf3), " \n")
sensitivity_rf3 ; cat("10-fold로 평가된 평균 sensitivity : ", mean(sensitivity_rf3), " \n")
specificity_rf3 ; cat("10-fold로 평가된 평균 specificity : ", mean(specificity_rf3), " \n")
auc_rf3 ; cat("10-fold로 평가된 평균 auc : ", mean(auc_rf3), " \n")

#### model [3] : Logistic Regression - Using k_fold.df ####

auc_glm1 = accuracy_glm1 = specificity_glm1 = sensitivity_glm1 = numeric()

for (i in 1:10) { 
  set.seed(1)
  glm1.fit <- glm(HeartDisease~., data = k_fold.df[-k_fold.k_fold.result[[i]],], family = binomial)
  
  glm1.true <- as.numeric(k_fold.df$HeartDisease[k_fold.k_fold.result[[i]]])
  glm1.probs <- as.numeric(predict(glm1.fit, new = k_fold.df[k_fold.k_fold.result[[i]],], type = "response"))
  
  glm1.pred <- rep("1", nrow(k_fold.df[k_fold.k_fold.result[[i]],]))
  glm1.pred[glm1.probs > 0.5] <- "2"
  glm1.pred <- as.numeric(glm1.pred)
  
  mat = table(glm1.pred, glm1.true) # confusion matrix
  accuracy_glm1[i] = sum(glm1.true == glm1.pred) / sum(mat) # 정확도 : (TP + TN) / (TP + TN + FP + FN)
  sensitivity_glm1[i] = sum(mat[2,2]) / sum(mat[,2]) # 민감도 : (TP) / (TP + FP)
  specificity_glm1[i] = sum(mat[1,1]) / sum(mat[,1]) # 특이도 : (FN) / (FN + TN)
  auc_glm1[i] <- roc(glm1.true, glm1.pred)$auc # AUC
  roc_glm1 <- roc(glm1.true, glm1.pred)
}

accuracy_glm1 ; cat("10-fold로 평가된 평균 Accuracy : ", mean(accuracy_glm1), " \n")
sensitivity_glm1 ; cat("10-fold로 평가된 평균 sensitivity : ", mean(sensitivity_glm1), " \n")
specificity_glm1 ; cat("10-fold로 평가된 평균 specificity : ", mean(specificity_glm1), " \n")
auc_glm1 ; cat("10-fold로 평가된 평균 auc : ", mean(auc_glm1), " \n")

#### model [3] : Logistic Regression - Using bwd.df ####

auc_glm2 = accuracy_glm2 = specificity_glm2 = sensitivity_glm2 = numeric()

for (i in 1:10) { 
  set.seed(1)
  glm2.fit <- glm(HeartDisease~., data = bwd.df[-k_fold.bwd.result[[i]],], family = binomial)
  
  glm2.true <- as.numeric(bwd.df$HeartDisease[k_fold.bwd.result[[i]]])
  glm2.probs <- as.numeric(predict(glm2.fit, new = bwd.df[k_fold.bwd.result[[i]],], type = "response"))
  
  glm2.pred <- rep("1", nrow(bwd.df[k_fold.bwd.result[[i]],]))
  glm2.pred[glm2.probs > 0.5] <- "2"
  glm2.pred <- as.numeric(glm2.pred)
  
  mat = table(glm2.pred, glm2.true) # confusion matrix
  accuracy_glm2[i] = sum(glm2.true == glm2.pred) / sum(mat) # 정확도 : (TP + TN) / (TP + TN + FP + FN)
  sensitivity_glm2[i] = sum(mat[2,2]) / sum(mat[,2]) # 민감도 : (TP) / (TP + FP)
  specificity_glm2[i] = sum(mat[1,1]) / sum(mat[,1]) # 특이도 : (FN) / (FN + TN)
  auc_glm2[i] <- roc(glm2.true, glm2.pred)$auc # AUC
}

accuracy_glm2 ; cat("10-fold로 평가된 평균 Accuracy : ", mean(accuracy_glm2), " \n")
sensitivity_glm2 ; cat("10-fold로 평가된 평균 sensitivity : ", mean(sensitivity_glm2), " \n")
specificity_glm2 ; cat("10-fold로 평가된 평균 specificity : ", mean(specificity_glm2), " \n")
auc_glm2 ; cat("10-fold로 평가된 평균 auc : ", mean(auc_glm2), " \n")


#### model [3] : Logistic Regression - Using best_subset.df ####

auc_glm3 = accuracy_glm3 = specificity_glm3 = sensitivity_glm3 = numeric()

for (i in 1:10) { 
  set.seed(1)
  glm3.fit <- glm(HeartDisease~., data = best_subset.df[-k_fold.best.result[[i]],], family = binomial)
  
  glm3.true <- as.numeric(best_subset.df$HeartDisease[k_fold.best.result[[i]]])
  glm3.probs <- as.numeric(predict(glm3.fit, new = best_subset.df[k_fold.best.result[[i]],], type = "response"))
  
  glm3.pred <- rep("1", nrow(best_subset.df[k_fold.best.result[[i]],]))
  glm3.pred[glm3.probs > 0.5] <- "2"
  glm3.pred <- as.numeric(glm3.pred)
  
  mat = table(glm3.pred, glm3.true) # confusion matrix
  accuracy_glm3[i] = sum(glm3.true == glm3.pred) / sum(mat) # 정확도 : (TP + TN) / (TP + TN + FP + FN)
  sensitivity_glm3[i] = sum(mat[2,2]) / sum(mat[,2]) # 민감도 : (TP) / (TP + FP)
  specificity_glm3[i] = sum(mat[1,1]) / sum(mat[,1]) # 특이도 : (FN) / (FN + TN)
  auc_glm3[i] <- roc(glm3.true, glm3.pred)$auc # AUC
}

accuracy_glm3 ; cat("10-fold로 평가된 평균 Accuracy : ", mean(accuracy_glm3), " \n")
sensitivity_glm3 ; cat("10-fold로 평가된 평균 sensitivity : ", mean(sensitivity_glm3), " \n")
specificity_glm3 ; cat("10-fold로 평가된 평균 specificity : ", mean(specificity_glm3), " \n")
auc_glm3 ; cat("10-fold로 평가된 평균 auc : ", mean(auc_glm3), " \n")

#### auc 가장 높은 Fold의 ROC curve 그리기 ####

true <- as.numeric(k_fold.df$HeartDisease[k_fold.k_fold.result[[9]]])

## SVM ROC ##
svm.fit1 <- svm(HeartDisease~., data = k_fold.df[-k_fold.k_fold.result[[9]],], kernel="linear", cost=0.1, scale=T)
svm.predict <- as.numeric(predict(svm.fit1, new = k_fold.df[k_fold.k_fold.result[[9]],]))
svm.roc1 <- roc(true, svm.predict)
summary(svm.roc1)

plot.roc(svm.roc1,   
         col="red",   
         print.auc=TRUE, print.auc.adj=c(-1.5,-1.5), max.auc.polygon=TRUE,  
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "red", print.thres.adj=c(-0.085,1.1),  
         auc.polygon=TRUE, auc.polygon.col="#D1F2EB", 
         main = "ROC curve by model")   

## RF ROC ##

rf1.fit <- randomForest(HeartDisease~., data = k_fold.df[-k_fold.k_fold.result[[9]],], 
                        mtry=which.min(k_fold.cv.rf$error.cv), 
                        importance=T, proximity = TRUE)
RF.predict <- as.numeric(predict(rf1.fit, new = k_fold.df[k_fold.k_fold.result[[9]],]))
RF.roc1 <- roc(true, RF.predict)

plot.roc(RF.roc1,   
         add=TRUE,   
         col="blue",   
         print.auc=TRUE, print.auc.adj=c(-1.5, 0),    
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "blue", print.thres.adj=c(-0.085,-1))   

## Logistic Regression ##


glm1.fit <- glm(HeartDisease~., data = k_fold.df[-k_fold.k_fold.result[[9]],], family = binomial)

glm1.probs <- as.numeric(predict(glm1.fit, new = k_fold.df[k_fold.k_fold.result[[9]],], type = "response"))

glm1.pred <- rep("1", nrow(k_fold.df[k_fold.k_fold.result[[9]],]))
glm1.pred[glm1.probs > 0.5] <- "2"
glm1.pred <- as.numeric(glm1.pred)

glm.roc1 <- roc(true, glm1.pred)

plot.roc(glm.roc1,   
         add=TRUE,   
         col="purple",   
         print.auc=TRUE, print.auc.adj=c(-1.5, 1.5),    
         print.thres=TRUE, print.thres.pch=19, print.thres.col = "purple", print.thres.adj=c(-0.085,3))    

legend("bottomright",  
       legend=c("SVM", "RandomForest", "Logistic Regression"),   
       col=c("red", "blue", "purple"), lwd=2)  
