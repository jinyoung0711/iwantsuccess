#
df<-read.csv("./heart.csv", header=T)
head(df)
dim(df)
sum(is.na(df)) #결측치 없음.
str(df)
# Sex, ChestPainType, FastingBS, RestingECG, 
# ExerciseAngina, ST_Slope, Heartdisease -> 범주형변수
summary(df)

### 1. EDA

library(ggplot2)

col1=adjustcolor("red", 0.4) ; col2=adjustcolor("blue", 0.4)
col12=c(col1,col2)

# 1. Age (나이)
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


# 2. Sex (성별)
table(df$HeartDisease, df$Sex)

ggplot(df, aes(x=factor(HeartDisease), fill=Sex))+
  geom_bar(position="dodge")+
  ggtitle("Plot of Sex by HeartDisease\n")+
  theme(plot.title = element_text(hjust = 0.5))
### 918개의 관측치 중 여성 관측치는 193개로, 전체 관측치의 21%에 해당함.
### 여성의 경우, 환자가 아닌 사람 수가 환자 수보다 약 3배 많고
### 남성의 경우, 환자 수가 환자가 아닌 사람 수보다 약 2배 많음.

# 3. ChestPainType (가슴 통증 종류)
table(df$HeartDisease, df$ChestPainType)

ggplot(df, aes(x=factor(HeartDisease), fill=ChestPainType))+
  geom_bar(position="dodge")+
  ggtitle("Plot of ChestPainType by HeartDisease\n")+
  theme(plot.title = element_text(hjust = 0.5))
### ChestPainType이 ASY인 경우가 전체 관측치의 54%에 해당하며
### ASY인 경우만, 환자의 수가 환자가 아닌 사람의 수보다 많고, 4배 가까이 많음.

# 4. RestingBP (안정 시 혈압)
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


# 5. Cholesterol (콜레스테롤 수치 mg/dl)
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


# 6. FastingBS (공복혈당 > 120 mg/dl (T, F))
table(df$HeartDisease, df$FastingBS)
ggplot(df, aes(x=factor(HeartDisease), fill=factor(FastingBS)))+
  geom_bar(position="dodge")+
  ggtitle("Plot of FastingBS by HeartDisease\n")+
  theme(plot.title = element_text(hjust = 0.5))
### FastingBS == 1 ->  if FastingBS > 120 mg/dl
### FastingBS가 1인 경우 환자의 수가 환자가 아닌 사람 수보다 약 4배 더 많음.

# 7. RestingECG (안정시 심전도 결과)
table(df$HeartDisease, df$RestingECG)
ggplot(df, aes(x=factor(HeartDisease), fill=RestingECG))+
  geom_bar(position="dodge")+
  ggtitle("Plot of RestingECG by HeartDisease\n")+
  theme(plot.title = element_text(hjust = 0.5))
### LVH, Normal, ST 모두 환자가 아닌 사람 수보다 환자의 수가 많으며, 
### RestingECG에 따른 HeartDisease의 뚜렷한 변화가 관찰되지 않음.

# 8. MaxHR (최대심박수)
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


# 9. ExerciseAngina (운동으로 인한 협심증 여부)
table(df$HeartDisease, df$ExerciseAngina)
ggplot(df, aes(x=factor(HeartDisease), fill=ExerciseAngina))+
  geom_bar(position="dodge")+
  ggtitle("Plot of ExerciseAngina by HeartDisease\n")+
  theme(plot.title = element_text(hjust = 0.5))

### ExerciseAngina가 N인(없는) 경우는 환자가 아닌 사람이 환자에 비해 2배 가까이 많고
### ExerciseAngina가 Y인(있는) 경우는 환자가 환자가 아닌 사람에 비해 약 6배 많음.
### -> 상관성 높다고 볼 수 있음.


# 10. Oldpeak(안정과 관련된 ST감소)
hist(df$Oldpeak, breaks=30, main="Histogram of Oldpeak")
boxplot(df$Oldpeak,main="Boxplot of Oldpeak")
sum(df$Oldpeak==0) ; sum(df$Oldpeak==0) / nrow(df)
### Oldpeak의 값이 0인 관측치가 368개임. (전체 관측치가 918개) -> 40% 차지함.
### Oldpeak가 0인 것, 0이 아닌 것으로 나누어, 범주형 변수로의 변환도 가능할 것 같음.



# 11. ST_Slope (the slope of the peak exercise ST segment)
table(df$HeartDisease, df$ST_Slope)
ggplot(df, aes(x=factor(HeartDisease), fill=ST_Slope))+
  geom_bar(position="dodge")+
  ggtitle("Plot of ST_Slope by HeartDisease\n")+
  theme(plot.title = element_text(hjust = 0.5))

### up일 때의 HeartDisease 값 분포가 다른 경우들(Down, Flat)과 
### 반대 양상을 보임을 확인할 수 있음.

# 12. HeartDisease
table(df$HeartDisease)

# label encoding
df$Sex<-as.numeric(factor(df$Sex, levels=unique(df$Sex)))
df$ChestPainType<-as.numeric(factor(df$ChestPainType, levels=unique(df$ChestPainType)))
df$FastingBS<-as.numeric(factor(df$FastingBS, levels=unique(df$FastingBS)))
df$RestingECG<-as.numeric(factor(df$RestingECG, unique(df$RestingECG)))
df$ExerciseAngina<-as.numeric(factor(df$ExerciseAngina), unique(df$ExerciseAngina))
df$ST_Slope<-as.numeric(factor(df$ST_Slope), unique(df$ST_Slope))
df$HeartDisease<-as.numeric(factor(df$HeartDisease), unique(df$HeartDisease))

'''
df$Sex <- as.numeric(as.factor(df$Sex)) # 1 : F, 2 : M
df$ChestPainType <- as.numeric(as.factor(df$ChestPainType)) # 1 : ASY, 2 : ATA, 3 : NAP, 4 : T
df$RestingECG <- as.numeric(as.factor(df$RestingECG)) # 1 : LVH, 2 : Normal, 3 : ST
df$ExerciseAngina <- as.numeric(as.factor(df$ExerciseAngina)) # 1: N, 2 : Y
df$ST_Slope <- as.numeric(as.factor(df$ST_Slope)) # 1 : Down, 2 : Flat, 3 : Up
df$HeartDisease <- as.factor(df$HeartDisease)
'''

# 산점도
# install.packages("GGally")
library("GGally")
fun_lower <- function(x, y) {
  usr <- par("usr"); on.exit(par(usr))
  # The par(“usr”) command is especially useful when creating custom “fixed” features in a plot.
  par(usr = c(0, 1, 0, 1))
  # cex(글자 크기) 상관계수 절대값에 비례하도록 조정
  text( 0.5, 0.5, round(cor(x, y),2), 
        cex = abs(round(cor(x, y),2))+1,
        col = adjustcolor("blue", alpha.f=abs(round(cor(x, y),2))+0.4))
}
pairs(df, lower.panel=fun_lower)

#ggpairs(df)

#### 2. 전처리 ####

## IQR_function
IQR_fun <- function(x) {
  UpperQ = fivenum(x)[4]
  LowerQ = fivenum(x)[2]
  IQR = UpperQ - LowerQ
  
  upperOutlier = x[ which( x > UpperQ + IQR*1.5) ]
  lowerOutlier = x[ which( x < LowerQ - IQR*1.5) ]
  
  df <<- df[!((x > UpperQ + IQR*1.5) | ( x < LowerQ - IQR*1.5)), ]
  return(length(upperOutlier) + length(lowerOutlier)) # 이상치로 판정되는 데이터의 개수를 의미
}

### [Drop RestingBP Outlier] ###

df <- df[!(df$RestingBP == 0 ), ] # 918개 중 1개 제거 -> 917개 
sum(df$RestingBP==0); sum(df$RestingBP==0)

IQR_fun(df$RestingBP) # 917개 중 27개 Outlier 제거 -> 890개
str(df$RestingBP); str(df$RestingBP)

par(mfrow=c(1,2))

boxplot(RestingBP ~ HeartDisease, df, col=col12, 
        main="Plot of RestingBP by HeartDisease\n")
legend("topright", legend=unique(df$HeartDisease), fill=col12) 

# [Drop Cholesterol Outlier] (콜레스테롤 수치 mg/dl)
df <- df[!(df$Cholesterol == 0 ), ] # 164개 데이터 제거 완료 -> 726개 데이터 존재
sum(df$Cholesterol==0)

IQR_fun(df$Cholesterol) # 22개 Outlier 제거 -> 704개 데이터 존재
str(df$Cholesterol)

boxplot(Cholesterol ~ HeartDisease, df, col=col12, 
        main="Plot of Cholesterol by HeartDisease\n")
legend("topright", legend=unique(df$HeartDisease), fill=col12) 

## [Drop MaxHR Outlier]
IQR_fun(df$MaxHR) # 0개 Outlier 제거
str(df$MaxHR)

## [Drop Oldpeak Outlier]
IQR_fun(df$Oldpeak) # 12개 Outlier 제거 -> 692개 데이터 존재
str(df$Oldpeak)

boxplot(Oldpeak ~ HeartDisease, df, col=col12, 
        main="Plot of Oldpeak by HeartDisease\n")
legend("topright", legend=unique(df$HeartDisease), fill=col12) 

### 3-1. 모델 구축

#### Model selection using K-fold CV ####
k <- 10 
set.seed(1)
folds <- base::sample(1:k, nrow(df), replace=T)
table(folds)
str(df)
cv.error <- matrix(0, k, 11)
colnames(cv.error) <- paste(1:11)
cv.error
library(e1071)
library(leaps)
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
plot(mean.cv.error, type='b')

reg.best <- regsubsets(HeartDisease~., data=df, nvmax=11)
coef(reg.best, 8)
str(df)

'''
# Normalization #
normal.fun <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

df$Cholesterol <- normal.fun(df$Cholesterol)
df$MaxHR <- normal.fun(df$MaxHR)
df$Oldpeak <- normal.fun(df$Oldpeak)
df$Age <- normal.fun(df$Age)
df$HeartDisease <- as.factor(df$HeartDisease)
head(df[1])
str(df)
'''

#### SVM model ####

df2 <- subset(df, select=-c(RestingBP, FastingBS, RestingECG))
df2$HeartDisease <- as.factor(df2$HeartDisease)
# Support Vector Machine
library(e1071)

svmfit <- svm(HeartDisease ~ ., data = df2, kernel = 'radial', cost=10, gamma = 0.5, scale=F)
plot(svmfit, df2)
svmfit

svmfit$index

# 10-fold CV
set.seed(1)
tune.out <- tune(svm, HeartDisease~., data=df2, kernel = 'radial', 
                 ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1,2,3,4)))

summary(tune.out) 
bestmod <-tune.out$best.model
bestmodS

# 여기부터 test 나눠서 해야함
ypred <- predict(bestmod, newdata = testdf)
table(ypred, testdf$y)
