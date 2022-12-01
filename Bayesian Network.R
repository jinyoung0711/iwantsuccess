install.packages("bnlearn")

library(bnlearn)

df <- read.csv("train.csv")
df

str(df)

# 이상치 처리

## [Drop RestingBP Outlier] ##
df <- df[!(df$RestingBP == 0 ), ] # 818개 중 1개 제거 -> 817개 
sum(df$RestingBP==0); 

## [Drop Cholesterol Outlier] (콜레스테롤 수치 mg/dl) ##
df <- df[!(df$Cholesterol == 0 ), ] # 817개 중 171개 데이터 제거 완료 -> 646개 데이터 존재
sum(df$Cholesterol==0)

## [Drop Oldpeak Outlier]
df$Oldpeak <- ifelse(df$Oldpeak >= 0, df$Oldpeak, NA)
na.omit(df$Oldpeak)
# 변수 factor화
df$Sex <- as.factor(df$Sex)
df$ChestPainType <- as.factor(df$ChestPainType)
df$FastingBS <- as.factor(df$FastingBS)
df$RestingECG <- as.factor(df$RestingECG)
df$ExerciseAngina <- as.factor(df$ExerciseAngina)
df$ST_Slope <- as.factor(df$ST_Slope)
df$HeartDisease <- as.factor(df$HeartDisease)

df$Age <- as.numeric(df$Age)
df$RestingBP <- as.numeric(df$RestingBP)
df$Cholesterol <- as.numeric(df$Cholesterol)
df$MaxHR <- as.numeric(df$MaxHR)
df$Oldpeak <- as.numeric(df$Oldpeak)

str(df)

# Bayesian Network

dag <- gs(df)
dag

plot(dag)

dag <- set.arc(dag, "Sex", "HeartDisease") # 방향성 제공 Sex -> HeartDisease
dag <- set.arc(dag, "ChestPainType", "ExerciseAngina") # 방향성 제공 Sex -> HeartDisease
dag <- set.arc(dag, "ST_Slope", "HeartDisease") # 방향성 제공 Sex -> HeartDisease

fit <- bn.fit(dag, df) 
fit

# 연속형 변수 시각화 (Histogram of the Residuals for Node)
# bn.fit.xyplot, bn.fit.qqplot
bn.fit.histogram(fit$Age)
bn.fit.histogram(fit$RestingBP)
bn.fit.histogram(fit$Cholesterol)
bn.fit.histogram(fit$MaxHR)
bn.fit.histogram(fit$Oldpeak)

# 범주형 변수 시각화 (Conditional Probablity for Node)
# bn.fit.dotplot
bn.fit.barchart(fit$Sex)
bn.fit.barchart(fit$ChestPainType)
bn.fit.barchart(fit$FastingBS)
bn.fit.barchart(fit$RestingECG)
bn.fit.barchart(fit$ExerciseAngina)
bn.fit.barchart(fit$ST_Slope)
bn.fit.barchart(fit$HeartDisease)

# 모든 변수 범주화 버전
# Age -> bin
str(df)
df2 <- df
summary(df2$Age)
df2$Age <- cut(x=df2$Age, breaks=c(0, 47, 55, 60, 78), right=F)
plot(df2$Age)

plot(df2$Cholesterol)

hist(df2$Cholesterol)
summary(df2$Cholesterol)
df2$Cholesterol <- cut(x=df2$Cholesterol, breaks=c(0, 208, 238, 276, 604), right=F)
str(df2)

df2$RestingBP <- cut(x = df2$RestingBP, breaks = c(0, 121, 131, 141, 201))

# MaxHR : 분위수 대로 범주화
df2$MaxHR <- cut(x=df2$MaxHR, breaks = c(0, 121, 141, 158, 196))

# Oldpeak : 0과 0이 아닌값으로 범주화
df2$Oldpeak <- ifelse(df2$Oldpeak == 0, 0, 1)
df2$Oldpeak <- as.factor(df2$Oldpeak)

summary(df$Oldpeak)

str(df2)

# Bayesian Network

dag <- gs(df2)
dag

plot(dag)

dag <- set.arc(dag, "Sex", "MaxHR") # 방향성 제공 Sex -> MaxHR
dag <- set.arc(dag, "Oldpeak", "Age") # 방향성 제공 Oldpeak -> Age
dag <- set.arc(dag, "ST_Slope", "HeartDisease") # 방향성 제공 Sex -> HeartDisease

fit <- bn.fit(dag, df) 
fit
