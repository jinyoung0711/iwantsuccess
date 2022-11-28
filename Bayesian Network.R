install.packages("bnlearn")

library(bnlearn)

df <- read.csv("train.csv")
df

str(df)

# 변수 factor화
df$Sex <- as.factor(df$Sex)
df$Age <- as.numeric(df$Age)
df$ChestPainType <- as.factor(df$ChestPainType)
df$FastingBS <- as.factor(df$FastingBS)
df$RestingECG <- as.factor(df$RestingECG)
df$ExerciseAngina <- as.factor(df$ExerciseAngina)
df$ST_Slope <- as.factor(df$ST_Slope)
df$HeartDisease <- as.factor(df$HeartDisease)

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
