# working directory setting
getwd()
setwd("C:/Users/82106/R-coding")

# loading librry
library(ggplot2)
library(stringr)
library(naniar)
library(corrplot)
library(randomForest)
library(dplyr)
library(glmnet)
library(caret)
library(ROCR)
library(Epi)
library(xgboost)
library(tidyverse)
library(caret)
library(pROC)
library(mice)

# read data
d1 <- read.table("phenotype.txt", sep="\t", header=T)
d2 <- read.csv("KCPS_r2_0.01.csv")
str(d1)
str(d2)

# 결측치 확인
colSums(is.na(d1))
colSums(is.na(d2))
sum(colSums(is.na(d1)))
sum(colSums(is.na(d2)))

naniar::gg_miss_var(d1)
gg_miss_upset(d1, nsets = 10)

# First Data Preprocessing
sum(d1$FID != d1$IID)
d1 <- subset(d1, select=-c(IID))
names(d1)[1] <- c("ID")

d1$SMOK_B <- ifelse(!is.na(d1$SMOK_B), d1$SMOK_B, round(mean(d1$SMOK_B, na.rm=T))) 

length(which(d1$SMOK_B == 1 & is.na(d1$SMOKA_MOD_B)))
length(which(d1$SMOK_B == 2 & is.na(d1$SMOKA_MOD_B)))
length(which(d1$SMOK_B == 3 & is.na(d1$SMOKA_MOD_B)))

d1$SMOKA_MOD_B <- ifelse(d1$SMOK_B == 1 & is.na(d1$SMOKA_MOD_B), 0, d1$SMOKA_MOD_B)
d1$SMOKA_MOD_B <- ifelse(d1$SMOK_B == 2 & is.na(d1$SMOKA_MOD_B), round(mean(d1$SMOKA_MOD_B[d1$SMOK_B==2], na.rm=T)), d1$SMOKA_MOD_B) 
d1$SMOKA_MOD_B <- ifelse(d1$SMOK_B == 3 & is.na(d1$SMOKA_MOD_B), round(mean(d1$SMOKA_MOD_B[d1$SMOK_B==3], na.rm=T)), d1$SMOKA_MOD_B) 
d1 <- subset(d1, select=-c(SMOK_B))

x1= table(d1$ALCO_B)
x1
i1 = which.max(as.vector(x1))
i1

d1$ALCO_B <- ifelse(!is.na(d1$ALCO_B), d1$ALCO_B, i1) 

d1$ALCO_AMOUNT_B <- ifelse(d1$ALCO_B == 1 & is.na(d1$ALCO_AMOUNT_B), round(mean(d1$ALCO_AMOUNT_B[d1$ALCO_B==1], na.rm=T), 2), d1$ALCO_AMOUNT_B) # 276개의 이상치, 0로 대체
d1$ALCO_AMOUNT_B <- ifelse(d1$ALCO_B == 2 & is.na(d1$ALCO_AMOUNT_B), 0, d1$ALCO_AMOUNT_B) 
d1 <- subset(d1, select=-c(ALCO_B))

x2= table(d1$EXER_B)
x2
i2 = which.max(as.vector(x1))
i2
d1$EXER_B <- ifelse(!is.na(d1$EXER_B), d1$EXER_B, i2) 

d1 <- subset(d1, select=-c(MDM_B))
d1 <- subset(d1, select=-c(MHTN_B))
d1 <- subset(d1, select=-c(MLPD_B))
d1 <- subset(d1, select=-c(PHTN_B))
d1 <- subset(d1, select=-c(PDM_B))
d1 <- subset(d1, select=-c(PLPD_B))

d1$HT_B <- ifelse(!is.na(d1$HT_B), d1$HT_B, round(mean(d1$HT_B, na.rm=T))) 
d1$WT_B <- ifelse(!is.na(d1$WT_B), d1$WT_B, round(mean(d1$WT_B, na.rm=T))) 
d1$WAIST_B <- ifelse(!is.na(d1$WAIST_B), d1$WAIST_B, round(mean(d1$WAIST_B, na.rm=T))) 

d1$ABSI_B <- (d1$WAIST_B * 0.3937) / (d1$WT_B/(d1$HT_B/100)^2) * 2/3 * (d1$HT_B/100) * 1/2
d1 <- subset(d1, select=-c(HT_B))
d1 <- subset(d1, select=-c(WT_B))
d1 <- subset(d1, select=-c(WAIST_B))

d1$SBP_B <- ifelse(!is.na(d1$SBP_B), d1$SBP_B, round(mean(d1$SBP_B, na.rm=T))) 
d1$DBP_B <- ifelse(!is.na(d1$DBP_B), d1$DBP_B, round(mean(d1$DBP_B, na.rm=T))) 
d1$HBP_B <- ifelse((d1$SBP_B >= 140 & d1$DBP_B >= 90), 1, 0) # 고혈압
d1$LBP_B <- ifelse((d1$SBP_B <= 90 & d1$DBP_B <= 60), 1, 0) # 저혈압 
d1$BP_B <- ifelse((d1$HBP == 1) | (d1$LBP == 1) , 1, 0) # 혈압 정상 / 비정상
d1 <- subset(d1, select=-c(SBP_B))
d1 <- subset(d1, select=-c(DBP_B))
d1 <- subset(d1, select=-c(HBP_B))
d1 <- subset(d1, select=-c(LBP_B))

d1$CHO_B <- ifelse(!is.na(d1$CHO_B), d1$CHO_B, round(mean(d1$CHO_B, na.rm=T))) 
d1$LDL_B <- ifelse(!is.na(d1$LDL_B), d1$LDL_B, round(mean(d1$LDL_B, na.rm=T))) 
d1$HDL_B <- ifelse(!is.na(d1$HDL_B), d1$HDL_B, round(mean(d1$HDL_B, na.rm=T))) 
d1$TG_B <- ifelse(!is.na(d1$TG_B), d1$TG_B, round(mean(d1$TG_B, na.rm=T))) 

d1$temp1 <- ifelse(d1$CHO_B < 200, 0, 1) # 총 콜레스테롤 정상기준 : 200미만
d1$temp2 <- ifelse(d1$LDL_B < 130, 0, 1) # LDL 콜레스테롤 정상기준 : 130미만 
d1$temp3 <- ifelse(d1$HDL >= 40, 0, 1) # HDL 콜레스테롤 정상 기준 : 40 이상
d1$temp4 <- ifelse(d1$TG_B < 200, 0, 1) # TG 중성지방 정상 기준 : 200 미만

d1$CVH <- rowSums(select(d1, "temp1", "temp2", "temp3", "temp4")) # 콜레스테롤에 의한 건강 여부 (0에 근접할 수록 정상)

d1 <- subset(d1, select=-c(CHO_B))
d1 <- subset(d1, select=-c(LDL_B))
d1 <- subset(d1, select=-c(HDL_B))
d1 <- subset(d1, select=-c(TG_B))
d1 <- subset(d1, select=-c(temp1))
d1 <- subset(d1, select=-c(temp2))
d1 <- subset(d1, select=-c(temp3))
d1 <- subset(d1, select=-c(temp4))

d1$HDL_B <- ifelse(!is.na(d1$HDL_B), d1$HDL_B, round(mean(d1$HDL_B, na.rm=T))) 
d1$FBS_B <- ifelse(!is.na(d1$FBS_B), d1$FBS_B, round(mean(d1$FBS_B, na.rm=T))) 
d1$GOT_B <- ifelse(!is.na(d1$GOT_B), d1$GOT_B, round(mean(d1$GOT_B, na.rm=T))) 
d1$GPT_B <- ifelse(!is.na(d1$GPT_B), d1$GPT_B, round(mean(d1$GPT_B, na.rm=T))) 
d1$GGT_B <- ifelse(!is.na(d1$GGT_B), d1$GGT_B, round(mean(d1$GGT_B, na.rm=T))) 
d1$URIC_B <- ifelse(!is.na(d1$URIC_B), d1$URIC_B, round(mean(d1$URIC_B, na.rm=T))) 
d1$BIL <- ifelse(!is.na(d1$BIL), d1$BIL, round(mean(d1$BIL, na.rm=T),2)) 

d1$temp1 <- ifelse(d1$GOT_B > 40, 1, 0) # GOP 정상수치 : 0~40 IU/L
d1$temp2 <- ifelse(d1$GPT_B > 40, 1, 0) # GPT 정상수치 : 0~40 IU/L
d1$temp3 <- ifelse((d1$GGT_B > 55 & d1$SEX1 == 1), 1, 0) # GGT 정상수치 
d1$temp4 <- ifelse((d1$GGT_B > 40 & d1$SEX1 == 2), 1, 0) # 남성 0~55 IU/L, 여성 0~40 IU/L
d1$temp5 <- ifelse((d1$BIL < 1.0 & d1$BIL > 0.2), 1, 0) # 총 빌리루빈 정상수치 0.2 ~ 1.0 IU/L
d1$LFD <- rowSums(select(d1, "temp1", "temp2", "temp3", "temp4", "temp5")) # 간기능 이상에 대한 변수 (0일수록 정상)

d1 <- subset(d1, select=-c(GOT_B))
d1 <- subset(d1, select=-c(GPT_B))
d1 <- subset(d1, select=-c(GGT_B))
d1 <- subset(d1, select=-c(BIL))
d1 <- subset(d1, select=-c(temp1))
d1 <- subset(d1, select=-c(temp2))
d1 <- subset(d1, select=-c(temp3))
d1 <- subset(d1, select=-c(temp4))
d1 <- subset(d1, select=-c(temp5))

d1$PCAN80 <- ifelse(!is.na(d1$PCAN80), d1$PCAN80, 0) 
d1$PCAN81 <- ifelse(!is.na(d1$PCAN81), d1$PCAN81, 0) 
d1$PCAN82 <- ifelse(!is.na(d1$PCAN82), d1$PCAN82, 0) 
d1$PCAN83 <- ifelse(!is.na(d1$PCAN83), d1$PCAN83, 0) 
d1$PCAN84 <- ifelse(!is.na(d1$PCAN84), d1$PCAN84, 0) 
d1$PCAN86 <- ifelse(!is.na(d1$PCAN86), d1$PCAN86, 0) 
d1$PCAN89 <- ifelse(!is.na(d1$PCAN89), d1$PCAN89, 0) 

d1$PAR <- rowSums(select(d1, starts_with("PCAN")))

d1 <- subset(d1, select=-c(PCAN80))
d1 <- subset(d1, select=-c(PCAN81))
d1 <- subset(d1, select=-c(PCAN82))
d1 <- subset(d1, select=-c(PCAN83))
d1 <- subset(d1, select=-c(PCAN84))
d1 <- subset(d1, select=-c(PCAN86))
d1 <- subset(d1, select=-c(PCAN89))

d1$FCAN80 <- ifelse(!is.na(d1$FCAN80), d1$FCAN80, 0) 
d1$FCAN81 <- ifelse(!is.na(d1$FCAN81), d1$FCAN81, 0) 
d1$FCAN82 <- ifelse(!is.na(d1$FCAN82), d1$FCAN82, 0)
d1$FCAN83 <- ifelse(!is.na(d1$FCAN83), d1$FCAN83, 0) 
d1$FCAN84 <- ifelse(!is.na(d1$FCAN84), d1$FCAN84, 0) 
d1$FCAN86 <- ifelse(!is.na(d1$FCAN86), d1$FCAN86, 0) 
d1$FCAN89 <- ifelse(!is.na(d1$FCAN89), d1$FCAN89, 0) 

d1$FAM <- rowSums(select(d1, starts_with("FCAN")))

d1 <- subset(d1, select=-c(FCAN80))
d1 <- subset(d1, select=-c(FCAN81))
d1 <- subset(d1, select=-c(FCAN82))
d1 <- subset(d1, select=-c(FCAN83))
d1 <- subset(d1, select=-c(FCAN84))
d1 <- subset(d1, select=-c(FCAN86))
d1 <- subset(d1, select=-c(FCAN89))

d1$Influenced <- rowSums(select(d1, "FAM", "PAR"))
d1 <- subset(d1, select=-c(FAM))
d1 <- subset(d1, select=-c(PAR))

sum(is.na(d1$FEV1))
sum(is.na(d1$FVC))
sum(is.na(d1$FEV1) & is.na(d1$FVC))

d1$FF <- d1$FEV1/d1$FVC
d1$FF <- ifelse(!is.na(d1$FF), d1$FF, round(mean(d1$FF, na.rm=T),2)) 
d1 <- subset(d1, select=-c(FEV1))
d1 <- subset(d1, select=-c(FVC))


d1$WBC <- ifelse(!is.na(d1$WBC), d1$WBC, round(mean(d1$WBC, na.rm=T),2))
d1$CREAT <- ifelse(!is.na(d1$CREAT), d1$CREAT, round(mean(d1$CREAT, na.rm=T),2)) 

d1 <- subset(d1, select=-c(CRC))
d1 <- subset(d1, select=-c(SCRC))

colSums(is.na(d1))

sum(d2$FID != d2$IID)

d2 <- subset(d2, select=-c(IID))
names(d2)[1] <- c("ID")

d0 <- merge(d1, d2, by=c('ID'))
str(d0)

# STOMA & phenotype correlation

d0_STOMA1 <- d0[, 2:9]
d0_STOMA1$STOMA <- d0$STOMA
d0_STOMA1_cor <- cor(d0_STOMA1)
corrplot(d0_STOMA1_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black") # AGE_B ~ 0.15 , SMOKA_MOD_B ~ 0.05 STOMA corr,

d0_STOMA2 <- d0[,26:32]
d0_STOMA2$STOMA <- d0$STOMA
d0_STOMA2_cor <- cor(d0_STOMA2)
corrplot(d0_STOMA2_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black") # SEX1 ~ -0.07, ABSI ~ 0.03, Influenced ~ 0.09 STOMA corr,

# COLON & phenotype correlation

d0_COLON1 <- d0[, 2:9]
d0_COLON1$COLON <- d0$COLON
d0_COLON1_cor <- cor(d0_COLON1)
corrplot(d0_COLON1_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black") # AGE_B ~ 0.09, FBS_B ~ 0.03 COLON corr,

d0_COLON2 <- d0[,26:32]
d0_COLON2$COLON <- d0$COLON
d0_COLON2_cor <- cor(d0_COLON2)
corrplot(d0_COLON2_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black") # BP_B ~ 0.02, CVH ~ 0.02, Influenced ~ 0.04 COLON corr,

# LIVER & phenotype correlation

d0_LIVER1 <- d0[, 2:9]
d0_LIVER1$LIVER <- d0$LIVER
d0_LIVER1_cor <- cor(d0_LIVER1)
corrplot(d0_LIVER1_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black") # AGE_B ~ 0.07 , SMOKA_MOD_B ~ 0.03, CREAT ~ 0.04 LIVER corr,

d0_LIVER2 <- d0[,26:32]
d0_LIVER2$LIVER <- d0$LIVER
d0_LIVER2_cor <- cor(d0_LIVER2)
corrplot(d0_LIVER2_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", # SEX1 ~ -0.07 , ABSI_B ~ 0.05, LFD ~ 0.13 LIVER corr,
         tl.srt=40, diag=FALSE, addCoef.col="black")

# LUNG & phenotype correlation

d0_LUNG1 <- d0[, 2:9]
d0_LUNG1$LUNG <- d0$LUNG
d0_LUNG1_cor <- cor(d0_LUNG1)
corrplot(d0_LUNG1_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black") # AGE_B ~ 0.16 , SMOKA_MOD_B ~ 0.07 LUNG corr,

d0_LUNG2 <- d0[,26:32]
d0_LUNG2$LUNG <- d0$LUNG
d0_LUNG2_cor <- cor(d0_LUNG2)
corrplot(d0_LIVER2_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black") # SEX ~ -0.07 , ABSI_B ~ 0.05, LFD ~ 0.13 LUNG corr,

# PROST & phenotype correlation

d0_PROST1 <- d0[, 2:9]
d0_PROST1$PROST <- d0$PROST
d0_PROST1_cor <- cor(d0_PROST1)
corrplot(d0_PROST1_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black") # AGE_B ~ 0.22 , SMOKA_MOD_B ~ 0.05, FBS_B ~ 0.05, CREAT ~ 0.06 PROST corr,

d0_PROST2 <- d0[,26:32]
d0_PROST2$PROST <- d0$PROST
d0_PROST2_cor <- cor(d0_PROST2)
corrplot(d0_PROST2_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black") # SEX1 ~ -0.13 , ABSI_B ~ 0.06, Influenced ~ -0.04 PROST corr,

# THROI & phenotype correlation

d0_THROI1 <- d0[, 2:9]
d0_THROI1$THROI <- d0$THROI
d0_THROI1_cor <- cor(d0_THROI1)
corrplot(d0_THROI1_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black") # AGE_B ~ -0.15, SMOKA_MOD_B ~ -0.11 THROI corr,

d0_THROI2 <- d0[,26:32]
d0_THROI2$THROI <- d0$THROI
d0_THROI2_cor <- cor(d0_THROI2)
corrplot(d0_THROI2_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black") # SEX1 ~ 0.17, Influenced ~ 0.1, ABSI_B ~ -0.07 THROI corr,

# RECTM & phenotype correlation

d0_RECTM1 <- d0[, 2:9]
d0_RECTM1$RECTM <- d0$RECTM
d0_RECTM1_cor <- cor(d0_RECTM1)
corrplot(d0_RECTM1_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black") # AGE_B ~ 0.03, SMOKA_MOD_B ~ 0.02 RECTM corr,

d0_RECTM2 <- d0[,26:32]
d0_RECTM2$RECTM <- d0$RECTM
d0_RECTM2_cor <- cor(d0_RECTM2)
corrplot(d0_RECTM2_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black") # SEX1 ~ -0.03 , ABSI_B ~ 0.02, Influenced ~ 0.02 RECTM corr,

# BREAC & phenotype correlation

d0_BREAC1 <- d0[, 2:9]
d0_BREAC1$BREAC <- d0$BREAC
d0_BREAC1_cor <- cor(d0_BREAC1)
corrplot(d0_BREAC1_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black") 

d0_BREAC2 <- d0[,26:32]
d0_BREAC2$BREAC <- d0$BREAC
d0_BREAC2_cor <- cor(d0_BREAC2)
corrplot(d0_BREAC2_cor, method="shade", addshade="all", shade.col=NA, tl.col="red", 
         tl.srt=40, diag=FALSE, addCoef.col="black")

# 암 환자 비율 확인

sum(d1$LIVER == 0) # 16659
sum(d1$LIVER == 1) # 296

sum(d1$STOMA == 0) # 15707
sum(d1$STOMA == 1) # 1248

sum(d1$LUNG == 0) # 16456
sum(d1$LUNG == 1)  # 499

sum(d1$PROST == 0) # 16480
sum(d1$PROST == 1) # 475

sum(d1$RECTM == 0) # 16672
sum(d1$RECTM == 1) # 283

sum(d1$COLON == 0) # 16640
sum(d1$COLON == 1) # 315

sum(d1$THROI == 0) # 15059
sum(d1$THROI == 1) # 1896

# THROI phenotype variable selecetion

str(d1)

d1_throi <- d1[, c(-1, -10, -11, -12, -13, -14, -16, -17,
                   -18, -19, -20, -21, -22, -23, -24, -25)]
str(d1_throi)
set.seed(1234)
n <- nrow(d1_throi)
idx <- 1:n
training_idx <- sample(idx, n*.80)
test_idx <- setdiff(idx, training_idx)
training <- d1[training_idx, ]
test <- d1[test_idx ,]
nrow(training) # 13564 data
nrow(test) # 3391 data

xx <- model.matrix(THROI ~ .-1 , d1_throi)
x <- xx[training_idx, ]
y <- as.numeric(as.character(training$THROI))

# THROI phenotype variable selection with LASSO

d1_cvfit <- cv.glmnet(x, y, family="binomial", alpha=1)
plot(d1_cvfit)

coef(d1_cvfit, s=c("lambda.min"))
length(which(coef(d1_cvfit, s=c("lambda.min")) != 0)) -1 
which(coef(d1_cvfit, s=c("lambda.min")) != 0)
# AGE, SMOKA_MOD, ALCO_AMOUNT, EXER, FBS, CREAT, SEX1, BP, CVH, LFD, Influenced, FF (12개)

coef(d1_cvfit, s=c("lambda.1se"))
length(which(coef(d1_cvfit, s=c("lambda.1se")) != 0)) -1 
# AGE_B, SMOKA_MOD_B, SEX1, Influenced (4개)

opt.lam = c(d1_cvfit$lambda.min, d1_cvfit$lambda.1se)
coef(d1_cvfit, s = opt.lam)

# only SNP variable selection with LASSO

str(d1)

d0_throi <- d0[, c(15, 33:15220)]
str(d0_throi)
set.seed(1234)
n <- nrow(d0_throi)
idx <- 1:n
training_idx <- sample(idx, n*.80)
test_idx <- setdiff(idx, training_idx)
training <- d0[training_idx, ]
test <- d0[test_idx ,]
nrow(training) # 13564 data
nrow(test) # 3391 data

xx <- model.matrix(THROI ~ .-1 , d0_throi)
x <- xx[training_idx, ]
y <- as.numeric(as.character(training$THROI))

d0_cvfit <- cv.glmnet(x, y, family="binomial", alpha=1)
plot(d0_cvfit)

coef(d0_cvfit, s=c("lambda.min"))

length(which(coef(d0_cvfit, s=c("lambda.min")) != 0)) -1 # 선택된 변수 = 26개
which(coef(d0_cvfit, s=c("lambda.min")) != 0)
# 177   887   893  1560  1644  2256  3025  3301  5531  5606  6387  6731  8775 
# 9208  9262 9391  9596 10188 10407 11124 11195 11390 12039 13137 13358 13389

coef(d0_cvfit, s=c("lambda.1se"))
length(which(coef(d0_cvfit, s=c("lambda.1se")) != 0)) -1 # 선택된 변수 = 3개
which(coef(d0_cvfit, s=c("lambda.1se")) != 0)
# 177 8775 12039

# (phenotype+SNP) variable selection with LASSO
str(d0)
d0_throi0 <- d0[, c(-1, -5,-6,-7,-8, -9, -10, -11, -12, -13, -14, -16, -17,-18, -19, -20, -21, -22,-23,-24,-25, -28, -29, -30, -32)]
d0_throi0 <- subset(d0_throi0, select = -c(IID))
str(d0_throi0)
set.seed(1234)
n <- nrow(d0_throi0)
idx <- 1:n
training_idx <- sample(idx, n*.80)
test_idx <- setdiff(idx, training_idx)
training <- d0[training_idx, ]
test <- d0[test_idx ,]
nrow(training) # 13564 data
nrow(test) # 3391 data

xx <- model.matrix(THROI ~ .-1 , d0_throi0)
x <- xx[training_idx, ]
y <- as.numeric(as.character(training$THROI))

d0_cvfit0 <- cv.glmnet(x, y, family="binomial", alpha=1)
plot(d0_cvfit0)

coef(d0_cvfit0, s=c("lambda.min"))
length(which(coef(d0_cvfit0, s=c("lambda.min")) != 0)) -1
which(coef(d0_cvfit0, s=c("lambda.min")) != 0)
# 1 2 3 5 7 183 205 610 631 893 896 899 1375 1566 1580 1650 2262 2674  3031 3131 3307 3372 4056 4622
# 4825  5537  5612  6393  6598  6737  6947  7850  7896  8254  8429  8781  8788  8937  9214  9268  9397  9602  9655  9709  9721 10151 10171 10194
# 10413 10533 10859 10991 11130 11201 11209 11389 11396 11476 12045 12061 12351 12631 12783 12835 12951 12993 13112 13143 13364 13395 13435 13581
# 14213 14376 14754 15055 (75개)

coef(d0_cvfit0, s=c("lambda.1se"))
length(which(coef(d0_cvfit0, s=c("lambda.1se")) != 0)) -1
which(coef(d0_cvfit0, s=c("lambda.1se")) != 0)
# AGE_B, SMOKA_MOD_B, SEX1, Influenced (4개)

###############################################################################
############################# Logistic Regression #############################
###############################################################################

# THROI logistic Regression - (phenotype) AUC 0.713 # AGE, SMOKA_MOD, ALCO_AMOUNT, SEX1, ABSI_B, Influenced ( LASSO + cor plot )
set.seed(1234)
d1_throi <- d1_throi[, c(1, 2, 3, 9, 10, 11, 15)]
str(d1_throi)

d1_throi$THROI <- as.numeric(d1_throi$THROI)
head(d1_throi)
idx <- sample(1:nrow(d1_throi), nrow(d1_throi)*0.8)
train <- d1_throi[idx, ]
test <- d1_throi[-idx, ]
train$THROI <- as.factor(train$THROI)
test$THROI <- as.factor(test$THROI)

d1_logistic_model <- glm(THROI ~ ., train, family = "binomial")
summary(d1_logistic_model)

d1_logistic_pred <- predict(d1_logistic_model, newdata= test, type="response")
d1_logistic_pred

d1_logistic_pr <- prediction(d1_logistic_pred, test$THROI)
d1_logistic_prf_ROC <- performance(d1_logistic_pr, measure = "tpr", x.measure = "fpr")
plot(d1_logistic_prf_ROC)

perf.auc <- performance(d1_logistic_pr, measure = "auc")
unlist(perf.auc@y.values)

ROC(form=THROI~d1_logistic_pred, data=test, plot="ROC")

# Confusion Matrix

d1_logistic_pred2 <- ifelse(d1_logistic_pred >= 0.5, '1', '0')
d1_logistic_pred2 <- as.factor(d1_logistic_pred2)
confusionMatrix(d1_logistic_pred2, test$THROI, positive = "1")
str(test$THROI)
str(d1_logistic_pred2)
levels(test$THROI) <- list("0" = "1", "1" = "2")

# THROI logistic Regression - (SNP) AUC 0.506 
set.seed(1234)
d0_throi1 <- d0[, c(15, 177, 887, 893, 1560, 1644, 2256, 3025, 3301, 5531, 5606, 6387,
                    6731, 8775, 9208, 9262, 9391, 9596, 10188, 10407, 11124, 11195, 11390,
                    12039, 13137, 13358, 13389)]

str(d0_throi1)

d0_throi1$THROI <- as.numeric(d0_throi1$THROI)
head(d0_throi1)
idx1 <- sample(1:nrow(d0_throi1), nrow(d0_throi1)*0.8)
train1 <- d0_throi1[idx1, ]
test1 <- d0_throi1[-idx1, ]
train1$THROI
d0_logistic_model1 <- glm(THROI ~ ., train1, family = "binomial")
summary(d0_logistic_model1)

d0_logistic_pred1 <- predict(d0_logistic_model1, newdata= test1, type="response")
d0_logistic_pred1

d0_logistic_pr1 <- prediction(d0_logistic_pred1, test1$THROI)
d0_logistic_prf_ROC1 <- performance(d0_logistic_pr1, measure = "tpr", x.measure = "fpr")
plot(d0_logistic_prf_ROC1)

perf.auc1 <- performance(d0_logistic_pr1, measure = "auc")
unlist(perf.auc1@y.values)

ROC(form=THROI~d0_logistic_pred1, data=test1, plot="ROC")

# THROI logistic Regression - (phenotype + SNP) AUC 0.708
set.seed(1234)
d0_throi2 <- d0[, c(2, 3, 4, 15, 26, 27, 31, 177, 887, 893, 1560, 1644, 2256, 3025, 3301, 5531, 5606, 6387,
                    6731, 8775, 9208, 9262, 9391, 9596, 10188, 10407, 11124, 11195, 11390,
                    12039, 13137, 13358, 13389)]
str(d0_throi2)

d0_throi2$THROI <- as.numeric(d0_throi2$THROI)
head(d0_throi2)
idx2 <- sample(1:nrow(d0_throi2), nrow(d0_throi2)*0.8)
train2 <- d0_throi2[idx2, ]
test2 <- d0_throi2[-idx2, ]
train2$THROI
d0_logistic_model2 <- glm(THROI ~ ., train2, family = "binomial")
summary(d0_logistic_model2)

d0_logistic_pred2 <- predict(d0_logistic_model2, newdata= test2, type="response")
d0_logistic_pred2

d0_logistic_pr2 <- prediction(d0_logistic_pred2, test2$THROI)
d0_logistic_prf_ROC2 <- performance(d0_logistic_pr2, measure = "tpr", x.measure = "fpr")
plot(d0_logistic_prf_ROC2)

perf.auc2 <- performance(d0_logistic_pr2, measure = "auc")
unlist(perf.auc2@y.values)

d0_logistic_ROC2 <- ROC(form=THROI~d0_logistic_pred2, data=test2, plot="ROC")

###############################################################################
################################ Random Forest ################################
###############################################################################

# THROI RandomForest - (phenotype) # AGE, SMOKA_MOD, ALCO_AMOUNT, SEX1, ABSI_B, Influenced
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
rf_fit <- train(THROI ~ ., data = train, method = "rf", trControl = fitControl, verbose = F)
rf_fit 
plot(rf_fit)

# mtry  Accuracy   Kappa     
# 2     0.8888676  0.05254699
# 4     0.8787673  0.08300353
# 6     0.8750515  0.09052618

d1_throi$THROI <- as.numeric(d1_throi$THROI)
head(d1_throi)
idx <- sample(1:nrow(d1_throi), nrow(d1_throi)*0.8)
train <- d1_throi[idx, ]
test <- d1_throi[-idx, ]
train$THROI <- as.factor(train$THROI)
test$THROI <- as.factor(test$THROI)

set.seed(1234)
d1_randomForest_model1 <- randomForest(THROI ~ ., ntree=500, mtry=2, data=train)
d1_randomForest_model1

randomForest_pred1 <- predict(d1_randomForest_model1, newdata = test, type = 'class')
sum(randomForest_pred1 == test$THROI) / nrow(test)
str(randomForest_pred1)

randomForest_ROC1 <- ROC(form=THROI~randomForest_pred1, data=test, plot="ROC") # mtry = 2 : AUC 0.521

d1_randomForest_model2 <- randomForest(THROI ~ ., ntree=500, mtry=4, data=train)
d1_randomForest_model2

randomForest_pred2 <- predict(d1_randomForest_model2, newdata = test, type = 'class')
randomForest_ROC2 <- ROC(form=THROI~randomForest_pred2, data=test, plot="ROC") # mtry = 3 : AUC 0.535

d1_randomForest_model3 <- randomForest(THROI ~ ., ntree=500, mtry=6, data=train)
d1_randomForest_model3

randomForest_pred3 <- predict(d1_randomForest_model3, newdata = test, type = 'class')
randomForest_ROC3 <- ROC(form=THROI~randomForest_pred3, data=test, plot="ROC") # mtry = 5 : AUC 0.529

# SNP THROI RandomForest why..?   contrasts can be applied only to factors with 2 or more levels

set.seed(1234)

d0_throi1 <- d0[, c(15, 177, 887, 893, 1560, 1644, 2256, 3025, 3301, 5531, 5606, 6387,
                    6731, 8775, 9208, 9262, 9391, 9596, 10188, 10407, 11124, 11195, 11390,
                    12039, 13137, 13358, 13389)]
str(d0_throi1)

d0_throi1$THROI <- as.numeric(d0_throi1$THROI)
head(d0_throi1)
idx <- sample(1:nrow(d0_throi1), nrow(d0_throi1)*0.8)
train <- d0_throi1[idx, ]
test <- d0_throi1[-idx, ]
str(train$THROI)

train$THROI <- as.factor(train$THROI)
test$THROI <- as.factor(test$THROI)
str(test$THROI)
d0_randomForest_model1 <- randomForest(THROI ~ ., ntree=500, mtry=6, data=train)
d0_randomForest_model1

d0_randomForest_pred1 <- predict(d0_randomForest_model1, newdata = test, type = 'class')
sum(d0_randomForest_pred1 == test$THROI) / nrow(test)

d0_randomForest_ROC1 <- ROC(form=THROI~d0_randomForest_pred1, data=test, plot="ROC")

# phenotype + SNP(only snp LASSO) THROI RandomForest # AUC 0.501
set.seed(1234)
d0_throi <- d0[, c(2, 3, 4, 15, 26, 27, 31, 177, 887, 893, 1560, 1644, 2256, 3025, 3301, 5531, 5606, 6387,
                   6731, 8775, 9208, 9262, 9391, 9596, 10188, 10407, 11124, 11195, 11390,
                   12039, 13137, 13358, 13389)]
str(d0_throi)

d0_throi$THROI <- as.factor(d0_throi$THROI)
head(d0_throi)
idx <- sample(1:nrow(d0_throi), nrow(d0_throi)*0.8)
train <- d0_throi[idx, ]
test <- d0_throi[-idx, ]
str(train)
train$THROI <- as.factor(train$THROI)
d0_randomForest_model <- randomForest(THROI ~ ., mtry=6, data=train)
d0_randomForest_model

d0_randomForest_pred <- predict(d0_randomForest_model, newdata = test, type = 'class')
sum(d0_randomForest_pred == test$THROI) / nrow(test)
d0_randomForest_ROC <- ROC(form=THROI~d0_randomForest_pred, data=test, plot="ROC")

# phenotype + SNP(phenotype + snp LASSO) THROI RandomForest

set.seed(1234)
d0_throi3 <- d0_throi0[, c(1, 2, 3, 4,5, 7, 183, 205, 610, 631, 893, 896, 899, 1375, 1566, 1580, 1650, 2262, 2674, 3031, 3131, 3307, 3372, 4056, 4622,
                           4825, 5537, 5612, 6393, 6598, 6737, 6947, 7850, 7896, 8254, 8429, 8781, 8788, 8937, 9214, 9268, 9397, 9602, 9655, 9709, 9721,
                           10151, 10171, 10194, 10413, 10533, 10859, 10991, 11130, 11201, 11209, 11389, 11396, 11476, 12045, 12061, 12351, 12631, 12783,
                           12835, 12951, 12993, 13112, 13143, 13364, 13395, 13435, 13581, 14213, 14376, 14754, 15055)]

d0_throi3$THROI <- as.factor(d0_throi3$THROI)
head(d0_throi3)
idx <- sample(1:nrow(d0_throi3), nrow(d0_throi3)*0.8)
train <- d0_throi3[idx, ]
test <- d0_throi3[-idx, ]
str(train)
train$THROI <- as.factor(train$THROI)
d0_randomForest_model <- randomForest(THROI ~ ., mtry=6, data=train)
d0_randomForest_model

d0_randomForest_pred <- predict(d0_randomForest_model, newdata = test, type = 'class')
sum(d0_randomForest_pred == test$THROI) / nrow(test)
d0_randomForest_ROC <- ROC(form=THROI~d0_randomForest_pred, data=test, plot="ROC")

###############################################################################
################################## XG boost ###################################
###############################################################################

# THROI XG boost - (phenotype) AUC 0.703

str(d1_throi)

d1_throi$THROI <- as.numeric(d1_throi$THROI)
head(d1_throi)
idx <- sample(1:nrow(d1_throi), nrow(d1_throi)*0.8)
train <- d1_throi[idx, ]
test <- d1_throi[-idx, ]
test$THROI <- as.factor(test$THROI)

train.label <- as.integer(train$THROI)
str(train.label)
mat_train.data <- as.matrix(train[,-4])
mat_test.data <- as.matrix(test[,- 4])

xgb.train <- xgb.DMatrix(data=mat_train.data, label= train.label)
xgb.test <- xgb.DMatrix(data=mat_test.data)

pa_list <- list(booster="gbtree", eta=0.001, max_depth=10,
                gamma=5, subsample=0.8, colsmple_bytree=0.8,
                objective="binary:logistic",
                eval_metric="auc")

md.xgb <- xgb.train(params=pa_list, data=xgb.train,
                    nrounds=200, early_stopping_rounds=10,
                    watchlist=list(val1=xgb.train),
                    verbose=1)

xgb.pred <- predict(md.xgb, newdata=xgb.test)
xgb.pred2 <- ifelse(xgb.pred >= 0.5, '1', '0')
xgb.pred2 <- as.factor(xgb.pred2)
confusionMatrix(xgb.pred2, test$THROI, positive = "1")
d0_xgb_pr1 <- prediction(xgb.pred, test$THROI)
d0_xgb_ROC1 <- performance(d0_xgb_pr1, measure = "tpr", x.measure = "fpr")
plot(d0_xgb_ROC1)

xgb.perf.auc <- performance(d0_xgb_pr1, measure = "auc")
unlist(xgb.perf.auc@y.values)
round(0.7033265,3)

ROC(form=THROI~xgb.pred2, data=test, plot="ROC") 

# THROI XG boost - (SNP) AUC 0.497
set.seed(1234)

d0_throi1 <- d0[, c(15, 177, 887, 893, 1560, 1644, 2256, 3025, 3301, 5531, 5606, 6387,
                    6731, 8775, 9208, 9262, 9391, 9596, 10188, 10407, 11124, 11195, 11390,
                    12039, 13137, 13358, 13389)]
str(d0_throi1)

d0_throi1$THROI <- as.numeric(d0_throi1$THROI)
head(d0_throi1)
idx <- sample(1:nrow(d0_throi1), nrow(d0_throi1)*0.8)
train <- d0_throi1[idx, ]
test <- d0_throi1[-idx, ]
str(train$THROI)
test$THROI <-as.factor(test$THROI)
train.label <- as.integer(train$THROI)
str(train.label)

mat_train.data <- as.matrix(train[,-1])
mat_test.data <- as.matrix(test[,- 1])

xgb.train <- xgb.DMatrix(data=mat_train.data, label= train.label)
xgb.test <- xgb.DMatrix(data=mat_test.data)

pa_list <- list(booster="gbtree", eta=0.001, max_depth=10,
                gamma=5, subsample=0.8, colsmple_bytree=0.8,
                objective="binary:logistic",
                eval_metric="auc")

md.xgb <- xgb.train(params=pa_list, data=xgb.train,
                    nrounds=200, early_stopping_rounds=10,
                    watchlist=list(val1=xgb.train),
                    verbose=1)

xgb.pred <- predict(md.xgb, newdata=xgb.test)
xgb.pred2 <- ifelse(xgb.pred >= 0.5, '1', '0')
xgb.pred2 <- as.factor(xgb.pred2)
confusionMatrix(xgb.pred2, test$THROI, positive = "1")
d0_xgb_pr1 <- prediction(xgb.pred, test$THROI)
d0_xgb_ROC1 <- performance(d0_xgb_pr1, measure = "tpr", x.measure = "fpr")
plot(d0_xgb_ROC1)

xgb.perf.auc <- performance(d0_xgb_pr1, measure = "auc")
unlist(xgb.perf.auc@y.values)

round(0.4969222,3)
str(test$THROI)
str(xgb.pred2)

# THROI XG boost - (phenotype + SNP) AUC 0.715
set.seed(1234)

d0_throi1 <- d0[, c(2, 3, 4, 15, 26, 27, 31, 177, 887, 893, 1560, 1644, 2256, 3025, 3301, 5531, 5606, 6387,
                    6731, 8775, 9208, 9262, 9391, 9596, 10188, 10407, 11124, 11195, 11390,
                    12039, 13137, 13358, 13389)]
str(d0_throi1)

d0_throi1$THROI <- as.numeric(d0_throi1$THROI)
head(d0_throi1)
idx <- sample(1:nrow(d0_throi1), nrow(d0_throi1)*0.8)
train <- d0_throi1[idx, ]
test <- d0_throi1[-idx, ]
str(train$THROI)
str(train)
train.label <- as.integer(train$THROI)
str(train.label)
str(mat_train.data)
mat_train.data <- as.matrix(train[,-4])
mat_test.data <- as.matrix(test[,-4])

xgb.train <- xgb.DMatrix(data=mat_train.data, label= train.label)
xgb.test <- xgb.DMatrix(data=mat_test.data)

pa_list <- list(booster="gbtree", eta=0.001, max_depth=10,
                gamma=5, subsample=0.8, colsmple_bytree=0.8,
                objective="binary:logistic",
                eval_metric="auc")

md.xgb <- xgb.train(params=pa_list, data=xgb.train,
                    nrounds=200, early_stopping_rounds=10,
                    watchlist=list(val1=xgb.train),
                    verbose=1)

xgb.pred <- predict(md.xgb, newdata=xgb.test)

xgb.pred2 <- ifelse(xgb.pred >= 0.5, '1', '0')
xgb.pred2 <- as.factor(xgb.pred2)
xgb.pred2
test$THROI <- as.factor(test$THROI)
confusionMatrix(xgb.pred2, test$THROI, positive = "1")
str(test$THROI)
str(xgb.pred2)
test$THROI <-as.factor(test$THROI)

d0_xgb_pr1 <- prediction(xgb.pred, test$THROI)
d0_xgb_ROC1 <- performance(d0_xgb_pr1, measure = "tpr", x.measure = "fpr")
plot(d0_xgb_ROC1)

xgb.perf.auc <- performance(d0_xgb_pr1, measure = "auc")
unlist(xgb.perf.auc@y.values)

# xgboost THROI (phenotype + SNP with LASSO) AUC 0.725

set.seed(1234)

d0_throi3 <- d0_throi0[, c(1, 2, 3, 4,5, 7, 183, 205, 610, 631, 893, 896, 899, 1375, 1566, 1580, 1650, 2262, 2674, 3031, 3131, 3307, 3372, 4056, 4622,
                           4825, 5537, 5612, 6393, 6598, 6737, 6947, 7850, 7896, 8254, 8429, 8781, 8788, 8937, 9214, 9268, 9397, 9602, 9655, 9709, 9721,
                           10151, 10171, 10194, 10413, 10533, 10859, 10991, 11130, 11201, 11209, 11389, 11396, 11476, 12045, 12061, 12351, 12631, 12783,
                           12835, 12951, 12993, 13112, 13143, 13364, 13395, 13435, 13581, 14213, 14376, 14754, 15055)]
str(d0_throi3)

d0_throi3$THROI <- as.numeric(d0_throi3$THROI)
head(d0_throi3)
idx <- sample(1:nrow(d0_throi3), nrow(d0_throi3)*0.8)
train <- d0_throi3[idx, ]
test <- d0_throi3[-idx, ]
str(train$THROI)
test$THROI <-as.factor(test$THROI)
train.label <- as.integer(train$THROI)
str(train.label)

mat_train.data <- as.matrix(train[,-4])
mat_test.data <- as.matrix(test[,- 4])
str(train.label)
xgb.train <- xgb.DMatrix(data=mat_train.data, label= train.label)
xgb.test <- xgb.DMatrix(data=mat_test.data)

pa_list <- list(booster="gbtree", eta=0.001, max_depth=10,
                gamma=5, subsample=0.8, colsmple_bytree=0.8,
                objective="binary:logistic",
                eval_metric="auc")

md.xgb <- xgb.train(params=pa_list, data=xgb.train,
                    nrounds=200, early_stopping_rounds=10,
                    watchlist=list(val1=xgb.train),
                    verbose=1)

xgb.pred <- predict(md.xgb, newdata=xgb.test)
xgb.pred2 <- ifelse(xgb.pred >= 0.5, '1', '0')
xgb.pred2 <- as.factor(xgb.pred2)
confusionMatrix(xgb.pred2, test$THROI, positive = "1")
d0_xgb_pr1 <- prediction(xgb.pred, test$THROI)
d0_xgb_ROC1 <- performance(d0_xgb_pr1, measure = "tpr", x.measure = "fpr")
plot(d0_xgb_ROC1)

xgb.perf.auc <- performance(d0_xgb_pr1, measure = "auc")
unlist(xgb.perf.auc@y.values)

round(0.7253309,3)

# Second Data Preprocessing

d1 <- read.table("phenotype.txt", sep="\t", header=T)
d2 <- read.csv("KCPS_r2_0.01.csv")

colSums(is.na(d1))
na.omit(d1$SMOKA_MOD_B)

str(d2)
colSums(is.na(d1))

# delete variable because of Missing
d1 <- subset(d1, select=-c(MDM_B))
d1 <- subset(d1, select=-c(MHTN_B))
d1 <- subset(d1, select=-c(MLPD_B))
d1 <- subset(d1, select=-c(PHTN_B))
d1 <- subset(d1, select=-c(PDM_B))
d1 <- subset(d1, select=-c(PLPD_B))
d1 <- subset(d1, select=-c(PCAN80))
d1 <- subset(d1, select=-c(PCAN81))
d1 <- subset(d1, select=-c(PCAN82))
d1 <- subset(d1, select=-c(PCAN83))
d1 <- subset(d1, select=-c(PCAN84))
d1 <- subset(d1, select=-c(PCAN86))
d1 <- subset(d1, select=-c(PCAN89))
d1 <- subset(d1, select=-c(FCAN80))
d1 <- subset(d1, select=-c(FCAN81))
d1 <- subset(d1, select=-c(FCAN82))
d1 <- subset(d1, select=-c(FCAN83))
d1 <- subset(d1, select=-c(FCAN84))
d1 <- subset(d1, select=-c(FCAN86))
d1 <- subset(d1, select=-c(FCAN89))
d1 <- subset(d1, select=-c(FEV1))
d1 <- subset(d1, select=-c(FVC))
d1 <- subset(d1, select=-c(SCOLON))
d1 <- subset(d1, select=-c(SRECTM))
d1 <- subset(d1, select=-c(SPROST))
d1 <- subset(d1, select=-c(STHROI))
d1 <- subset(d1, select=-c(SBREAC))
d1 <- subset(d1, select=-c(SLUNG))
d1 <- subset(d1, select=-c(SSTOMA))
d1 <- subset(d1, select=-c(SLIVER))
d1$ABSI_B <- (d1$WAIST_B * 0.3937) / (d1$WT_B/(d1$HT_B/100)^2) * 2/3 * (d1$HT_B/100) * 1/2
d1 <- subset(d1, select=-c(HT_B))
d1 <- subset(d1, select=-c(WT_B))
d1 <- subset(d1, select=-c(WAIST_B))
d1 <- subset(d1, select=-c(IID))
names(d1)[1] <- c("ID")
names(d2)[1] <- c("ID")
d2 <- subset(d2, select=-c(IID))

naniar::gg_miss_var(d1)
gg_miss_upset(d1, nsets = 10)

# No missing data frame 

perpect <- d1[complete.cases(d1),] # 8787 obs. of  34 variables:
str(perpect)

str(d1)

perpect_d1 <- perpect[, c(-17, -16, -14, -13, -12, -11, -10, -1)]
str(perpect_d1) # 8787 obs. of  26 variables:
perpect_df <- perpect_d1[, c(1,2,3,5,20,23,26)]
str(perpect_df) # 8787 obs. of  7 variables:

d0 <- merge(perpect_df, d2, by=c('ID'))
str(d0) #8787 obs. of  15196 variables:
d0$THROI <- as.factor(d0$THROI)

set.seed(1234)
n <- nrow(perpect_df)
idx <- 1:n
training_idx <- sample(idx, n*.70)
test_idx <- setdiff(idx, training_idx)
training <- perpect_df[training_idx, ]
test <- perpect_df[test_idx ,]
nrow(training) # 6150 data
nrow(test) # 2637 data
str(perpect_df)
xx <- model.matrix(THROI ~ .-1 , d0)
str(xx)
x <- xx[training_idx, ]
y <- as.numeric(as.character(training$THROI))

# THROI phenotype variable selection with LASSO

perpect_df_cvfit <- cv.glmnet(x, y, family="binomial", alpha=1)
plot(perpect_df_cvfit)

coef(perpect_df_cvfit, s=c("lambda.min"))
length(which(coef(perpect_df_cvfit, s=c("lambda.min")) != 0)) -1 
which(coef(perpect_df_cvfit, s=c("lambda.min")) != 0)
# AGE_B, SMOK_B, SMOKA_MOD_B, ALCO_B, ALCO_AMOUNT_B

coef(perpect_df_cvfit, s=c("lambda.1se"))
length(which(coef(perpect_df_cvfit, s=c("lambda.1se")) != 0)) -1
which(coef(perpect_df_cvfit, s=c("lambda.1se")) != 0)
# AGE, SMOK, SEX

opt.lam = c(perpect_df_cvfit$lambda.min, perpect_df_cvfit$lambda.1se)
coef(perpect_df_cvfit, s = opt.lam)

# LASSO SNP
str(d0)
SNP_throi <- d0[, c(7:15194)]
str(SNP_throi) #8787 obs. of  15189 variables:
set.seed(1234)
n <- nrow(SNP_throi)
idx <- 1:n
training_idx <- sample(idx, n*.70)
test_idx <- setdiff(idx, training_idx)
training <- SNP_throi[training_idx, ]
test <- SNP_throi[test_idx ,]
nrow(training) # 6150 data
nrow(test) # 2637 data

xx <- model.matrix(THROI ~ .-1 , SNP_throi)
x <- xx[training_idx, ]
str(x)
y
y <- as.numeric(as.character(training$THROI))
SNP_cvfit <- cv.glmnet(x, y, family="binomial", alpha=1)
plot(SNP_cvfit)

coef(SNP_cvfit, s=c("lambda.min"))

length(which(coef(SNP_cvfit, s=c("lambda.min")) != 0)) -1 # 선택된 변수 = 96개
which(coef(SNP_cvfit, s=c("lambda.min")) != 0)

coef(SNP_cvfit, s=c("lambda.1se"))
length(which(coef(SNP_cvfit, s=c("lambda.1se")) != 0)) -1 # 선택된 변수 = 0개
which(coef(SNP_cvfit, s=c("lambda.1se")) != 0)

str(d0)
str(SNP_throi) #8787 obs. of  15189 variables:
set.seed(1234)
n <- nrow(d0)
idx <- 1:n
training_idx <- sample(idx, n*.70)
test_idx <- setdiff(idx, training_idx)
training <- d0[training_idx, ]
test <- d0[test_idx ,]
nrow(training) # 6150 data
nrow(test) # 2637 data

xx <- model.matrix(THROI ~ .-1 , d0)
x <- xx[training_idx, ]
y <- as.numeric(as.character(training$THROI))

SNP_cvfit <- cv.glmnet(x, y, family="binomial", alpha=1)
plot(SNP_cvfit)

df <- d0[, c(1,2,3,4,5,182,773,892,953,1569,1746,2347,2361,2825,2860,
             3227,3339,4055,5416,5487,5611,7219,7797,8103,8780,9078,
             9156,9176,9214,9396,9685,9689,10636,10870,11200,11666,12044,12451,13625,14245)]

# Elastic Net SNP alpha = 0.9
SNP_cvfit1 <- cv.glmnet(x, y, family="binomial", alpha=0.9)
plot(SNP_cvfit1)

coef(SNP_cvfit1, s=c("lambda.min"))

length(which(coef(SNP_cvfit1, s=c("lambda.min")) != 0)) -1 # 선택된 변수 = 51개
which(coef(SNP_cvfit1, s=c("lambda.min")) != 0)

coef(SNP_cvfit1, s=c("lambda.1se"))
length(which(coef(SNP_cvfit1, s=c("lambda.1se")) != 0)) -1 # 선택된 변수 = 0개
which(coef(SNP_cvfit1, s=c("lambda.1se")) != 0)

# Elastic Net SNP alpha = 0.8
SNP_cvfit2 <- cv.glmnet(x, y, family="binomial", alpha=0.8)
plot(SNP_cvfit2)

coef(SNP_cvfit2, s=c("lambda.min"))

length(which(coef(SNP_cvfit2, s=c("lambda.min")) != 0)) -1 # 선택된 변수 = 76개
which(coef(SNP_cvfit2, s=c("lambda.min")) != 0)

coef(SNP_cvfit2, s=c("lambda.1se"))
length(which(coef(SNP_cvfit2, s=c("lambda.1se")) != 0)) -1 # 선택된 변수 = 0개
which(coef(SNP_cvfit2, s=c("lambda.1se")) != 0)

# Elastic Net SNP alpha = 0.7
SNP_cvfit3 <- cv.glmnet(x, y, family="binomial", alpha=0.7)
plot(SNP_cvfit3)

coef(SNP_cvfit3, s=c("lambda.min"))

length(which(coef(SNP_cvfit3, s=c("lambda.min")) != 0)) -1 # 선택된 변수 = 52
which(coef(SNP_cvfit3, s=c("lambda.min")) != 0)

coef(SNP_cvfit3, s=c("lambda.1se"))
length(which(coef(SNP_cvfit3, s=c("lambda.1se")) != 0)) -1 # 선택된 변수 = 0개
which(coef(SNP_cvfit3, s=c("lambda.1se")) != 0)

# Elastic Net SNP alpha = 0.6
SNP_cvfit4 <- cv.glmnet(x, y, family="binomial", alpha=0.6)
plot(SNP_cvfit4)

coef(SNP_cvfit4, s=c("lambda.min"))

length(which(coef(SNP_cvfit4, s=c("lambda.min")) != 0)) -1 # 선택된 변수 = 79
which(coef(SNP_cvfit4, s=c("lambda.min")) != 0)

coef(SNP_cvfit4, s=c("lambda.1se"))
length(which(coef(SNP_cvfit4, s=c("lambda.1se")) != 0)) -1 # 선택된 변수 = 0개
which(coef(SNP_cvfit4, s=c("lambda.1se")) != 0)

# Elastic Net SNP alpha = 0.1
SNP_cvfit5 <- cv.glmnet(x, y, family="binomial", alpha=0.1)
plot(SNP_cvfit5)

coef(SNP_cvfit5, s=c("lambda.min"))

length(which(coef(SNP_cvfit5, s=c("lambda.min")) != 0)) -1 # 선택된 변수 = 117개
which(coef(SNP_cvfit5, s=c("lambda.min")) != 0)

coef(SNP_cvfit5, s=c("lambda.1se"))
length(which(coef(SNP_cvfit5, s=c("lambda.1se")) != 0)) -1 # 선택된 변수 = 0개
which(coef(SNP_cvfit5, s=c("lambda.1se")) != 0)

# Ridge SNP alpha = 0
SNP_cvfit6 <- cv.glmnet(x, y, family="binomial", alpha=0)
plot(SNP_cvfit6)

coef(SNP_cvfit6, s=c("lambda.min"))

length(which(coef(SNP_cvfit6, s=c("lambda.min")) != 0)) -1 # 선택된 변수 = 15188개
which(coef(SNP_cvfit6, s=c("lambda.min")) != 0)

coef(SNP_cvfit6, s=c("lambda.1se"))
length(which(coef(SNP_cvfit6, s=c("lambda.1se")) != 0)) -1 # 선택된 변수 = 15188개
which(coef(SNP_cvfit6, s=c("lambda.1se")) != 0)

# THROI Logistic Regression

set.seed(1234)
str(perpect_df)
d1_throi <- perpect_df[, c(1, 2, 4, 11, 12, 13)]
str(d1_throi)

d1_throi$THROI <- as.numeric(d1_throi$THROI)
head(d1_throi)
idx <- sample(1:nrow(d1_throi), nrow(d1_throi)*0.8)
train <- d1_throi[idx, ]
test <- d1_throi[-idx, ]
train$THROI <- as.factor(train$THROI)
test$THROI <- as.factor(test$THROI)

d1_logistic_model <- glm(THROI ~ ., train, family = "binomial")
summary(d1_logistic_model)

d1_logistic_pred <- predict(d1_logistic_model, newdata= test, type="response")
d1_logistic_pred

d1_logistic_pr <- prediction(d1_logistic_pred, test$THROI)
d1_logistic_prf_ROC <- performance(d1_logistic_pr, measure = "tpr", x.measure = "fpr")
plot(d1_logistic_prf_ROC)

perf.auc <- performance(d1_logistic_pr, measure = "auc")
unlist(perf.auc@y.values)

ROC(form=THROI~d1_logistic_pred, data=test, plot="ROC")

# Confusion Matrix

d1_logistic_pred2 <- ifelse(d1_logistic_pred >= 0.5, '1', '0')
d1_logistic_pred2 <- as.factor(d1_logistic_pred2)
levels(test$THROI) <- list("0" = "1", "1" = "2")
confusionMatrix(d1_logistic_pred2, test$THROI, positive = "1")
# ConfusionMatrix 확인결과 Logistic Regression은 True Negative 수가 너무 적어 부적합한 모델이라고 판단.

# SNP LASSO RandomForest AUC : 

set.seed(1234)
ap1_snp_throi <- d0[, c(20, 103,177, 247, 303, 673, 846, 866, 948, 1331, 1379, 1434, 1564, 1632, 2167, 2224, 2227, 2318, 2375, 2398, 2449, 2596, 2659, 2677
                        , 2855, 3273, 3334, 3335, 3405, 3447, 3714, 3893, 3951, 4002, 4336, 4368, 4470, 4740, 4831, 4864, 4959, 5151, 5520, 6387, 6480, 6623, 6643, 6714, 6731, 
                        6780, 7121, 7214, 7389, 7468, 7550, 7938, 7976, 8169, 8565, 8677, 8775, 8786, 8809, 9262, 9727, 10103, 10218, 10550, 10583, 10675, 10767, 11124, 11176, 11202, 
                        11224, 11303, 11334, 11357, 11390, 12035, 12039, 12273, 12446, 12780, 12829, 12863, 12925, 13051, 13250, 13265, 13284, 13396, 13429, 13624, 14069, 14244,14330)]
str(ap1_snp_throi)
ap1_snp_throi$THROI <- as.factor(ap1_snp_throi$THROI)
head(ap1_snp_throi)
idx <- sample(1:nrow(ap1_snp_throi), nrow(ap1_snp_throi)*0.7)
train <- ap1_snp_throi[idx, ]
test <- ap1_snp_throi[-idx, ]
str(train)
train$THROI <- as.factor(train$THROI)
ap1_snp_throi_randomForest_model <- randomForest(THROI ~ ., mtry=83, data=train)
ap1_snp_throi_randomForest_model

ap1_snp_throi_randomForest_pred <- predict(ap1_snp_throi_randomForest_model, newdata = test, type = 'class')
sum(ap1_snp_throi_randomForest_pred == test$THROI) / nrow(test)
ap1_snp_throi_randomForest_ROC <- ROC(form=THROI~ap1_snp_throi_randomForest_pred, data=test, plot="ROC")

################################################################################
################################# RANDOMFOREST #################################
################################################################################

set.seed(1234)

str(d0)
perpect_df <- subset(perpect_df, select = -c(ID))
str(perpect_df)
ph_random_df <- perpect_df

ph_random_df$THROI <- as.factor(ph_random_df$THROI)
head(ph_random_df)
ph_random_idx <- sample(1:nrow(ph_random_df), nrow(ph_random_df)*0.8)
ph_random_train <- ph_random_df[ph_random_idx, ]
ph_random_test <- ph_random_df[-ph_random_idx, ]
str(ph_random_train)

ph_randomForest_model <- randomForest(THROI ~ ., mtry=2, data=ph_random_train)
ph_randomForest_model

ph_randomForest_pred <- predict(ph_randomForest_model, newdata = ph_random_test, type = 'class')
ph_randomForest_ROC <- ROC(form=THROI~ph_randomForest_pred, data=ph_random_test, plot="ROC")

d0 <- subset(d0, select = -c(ID))
str(d0)


## SNP randomForest
# 183 205 610 631 893 896 899 1375 1566 1580 1650 2262 2674  3031 3131 3307 3372 4056 4622
# 4825  5537  5612  6393  6598  6737  6947  7850  7896  8254  8429  8781  8788  8937  9214  9268  9397  9602  9655  9709  9721 10151 10171 10194
# 10413 10533 10859 10991 11130 11201 11209 11389 11396 11476 12045 12061 12351 12631 12783 12835 12951 12993 13112 13143 13364 13395 13435 13581
# 14213 14376 14754 15055 (75개)
str(d0)

snp_random_df <- d0_throi0[, c(3, 182, 204, 609, 630, 892, 895, 900, 1374, 1565, 1579, 1649, 2261, 2673, 3030, 3130, 3306, 3371, 4055, 4621,
                               4824, 5536, 5611, 6392, 6597, 6736, 6946, 7849, 7895, 8253, 8428, 8780, 8787, 8936, 9213, 9267, 9396, 9601, 9654, 9708, 9720,
                               10150, 10170, 10193, 10412, 10532, 10858, 10990, 11129, 11200, 11208, 11388, 11395, 11475, 12044, 12060, 12350, 12630, 12782,
                               12834, 12950, 12992, 13111, 13142, 13363, 13394, 13434, 13580, 14212, 14375, 14753, 15054)]
str(snp_random_df)
snp_random_df$THROI <- as.factor(snp_random_df$THROI)

snp_random_idx <- sample(1:nrow(snp_random_df), nrow(snp_random_df)*0.7)
snp_random_train <- snp_random_df[snp_random_idx, ]
snp_random_test <- snp_random_df[-snp_random_idx, ]
str(snp_random_train)
str(snp_random_test)
sum(snp_random_test$THROI == 1)
levels(snp_random_train$THROI) <- list("0" = "1", "1" = "2")
levels(snp_random_test$THROI) <- list("0" = "1", "1" = "2")

snp_randomForest_model <- randomForest(THROI ~ ., ntree=1000, mtry=8, data=snp_random_train)
snp_randomForest_model

snp_random_train$THROI
snp_random_test$THROI
snp_randomForest_pred <- predict(snp_randomForest_model, newdata = snp_random_test, type = 'class')
snp_randomForest_ROC <- ROC(form=THROI~snp_randomForest_pred, data=snp_random_test, plot="ROC")

## ph + SNP randomForest
str(d0)
d0_throi0 <- d0[, c(-1, -5,-6,-7,-8, -9, -10, -11, -12, -13, -14, -16, -17,-18, -19, -20, -21, -22,-23,-24,-25, -28, -29, -30, -32)]
str(d0_throi0)
mix_random_df <- d0_throi0[, c(1,2 ,3, 182, 204, 609, 630, 892, 895, 900, 1374, 1565, 1579, 1649, 2261, 2673, 3030, 3130, 3306, 3371, 4055, 4621,
                               4824, 5536, 5611, 6392, 6597, 6736, 6946, 7849, 7895, 8253, 8428, 8780, 8787, 8936, 9213, 9267, 9396, 9601, 9654, 9708, 9720,
                               10150, 10170, 10193, 10412, 10532, 10858, 10990, 11129, 11200, 11208, 11388, 11395, 11475, 12044, 12060, 12350, 12630, 12782,
                               12834, 12950, 12992, 13111, 13142, 13363, 13394, 13434, 13580, 14212, 14375, 14753, 15054)]

str(d0)
str(mix_random_df)
mix_random_df$AGE_B <- d0$AGE_B
mix_random_df$SEX1 <- d0$SEX1
mix_random_df$ABSI_B <- d0$ABSI_B
mix_random_df$THROI <- as.factor(mix_random_df$THROI)

mix_random_idx <- sample(1:nrow(mix_random_df), nrow(mix_random_df)*0.8)
mix_random_train <- mix_random_df[mix_random_idx, ]
mix_random_test <- mix_random_df[-mix_random_idx, ]
str(mix_random_train)

mix_randomForest_model <- randomForest(THROI ~ ., mtry=8, data=mix_random_train)
mix_randomForest_model

mix_randomForest_pred <- predict(mix_randomForest_model, newdata = mix_random_test, type = 'class')
mix_randomForest_ROC <- ROC(form=THROI~mix_randomForest_pred, data=mix_random_test, plot="ROC")

################################################################################
################################### XG boost ###################################
################################################################################

ph_random_df

str(ph_random_df)

ph_random_df$THROI <- as.numeric(ph_random_df$THROI)
head(ph_random_df)
idx <- sample(1:nrow(ph_random_df), nrow(ph_random_df)*0.8)
train <- ph_random_df[idx, ]
test <- ph_random_df[-idx, ]
test$THROI <- as.factor(test$THROI)
train$THROI <- as.numeric(train$THROI)
train.label <- as.integer(train$THROI) -1
train.label == 0
str(train.label)
str(test$THROI)
mat_train.data <- as.matrix(train[,-4])
mat_test.data <- as.matrix(test[,- 4])

levels(train.label) <- list("0" = "1", "1" = "2")
levels(test$THROI) <- list("0" = "1", "1" = "2")


xgb.train <- xgb.DMatrix(data=mat_train.data, label= train.label)
xgb.test <- xgb.DMatrix(data=mat_test.data)

pa_list <- list(booster="gbtree", eta=0.01, max_depth=10,
                gamma=5, subsample=0.8, colsmple_bytree=0.5,
                objective="binary:logistic",
                eval_metric="auc")

md.xgb <- xgb.train(params=pa_list, data=xgb.train,
                    nrounds=500, early_stopping_rounds=10,
                    watchlist=list(val1=xgb.train),
                    verbose=1)

xgb.pred <- predict(md.xgb, newdata=xgb.test)
xgb.pred2 <- ifelse(xgb.pred >= 0.5, '1', '0')
xgb.pred2 <- as.factor(xgb.pred2)
xgb.pred2
test$THROI
test$THROI <- as.factor(test$THROI)
confusionMatrix(xgb.pred2, test$THROI, positive = "1")
d0_xgb_pr1 <- prediction(xgb.pred, test$THROI)
d0_xgb_ROC1 <- performance(d0_xgb_pr1, measure = "tpr", x.measure = "fpr")
plot(d0_xgb_ROC1)


xgb.perf.auc <- performance(d0_xgb_pr1, measure = "auc")
unlist(xgb.perf.auc@y.values)
round(0.5288705,3)

## SNP XGBOOST

str(df)

df$THROI <- as.numeric(df$THROI)
head(df)
idx <- sample(1:nrow(df), nrow(df)*0.8)
train <- df[idx, ]
test <- df[-idx, ]
test$THROI <- as.integer(test$THROI) 
test$THROI
train.label 
test$THROI <- as.factor(test$THROI)
train$THROI <- as.numeric(train$THROI)
train.label <- as.integer(train$THROI)
str(train.label)
mat_train.data <- as.matrix(train[,-4])
mat_test.data <- as.matrix(test[,- 4])

xgb.train <- xgb.DMatrix(data=mat_train.data, label= train.label)
xgb.test <- xgb.DMatrix(data=mat_test.data)

pa_list <- list(booster="gbtree", eta=0.01, max_depth=10,
                gamma=5, subsample=0.8, colsmple_bytree=0.5,
                objective="binary:logistic",
                eval_metric="auc")

md.xgb <- xgb.train(params=pa_list, data=xgb.train,
                    nrounds=200, early_stopping_rounds=10,
                    watchlist=list(val1=xgb.train),
                    verbose=1)

xgb.pred <- predict(md.xgb, newdata=xgb.test)
xgb.pred2 <- ifelse(xgb.pred >= 0.5, '1', '0')
xgb.pred2 <- as.factor(xgb.pred2)
xgb.pred2
test$THROI
test$THROI <- as.factor(test$THROI)
confusionMatrix(xgb.pred2, test$THROI, positive = "1")
d0_xgb_pr1 <- prediction(xgb.pred, test$THROI)
d0_xgb_ROC1 <- performance(d0_xgb_pr1, measure = "tpr", x.measure = "fpr")
plot(d0_xgb_ROC1)


xgb.perf.auc <- performance(d0_xgb_pr1, measure = "auc")
unlist(xgb.perf.auc@y.values)
round(0.7448648,3)