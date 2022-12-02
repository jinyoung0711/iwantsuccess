
# load library

library(visNetwork)
library(bnlearn)
library(dplyr)

# check version
sessionInfo()

# load data
setwd("C:/Users/82106/iwantsuccess")
df <- read.csv("train.csv")

#### Heart Disease Bayesian Network GS Algorithm ####

# 이상치 처리

## [Drop RestingBP Outlier] ##
sum(df$RestingBP==0); 
df <- df[!(df$RestingBP == 0 ), ] # 818개 중 1개 제거 -> 817개 

## [Drop Cholesterol Outlier] (콜레스테롤 수치 mg/dl) ##
sum(df$Cholesterol==0)
df <- df[!(df$Cholesterol == 0 ), ] # 817개 중 171개 데이터 제거 완료 -> 646개 데이터 존재


## Oldpeak : 0과 0이 아닌값으로 범주화
df$Oldpeak <- ifelse(df$Oldpeak == 0, "0", "1")
df$Oldpeak <- as.factor(df$Oldpeak)

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

df2 <- df

# Age -> bin : 분위수를 기준값으로 4분위로 범주화
df2$Age <- cut(x=df2$Age, breaks=c(0, 47, 55, 60, 78), right=F)

# RestingBP -> bin
df2$RestingBP <- cut(x = df2$RestingBP, breaks = c(0, 121, 131, 141, 201))

# MaxHR -> bin
df2$MaxHR <- cut(x=df2$MaxHR, breaks = c(0, 121, 141, 158, 196))

H.df2 <- subset(df2, select= -c(Cholesterol)) # Choleterol이 없는 df

plot_network = function(dag,strength_df=NULL,undirected=FALSE,
                        group=NA,title=NULL,height=NULL,width=NULL)
{
  edge_size = ifelse(is.null(strength_df),NA,
                     right_join(strength_df, data.frame(dag$arcs[,c(1,2)]))$strength)
  
  nodes = names(dag$nodes)
  nodes = data.frame(id   = nodes,
                     label= nodes,
                     size = 16,
                     font.size= 18,
                     shadow   = TRUE,
                     group    = group)
  
  edges = data.frame(from   = dag$arcs[,1],
                     to     = dag$arcs[,2],
                     value  = edge_size,
                     arrows = list(to=list(enabled=TRUE,scaleFactor=.5)),
                     shadow = TRUE)
  
  if(is.na(group[1]))     nodes = nodes[,-6] # without group
  if(is.na(edge_size)) edges = edges[,-3] # without edge_size
  if(undirected)       edges$arrows.to.enabled=FALSE
  
  network=visNetwork(nodes,edges,main=title,height=height, width=width)%>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
  return(network)
}

#### DAG에 포함되어야할 엣지들 -> 앞선 분석의 EDA를 바탕으로 판단 ####

white_list = matrix(c("ChestPainType", "HeartDisease", "ExerciseAngina", "HeartDisease",
                      "Oldpeak", "HeartDisease", "ST_Slope", "HeartDisease"),
                    ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))

#### Bayesian Network [Heart Disease] : Grow-Shrink Algorithm ####
H.dag2 <- gs(H.df2, whitelist = white_list)
H.dag2

plot(H.dag2)
H.dag2 <- drop.arc(H.dag2, "Sex", "MaxHR") # Sex -> MaxHR 방향성 제거 (Undirected arcs 제거)
H.fit2 <- bn.fit(H.dag2, H.df2) 
bn.fit.barchart(H.fit2$HeartDisease)

H.fit2$HeartDisease$prob

H.group2 = ifelse(names(H.dag2$nodes)%in%c("HeartDisease"),2,1)

plot_network(H.dag2,group=H.group2,title="H.dag2_fitting")
strength_H2 = boot.strength(H.df2, R = 200, algorithm = "gs",
                            algorithm.args = list(whitelist = white_list))
strength_H2
# strength는 bootstrap sample에서 순서에 상관없이 from → to나 to→ from의 연결이 나온 확률
# direction은 bootstrap sample에서 from → to의 연결이 나온 확률
attr(strength_H2, 'threshold') # 임계값을 넘는 값들만  
average_H2 = averaged.network(strength_H2)

plot_network(average_H2, strength_H2, group=H.group2) 

#### Cholesterol Bayesian Network GS Algorithm ####

df <- read.csv("train.csv")

## Oldpeak : 0과 0이 아닌값으로 범주화
df$Oldpeak <- ifelse(df$Oldpeak == 0, "0", "1")
df$Oldpeak <- as.factor(df$Oldpeak)

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

df2 <- df

# Age -> bin : 분위수를 기준값으로 4분위로 범주화
df2$Age <- cut(x=df2$Age, breaks=c(0, 47, 55, 60, 78), right=F)

# RestingBP -> bin
df2$RestingBP <- cut(x = df2$RestingBP, breaks = c(0, 121, 131, 141, 201))

df2$Cholesterol <- cut ( x = df2$Cholesterol, breaks = c(0, 200, 239, 604), right = F,
                         labels = c('desirable', 'borderline high', 'high'))
plot(df2$Cholesterol, horiz = TRUE, main = 'Cholesterol barplot')

# MaxHR -> bin
df2$MaxHR <- cut(x=df2$MaxHR, breaks = c(0, 121, 141, 158, 196))

C.df2 <- subset(df2, select= -c(HeartDisease)) # HeartDisease이 없는 df

C.dag2 <- gs(C.df2)
C.dag2
plot(C.dag2)

# 수정
C.dag2 <- drop.arc(C.dag2, "Age", "RestingBP") # Age - RestingBP 제거 (Undirected arcs 제거)
C.fit2 <- bn.fit(C.dag2, C.df2)

bn.fit.barchart(C.fit2$Cholesterol)
C.fit2

C.group2 = ifelse(names(C.dag2$nodes)%in%c("Cholesterol"),2,1)

plot_network(C.dag2,group=C.group2,title="C.dag2_fitting")
strength_C2 = boot.strength(C.df2, R = 200, algorithm = "gs")
strength_C2
# strength는 bootstrap sample에서 순서에 상관없이 from → to나 to→ from의 연결이 나온 확률
# direction은 bootstrap sample에서 from → to의 연결이 나온 확률
attr(strength_C2, 'threshold') # 임계값을 넘는 값들만  
average_C2 = averaged.network(strength_C2)

plot_network(average_C2, strength_C2, group=C.group2) 
