# install.packages("bnlearn")

library(bnlearn)
setwd("C:/Users/82106/iwantsuccess")
df <- read.csv("train.csv")
df

str(df)

# 이상치 처리

## [Drop RestingBP Outlier] ##
sum(df$RestingBP==0); 
df <- df[!(df$RestingBP == 0 ), ] # 818개 중 1개 제거 -> 817개 

## [Drop Cholesterol Outlier] (콜레스테롤 수치 mg/dl) ##
sum(df$Cholesterol==0)
df <- df[!(df$Cholesterol == 0 ), ] # 817개 중 171개 데이터 제거 완료 -> 646개 데이터 존재

'''
## [Drop Oldpeak Outlier]
df <- df[df$Oldpeak >= 0,]
df$Oldpeak  
# ifelse(df$Oldpeak >= 0, df$Oldpeak, )
na.omit(df$Oldpeak)
sum(is.na(df$Oldpeak))
'''

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

str(df)
H.df <- subset(df, select= -c(Cholesterol)) # Choleterol이 없는 df
head(H.df)

C.df <- subset(df, select= -c(HeartDisease)) # HeartDisease가 없는 df
head(C.df)

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

# Bayesian Network for HeartDisease: Grow-Shrink Algorithm
# DAG에 포함되면 안되는 엣지들
black_list = tiers2blacklist(list("Age", "Sex",
                                  c("ChestPainType", "RestingBP", "FastingBS", "RestingECG", 
                                    "MaxHR", "ExerciseAngina","Oldpeak", "ST_Slope")))

black_list = rbind(black_list, c("Age", "Sex"))

# DAG에 포함되어야할 엣지들 -> 승현이가 세운 가설 바탕으로 넣음 (다같이 고민해보기)
white_list=matrix(c("HeartDisease", "ChestPainType", "HeartDisease", "ExerciseAngina",
                    "Oldpeak", "HeartDisease", "ST_Slope", "HeartDisease", "HeartDisease", "RestingBP"),
                  ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))

H.dag <- gs(H.df, blacklist = black_list, whitelist = white_list)
H.dag
# 노드의 진행 방향을 사전에 아는 경우, 즉 실험 설정에서 사전 지식을 바탕으로 인과관계를 직접 설정하는 방법
# H.dag = pdag2dag(H.dag, ordering = "")
plot(H.dag)
str(H.df)
# 이산형노드는 다른 이산형 노드만을 부모로 가질 수 있기에 
attributes(H.dag)
H.dag$learning
H.dag$nodes$HeartDisease
H.dag$arcs

H.group = ifelse(names(H.dag$nodes)%in%c("Age","Sex"),2,1)
plot_network(H.dag,group=H.group,title="H.dag_fitting")

strength_H1 = boot.strength(H.df, R = 200, algorithm = "gs",
                           algorithm.args = list(blacklist = black_list, whitelist = white_list))
strength_H1
# strength는 bootstrap sample에서 순서에 상관없이 from → to나 to→ from의 연결이 나온 확률
# direction은 bootstrap sample에서 from → to의 연결이 나온 확률
attr(strength_H1, 'threshold') # 임계값을 넘는 값들만  
average_H1 = averaged.network(strength_H1)

plot_network(average_H1, strength_H1, group=H.group) 

# undirected.arcs(H.dag)
# 임의로 방향성 제공해주기 
# Sex -> ChestPainType, RestingBP <- Age <- MaxHR, ST_Slope -> HeartDisease
H.fit <- bn.fit(H.dag, H.df) 
H.fit

summary(H.fit)

bn.fit.barchart(H.fit$HeartDisease)

# Bayesian Network about Choletersol : Grow-Shrink Algorithm 
C.dag <- gs(C.df)
C.dag

plot(C.dag)

# 임의로 방향성 제공해주기 
# RestingBP <- Age 
C.dag <- set.arc(C.dag, "Age", "RestingBP")

C.fit <- bn.fit(C.dag, C.df) 
C.fit

summary(C.fit)

C.group = ifelse(names(C.dag$nodes)%in%c("Age","Sex"),2,1)
plot_network(C.dag,group=C.group,title="C.dag_fitting")


# 연속형 변수 시각화 (Histogram of the Residuals for Node)
# bn.fit.xyplot, bn.fit.qqplot, bn.fit.histogram

# 범주형 변수 시각화 (Conditional Probablity for Node)
# bn.fit.dotplot, bn.fit.barchart

# 모든 변수 범주화 
df2 <- df
# Age -> bin : 분위수를 기준값으로 4분위로 범주화
df2$Age <- cut(x=df2$Age, breaks=c(0, 47, 55, 60, 78), right=F) 
plot(df2$Age)

# Choleterol -> bin : 분위수를 기준값으로 4분위로 범주화
df2$Cholesterol <- cut(x=df2$Cholesterol, breaks=c(0, 209, 238, 276, 604), right=F)
plot(df2$Cholesterol)

df2$RestingBP <- cut(x = df2$RestingBP, breaks = c(0, 121, 131, 141, 201))

# MaxHR : 분위수 대로 범주화
df2$MaxHR <- cut(x=df2$MaxHR, breaks = c(0, 121, 141, 158, 196))

str(df2)

H.df2 <- subset(df2, select= -c(Cholesterol))
C.df2 <- subset(df2, select= -c(HeartDisease))

# Bayesian Network : Grow-Shrink Algorithm
H.dag2 <- gs(H.df2, blacklist = black_list, whitelist = white_list)
H.dag2

plot(H.dag2)

# H.dag2 <- set.arc(H.dag2, "Sex", "MaxHR") # 방향성 제공 Sex -> MaxHR
# H.dag2 <- set.arc(H.dag2, "Age", "Oldpeak") # 방향성 제공 Age -> Oldpeak
# H.dag2 <- set.arc(H.dag2, "ST_Slope", "HeartDisease") # 방향성 제공 Sex -> HeartDisease
H.dag2 <- drop.arc(H.dag2, "ChestPainType", "ExerciseAngina")
H.fit2 <- bn.fit(H.dag2, H.df2) 
H.fit2

summary(H.fit2)

df2 %>%
  filter(HeartDisease == 0 & Oldpeak == 0) %>% View()

bn.fit.barchart(H.fit2$HeartDisease)

H.group2 = ifelse(names(H.dag2$nodes)%in%c("Age","Sex"),2,1)
plot_network(H.dag2,group=H.group2,title="H.dag2_fitting")

C.dag2 <- gs(C.df2, blacklist = black_list, whitelist = white_list)
plot(C.dag2)
# C.dag2 <- set.arc(C.dag2, "Sex", "MaxHR") # 방향성 제공 Sex -> MaxHR
# C.dag2 <- set.arc(C.dag2, "Age", "Oldpeak") # 방향성 제공 Age -> Oldpeak

C.fit2 <- bn.fit(C.dag2, C.df2)
C.fit2

bn.fit.dotplot(C.fit2$ExerciseAngina)

C.group2 = ifelse(names(C.dag2$nodes)%in%c("Age","Sex"),2,1)
plot_network(C.dag2,group=C.group2,title="C.dag2_fitting")

