library(bnlearn)
library(dplyr)
library(visNetwork)

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

## Oldpeak : 0과 0이 아닌값으로 범주화
df$Oldpeak <- ifelse(df$Oldpeak == 0, "0", "1")
df$Oldpeak <- as.factor(df$Oldpeak)
df$Oldpeak

str(df)

# 변수 factor화
df$Sex <- as.factor(df$Sex)
df$ChestPainType <- as.factor(df$ChestPainType)
df$FastingBS <- as.factor(df$FastingBS)
df$RestingECG <- as.factor(df$RestingECG)
df$ExerciseAngina <- as.factor(df$ExerciseAngina)
df$ST_Slope <- as.factor(df$ST_Slope)
df$HeartDisease <- as.factor(df$HeartDisease)

# 변수 numeric화
df$Age <- as.numeric(df$Age)
df$RestingBP <- as.numeric(df$RestingBP)
df$Cholesterol <- as.numeric(df$Cholesterol)
df$MaxHR <- as.numeric(df$MaxHR)

x <- df[, -5]

# DAG에 포함되지 말아야할 엣지들
black_list = tiers2blacklist(list("Age", "Sex",
                                  c("ChestPainType", "RestingBP", "FastingBS", "RestingECG", 
                                    "MaxHR", "ExerciseAngina","Oldpeak", "ST_Slope")))

black_list = rbind(black_list, c("Age", "Sex"))
# DAG에 포함되어야할 엣지들
white_list=matrix(c("HeartDisease", "ChestPainType", "HeartDisease", "ExerciseAngina",
                    "Oldpeak", "HeartDisease", "ST_Slope", "HeartDisease", "HeartDisease", "RestingBP"),
                    ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))
?hc()
DAG = hc(x, blacklist = black_list, whitelist = white_list)

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

group = ifelse(names(DAG$nodes)%in%c("Age","Sex"),2,1)
plot_network(DAG,group=group,title="Hill-Climbing")

strength_x = boot.strength(x, R = 200, algorithm = "hc",
                              algorithm.args = list(blacklist = black_list, whitelist = white_list))

head(strength_x)
attr(strength_x, 'threshold')
average_x = averaged.network(strength_x)

plot_network(average_x, strength_x, group=group) 

DAG2 = tabu(x, blacklist = black_list, whitelist = white_list)
group = ifelse(names(DAG2$nodes)%in%c("Age","Sex"),2,1)
plot_network(DAG2,group=group,title="Tabu")

strength_x = boot.strength(x, R = 200, algorithm = "tabu",
                           algorithm.args = list(blacklist = black_list, whitelist = white_list))
sessionInfo()
head(strength_x)
attr(strength_x, 'threshold')
average_x = averaged.network(strength_x)

plot_network(average_x, strength_x, group=group) 
