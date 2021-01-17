library(chron)
library(dplyr)
library(MASS)
library(ggplot2)
library(data.table)
library(splitstackshape)
library(tidyr)
library(dplyr)
library(reticulate)

#score the data using the python script
use_python("/Users/strickla/Desktop/WPy64-3680/python-3.6.8.amd64/python.exe", required=T)

os <- import("os")

os$chdir("/Users/strickla/Documents/MARSOT/MARSOT_AY20")

source_python('CFT_APFT_Scoring.py')
marsot_spring_scored<-score_data("MARSOT_Spring_Scores_Raw.csv", "MARSOT_Spring_Scores_Final.csv")

#take out what we don't care about:
keeps<-c("Name", "Alpha", "Squad", "Gender", "Ocourse_Time", "Ecourse_Time", 
         "Rifle_Range", "CFT_Score", "APFT_Score")
marsot<-marsot_spring_scored[, (colnames(marsot_spring_scored) %in% keeps)]

#rank APFT and CFT based on combined scores (already includes the male/female separation) [Fastest Time = 1 (descending order)] 
marsot$APFT_rank<-rank(desc(marsot$APFT_Score))
marsot$CFT_rank<-rank(desc(marsot$CFT_Score))

#split into Males and Females:
marsot_female<-subset(marsot, Gender=="F")
marsot_male<-subset(marsot, Gender=="M")


#not until the Spring Screener
#mean(marsot_male$Ocourse_Time[marsot_male$Screener=="Spring"])
#mean(marsot_male$Ocourse_Time[marsot_male$Screener=="Fall"], na.rm = TRUE)
#mean(marsot_female$Ocourse_Time[marsot_female$Screener=="Spring"])
#mean(marsot_female$Ocourse_Time[marsot_female$Screener=="Fall"], na.rm = TRUE)


#Score the Ecourse and Ocourse by gender

# Males (Ecourse):
# Step 1 
min_ecourse_male<-min(marsot_male$Ecourse_Time)
min_ecourse_male
# Step 2
marsot_male$Ecourse_score<-100-ceiling((marsot_male$Ecourse_Time-min_ecourse_male)/10)
marsot_male$Ecourse_score
# Step 3
tt<-which((marsot_male$Ecourse_score<0)) #four negative scores, set scores = 0
marsot_male$Ecourse_score<-ifelse(marsot_male$Ecourse_score<0, 0, marsot_male$Ecourse_score)
marsot_male[tt,]

#Males (Ocourse)
# Step 1
min_ocourse_male<-min(na.omit(marsot_male$Ocourse_Time))
min_ocourse_male
# Step 2
marsot_male$Ocourse_score<-100-ceiling((marsot_male$Ocourse_Time-min_ocourse_male)/4)
marsot_male$Ocourse_score<-ifelse(is.na(marsot_male$Ocourse_score), 0, marsot_male$Ocourse_score) #DNF (score = 0)
#Step 3
which((marsot_male$Ocourse_score<0)) 
marsot_male$Ocourse_score<-ifelse(marsot_male$Ocourse_score<0, 1, marsot_male$Ocourse_score)
#######################

#Females (Ecourse):
# Step 1 
min_ecourse_female<-min(marsot_female$Ecourse_Time)
min_ecourse_female
# Step 2
marsot_female$Ecourse_score<-100-ceiling((marsot_female$Ecourse_Time-min_ecourse_female)/10)
# Step 3
tt<-which((marsot_female$Ecourse_score<0))
marsot_female$Ecourse_score<-ifelse(marsot_female$Ecourse_score<0, 0, marsot_female$Ecourse_score)
marsot_female[tt,]

# Females (Ocourse)
# Step 1
min_ocourse_female<-min(na.omit(marsot_female$Ocourse_Time))
# Step 2
marsot_female$Ocourse_score<-100-ceiling((marsot_female$Ocourse_Time-min_ocourse_female)/4)
marsot_female$Ocourse_score<-ifelse(is.na(marsot_female$Ocourse_score), 0, marsot_female$Ocourse_score) #DNF (score = 0)
#Step 3
which((marsot_female$Ocourse_score<0))
marsot_female$Ocourse_score<-ifelse(marsot_female$Ocourse_score<0, 1, marsot_female$Ocourse_score)


#bring everything back togeter:
marsot_final_scores<-rbind(marsot_female, 
                           marsot_male)

#now rank the Ecouse and Ocourse scores
marsot_final_scores$Ecourse_rank<-rank(desc(marsot_final_scores$Ecourse_score))
marsot_final_scores$Ocourse_rank<-rank(desc(marsot_final_scores$Ocourse_score))


#RR
marsot_final_scores$RR_Rank<-rank(desc(as.numeric(marsot_final_scores$Rifle_Range)))

p<- .2
r<- .1

#now total the scores:
marsot_final_scores$Initial_Scores<-p*marsot_final_scores$APFT_rank+
  p*marsot_final_scores$CFT_rank+
  p*marsot_final_scores$Ecourse_rank+
  p*marsot_final_scores$Ocourse_rank+
  r*marsot_final_scores$RR_Rank


initial_ranking<-marsot_final_scores[order(marsot_final_scores$Initial_Score),]
initial_ranking$Final_Rank<-rank((initial_ranking$Initial_Scores))

#write.csv(initial_ranking, file = "MARSOT_Ranking_Final_Spring.csv", row.names=FALSE)

#bring in the ranked dfs
t_fall<-read.csv("MARSOT_Ranking_Final_Fall2.csv")
t_spring<-read.csv("MARSOT_Ranking_Final_Spring.csv")

keeps<-c("Screener", "Name", "Alpha", "Squad", "Gender", "Ocourse_score", 
                "Ecourse_score", "Rifle_Range", "CFT_Score", "APFT_Score",
                "APFT_rank", "CFT_rank", "Ecourse_rank", "Ocourse_rank",
                "RR_Rank", "Initial_Scores", "Final_Rank")

t_fall<-t_fall[, (colnames(t_fall) %in% keeps)]
t_spring<-t_spring[, (colnames(t_spring) %in% keeps)]

df_final<-rbind(t_fall,t_spring)

#write.csv(df_final, file = "MARSOT_Ranking_Final.csv", row.names=FALSE)

#test for normality:
  #Fall
hist(t_fall$APFT_Score)
shapiro.test(t_fall$APFT_Score) #not normal
hist(t_fall$CFT_Score)
shapiro.test(t_fall$CFT_Score)  #normal
hist(t_fall$Ecourse_score)
shapiro.test(t_fall$Ecourse_score) #not normal
hist(t_fall$Ocourse_score)
shapiro.test(t_fall$Ocourse_score) #not normal
  #Spring
hist(t_spring$APFT_Score)
shapiro.test(t_spring$APFT_Score) #normal
hist(t_spring$CFT_Score)
shapiro.test(t_spring$CFT_Score)  #not normal
hist(t_spring$Ecourse_score)
shapiro.test(t_spring$Ecourse_score) #not normal
hist(t_spring$Ocourse_score)
shapiro.test(t_spring$Ocourse_score) #not normal

kruskal.test(Initial_Scores ~ Screener, data=df_final)


wilcox.test(t_fall$APFT_Score, 
            t_spring$APFT_Score, 
            alternative = "two.sided") 
mean(t_fall$APFT_Score)
sd(t_fall$APFT_Score)
mean(t_spring$APFT_Score)
sd(t_spring$APFT_Score)

wilcox.test(t_fall$CFT_Score, 
            t_spring$CFT_Score, 
            alternative = "two.sided") 

mean(t_fall$CFT_Score)
sd(t_fall$CFT_Score)
mean(t_spring$CFT_Score)
sd(t_spring$CFT_Score)

wilcox.test(t_fall$Ecourse_score, 
            t_spring$Ecourse_score, 
            alternative = "two.sided") 

mean(t_fall$Ecourse_score)
sd(t_fall$Ecourse_score)
mean(t_spring$Ecourse_score)
sd(t_spring$Ecourse_score)

wilcox.test(t_fall$Ocourse_score, 
            t_spring$Ocourse_score, 
            alternative = "two.sided") 

mean(t_fall$Ocourse_score)
sd(t_fall$Ocourse_score)
mean(t_spring$Ocourse_score)
sd(t_spring$Ocourse_score)
