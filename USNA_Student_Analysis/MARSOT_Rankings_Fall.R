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
marsot_fall_scored<-score_data("MARSOT_Fall_Scores_Raw.csv", "MARSOT_Fall_Scores_Final2.csv")




#marsot_fall <- read.csv("/Users/strickla/Documents/MARSOT/MARSOT_AY20/MARSOT_Fall_Scores_Raw.csv", header=TRUE)

#take out what we don't care about:
keeps<-c("Name", "Alpha", "Squad", "Gender", "Ocourse_Time", "Ecourse_Time", "Rifle_Range",
         "OOM1","OOM2","OOM3","OOM4","OOM5","CFT_Score", "APFT_Score")
marsot<-marsot_fall_scored[, (colnames(marsot_fall_scored) %in% keeps)]

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
    # Step 3
tt<-which((marsot_male$Ecourse_score<0)) #four negative scores, set scores = 0
marsot_male$Ecourse_score<-ifelse(marsot_male$Ecourse_score<0, 0, marsot_male$Ecourse_score)
marsot_male[tt,]
marsot[1,]
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

#OOM
#marsot_final_scores$OOM_Rank<-rank(marsot_final_scores$OOM)

#SFS
#marsot_final_scores$SFS<-ifelse(is.na(marsot_final_scores$SFS_Participation), 0, marsot_final_scores$SFS_Participation) 
#marsot_final_scores$SFS_Rank<-rank(desc(as.numeric(marsot_final_scores$SFS)))

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
  #s*marsot_final_scores$SFS_Rank+
  #o*marsot_final_scores$OOM_Rank


initial_ranking<-marsot_final_scores[order(marsot_final_scores$Initial_Score),]
initial_ranking$Final_Rank<-rank((initial_ranking$Initial_Scores))

#write.csv(initial_ranking, file = "MARSOT_Ranking_Final_Fall2.csv", row.names=FALSE)


########################################################
############# - Statistical Testing - ##################
########################################################

  #split into data by squad   pairs(LN.Score~., data=d2, panel=panel.smooth
pairs(Initial_Scores~Squad, data=initial_ranking)
lm.model<-lm(Initial_Scores~., data=na.omit(initial_ranking))
summary(lm.model)

initial_ranking$Squad<-as.factor(initial_ranking$Squad)


ggplot(initial_ranking, mapping = (aes(x=OOM5, y=Final_Rank))) +
  geom_boxplot(alpha=0)

initial_score_test<-aov(initial_ranking$Final_Rank ~ initial_ranking$OOM5)
summary(initial_score_test)


kruskal.test(Final_Rank ~ OOM5, data=initial_ranking)

shapiro.test(initial_ranking$Initial_Scores[initial_ranking$Squad==1])  #all above 0.10 except: 8  and 17
hist(initial_ranking$Initial_Scores[initial_ranking$Squad==1], breaks = 7)

wilcox.test(initial_ranking$APFT_Score[initial_ranking$OOM1=="A"], 
            initial_ranking$APFT_Score[initial_ranking$OOM5=="A"], 
            alternative = "two.sided")

mean(initial_ranking$APFT_Score[initial_ranking$OOM1=="A"])
mean(initial_ranking$APFT_Score[initial_ranking$OOM5=="A"])

hist(na.omit(initial_ranking$Initial_Scores[initial_ranking$OOM1=="A"]))
shapiro.test(initial_ranking$Initial_Scores[initial_ranking$OOM5=="C"])

hist(na.omit(initial_ranking$Initial_Scores[initial_ranking$OOM3=="C"]))
  
hist(initial_ranking$Initial_Scores)
shapiro.test(initial_ranking$Initial_Scores)

#ANOVA testing for bias towards a particular screener (Assume normality)
initial_score_test<-aov(initial_ranking$Initial_Scores ~ initial_ranking$OOM1)
summary(initial_score_test)

initial_score_test<-aov(initial_ranking$APFT_Score ~ initial_ranking$Screener)
summary(initial_score_test)

initial_score_test<-aov(initial_ranking$CFT_Score ~ initial_ranking$Screener)
summary(initial_score_test) # p-value<.05

mean(na.omit(initial_ranking$CFT_Score[initial_ranking$Screener=="Spring"]))
mean(na.omit(initial_ranking$CFT_Score[initial_ranking$Screener=="Fall"]))

initial_score_test<-aov(initial_ranking$Ecourse_score ~ initial_ranking$Screener)
summary(initial_score_test) # p-value<.05
mean(na.omit(initial_ranking$Ecourse_Time[initial_ranking$Screener=="Spring"]))
mean(na.omit(initial_ranking$Ecourse_Time[initial_ranking$Screener=="Fall"]))

initial_score_test<-aov(initial_ranking$Ocourse_score ~ initial_ranking$Screener)
summary(initial_score_test)


# Non-Parametric test
wilcox.test(initial_ranking$Initial_Scores[initial_ranking$Screener=="Spring"], 
            initial_ranking$Initial_Scores[initial_ranking$Screener=="Fall"], 
            alternative = "two.sided") #not significant
wilcox.test(initial_ranking$APFT_Score[initial_ranking$Screener=="Spring"], 
            initial_ranking$APFT_Score[initial_ranking$Screener=="Fall"], 
            alternative = "two.sided") #not significant
wilcox.test(initial_ranking$CFT_Score[initial_ranking$Screener=="Spring"], 
            initial_ranking$CFT_Score[initial_ranking$Screener=="Fall"], 
            alternative = "two.sided") # significant  p_value<.05
wilcox.test(initial_ranking$Ecourse_score[initial_ranking$Screener=="Spring"], 
            initial_ranking$Ecourse_score[initial_ranking$Screener=="Fall"], 
            alternative = "two.sided") # significant  p_value<.05
wilcox.test(initial_ranking$Ocourse_score[initial_ranking$Screener=="Spring"], 
            initial_ranking$Ocourse_score[initial_ranking$Screener=="Fall"], 
            alternative = "two.sided") #not significant

kruskal.test(Initial_Scores ~ Screener, data=initial_ranking) #not significant
kruskal.test(APFT_rank ~ Screener, data=initial_ranking)      #not significant
kruskal.test(CFT_rank ~ Screener, data=initial_ranking)       # significant p_value<.05
kruskal.test(Ecourse_rank ~ Screener, data=initial_ranking)   # significant p_value<.05
kruskal.test(Ocourse_rank ~ Screener, data=initial_ranking) 
#rbind the two screeners
#marsot_clean<-rbind(marsot_fall_clean, marsot_sprng_clean)

#write cleaned csv to file:
#write.csv(marsot_clean, file = "MARSOT_Data_Clean.csv", row.names=FALSE)

###-START HERE-#### python script to score values
  #Score the APFT based on gender

  #Score the CFT based on gender













