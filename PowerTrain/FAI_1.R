library(plyr)
library(text2vec)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(readr)
library(SnowballC)
library(ggplot2)
library(treemap)
library(magrittr)

#set working directory (must be changed to run on different computer)
setwd("/Users/colemanstrickland/Documents/Coleman/PowerTrain/")

#bring in data sets containing the compentencies, keep the titles of the columns and delineate by , 
FAI_COR_1 <- read.table("/Users/colemanstrickland/Documents/Coleman/PowerTrain/FAC_COR_1_raw.csv",
                       header=TRUE, 
                       stringsAsFactors = F,
                       quote="\"",sep=",")
FAI_COR_2 <- read.table("/Users/colemanstrickland/Documents/Coleman/PowerTrain/FAC_COR_2_raw.csv",
                        header=TRUE, 
                        stringsAsFactors = F,
                        quote="\"",sep=",")
FAI_COR_3 <- read.table("/Users/colemanstrickland/Documents/Coleman/PowerTrain/FAC_COR_3_raw.csv",
                        header=TRUE, 
                        stringsAsFactors = F,
                        quote="\"",sep=",")
FAI_PPM_1 <- read.table("/Users/colemanstrickland/Documents/Coleman/PowerTrain/FAC_PPM_1_raw.csv",
                        header=TRUE, 
                        stringsAsFactors = F,
                        quote="\"",sep=",")
FAI_PPM_2 <- read.table("/Users/colemanstrickland/Documents/Coleman/PowerTrain/FAC_PPM_2_raw.csv",
                        header=TRUE, 
                        stringsAsFactors = F,
                        quote="\"",sep=",")
FAI_PPM_3 <- read.table("/Users/colemanstrickland/Documents/Coleman/PowerTrain/FAC_PPM_3_raw.csv",
                        header=TRUE, 
                        stringsAsFactors = F,
                        quote="\"",sep=",")
ITPM <- read.table("/Users/colemanstrickland/Documents/Coleman/PowerTrain/ITPM_raw.csv",
                   header=TRUE, 
                   stringsAsFactors = F,
                   quote="\"",sep=",")

#bring in master file of objectives, convert to utf8 encoding as above
#MF <- read.table("/Users/colemanstrickland/Documents/Coleman/PowerTrain/2.5_Master_File_raw.csv",
#MF <- read.table("/Users/colemanstrickland/Documents/Coleman/PowerTrain/COR_master_file_raw.csv",
#MF <- read.table("/Users/colemanstrickland/Documents/Coleman/PowerTrain/PPM_1_master_file_raw.csv",
MF <- read.table("/Users/colemanstrickland/Documents/Coleman/PowerTrain/PPM_2_master_file_raw.csv",
                 header=TRUE, 
                 stringsAsFactors = F,
                 quote="\"", sep=",")
MF$TLO.ELO <- iconv(enc2utf8(MF$TLO.ELO),sub="byte")

#build a vector of stopwords:
mystopwords<-c("use","that","can","be","used")#, "analysis", "one", "two", "three", "model", "will")
stopWords <- c(stopwords("en"),mystopwords)

#prep_fun1: cleans the data in order to strip the text down to base terms and eliminate stopwords
#output: text vector containing the cleaned ELOs/TLOs
prep_fun1 = function(x){ #input is a column of a dataframe (string format)
  word_list<-c()      #empty vector
  for (i in 1:length(x)){
    w<-x[i]
    words<-Corpus(VectorSource(w))
    words <- tm_map(words, content_transformer(tolower))
    words<-  tm_map(words, stripWhitespace)
    words <- tm_map(words, removePunctuation)
    words <- tm_map(words, removeWords, stopWords)
    words <- tm_map(words, stemDocument)
    word_list[i]<-words[1]$content
  }   
  return (word_list)}

#function to convert incoming csv files to utf8 encoding, eliminates issues such as (', ", etc.)
clean_comp_data <- function(df){
  df$Performance.Outcome <- iconv(enc2utf8(df$Performance.Outcome),sub="byte")
  df$Search.Terms <- iconv(enc2utf8(df$Search.Terms ),sub="byte")
  df$Clean <- prep_fun1(df$Performance.Outcome)
  return (df) }

jaccard_token= function(x,y){                  #"clean" columns of a dataframe
  it1 = itoken(x, progressbar = FALSE)
  it2 = itoken(y, progressbar = FALSE)
  v = create_vocabulary(it2) %>% prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
  vectorizer = vocab_vectorizer(v)
  
  dtm1 = create_dtm(it1, vectorizer)
  
  dtm2 = create_dtm(it2, vectorizer)
  
  d1_d2_jac_sim = sim2(dtm1, dtm2, method = "jaccard", norm = "none")
  
  return (d1_d2_jac_sim)}

cosine_token= function(x,y){                      
  it1 = itoken(x, progressbar = FALSE)#, tokenizer = BigramTokenizer )
  it2 = itoken(y, progressbar = FALSE)#, tokenizer = BigramTokenizer )
  v = create_vocabulary(it2) %>% prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
  vectorizer = vocab_vectorizer(v)
  
  dtm1 = create_dtm(it1, vectorizer)
  
  dtm2 = create_dtm(it2, vectorizer)
  
  d1_d2_cos_sim = sim2(dtm1, dtm2, method = "cosine", norm = "l2")  #changed to "cosine"
  
  return (d1_d2_cos_sim)}

retrieve_topn = function(mat,master_df,term_list,n_terms){    #mat,n,df){
  t<-as.data.frame(as.matrix(mat))
  final_df <- as.data.frame(t(t))
  colnames(final_df) <- "score"
  final_df$names <-as.numeric(rownames(final_df))
  #print(length(final_df$names))
  course_num <- c()
  TLO.ELO_num<- c()
  sim_score<-c()
  objective<-c()
  for (i in 1:length(final_df$names)){
    index<-final_df$names[i]
    course_num[i]<-master_df$Course.Number[index]
    TLO.ELO_num[i]<-master_df$TLO.ELO.Number[index]
    objective[i]<-master_df$TLO.ELO[index]
    sim_score[i]<-final_df$score[index]
  }
  
  scored_data<-data.frame(Course.Number = course_num,
                          Competency.Number_TLO.ELO.Number=TLO.ELO_num,
                          Performance.Outcome_Objective=objective, 
                          Similarity.Score=sim_score)
  scored_data2<-subset(scored_data, Similarity.Score > 0)
  #print(length(scored_data2$Course.Number))
  scored_data2<-scored_data2[rev(order(scored_data2$Similarity.Score)),]
  
  if (length(scored_data2$Course.Number)==0){
    return (scored_data2)
  }
  
  term_list<-strsplit(term_list, split=", ")[[1]]
  #print(term_list)
  word_list1<-c()
  for (i in 1:length(term_list)){
    w<-term_list[i]
    words<-Corpus(VectorSource(w))
    words <- tm_map(words, content_transformer(tolower))
    words<-  tm_map(words, stripWhitespace)
    words <- tm_map(words, removePunctuation)
    words <- tm_map(words, removeWords, stopWords)
    words <- tm_map(words, stemDocument)
    word_list1[i]<-words[1]$content
  }   
  #print("here1")   
  word_list2<-c()
  #print(length(scored_data2$Performance.Outcome_Objective))
  for (i in 1:length(scored_data2$Performance.Outcome_Objective)){
    w<-scored_data2$Performance.Outcome_Objective[i]
    words<-Corpus(VectorSource(w))
    words <- tm_map(words, content_transformer(tolower))
    words<-  tm_map(words, stripWhitespace)
    words <- tm_map(words, removePunctuation)
    words <- tm_map(words, removeWords, stopWords)
    words <- tm_map(words, stemDocument)
    word_list2[i]<-words[1]$content
  }   
  
  scored_data2$clean<-word_list2
 
  term_split<-strsplit(word_list1, split=" ")
  
  tt<-c()
  count<-1
  for (i in 1:length(scored_data2$clean)){ 
    for (j in term_split){
      l<-length(j)
      t<-ngrams(words(scored_data2$clean[i]), l)
      for (item in t){
        if (sum(item %in% j) == l){
          tt<-c(tt,count)
          break}
      }
    }
    count<-count+1
  }
  
  scored_data3<-scored_data2[tt,]
  #print(length(scored_data3$clean))
  if (length(scored_data3$clean)<n_terms){
    #print("hi")
    scored_data4<-search_for_exact_terms(master_df,term_split)  #
  
    scored_data3<-data.frame(Course.Number = scored_data4$Course.Number,
                            Competency.Number_TLO.ELO.Number=scored_data4$TLO.ELO.Number,
                            Performance.Outcome_Objective=scored_data4$TLO.ELO, 
                            Similarity.Score=rep("Not enough similar matches",length(scored_data4$Course.Number)),
                            clean = rep("NA",length(scored_data4$Course.Number)))
    
    }
  scored_data3<-unique(scored_data3)
  if (length(scored_data3$clean)>30){
    scored_data3<-scored_data3[1:30,]
  }
  #print("here3")
  return (scored_data3) }

search_for_exact_terms= function(master_df,term_list){    #mat,n,df){
  #print(term_list)
  #term_list<-strsplit(term_list, split=", ")[[1]]
  #term_split<-strsplit(term_list, split=" ")
  #print(term_split)
  tt<-c()
  count<-1
  for (i in 1:length(master_df$Clean)){
    for (j in term_list){
      l<-length(j)
      t<-ngrams(words(master_df$Clean[i]), l)
      for (item in t){
        if (sum(item %in% j) == l){
          tt<-c(tt,count)
          break}
      }
    }
    count<-count+1}
  
  df1<-master_df[tt,]
  
  return (df1)}

compare_text <- function(master_df, comp_df, blank_output){
  final_df<-blank_output
  for (i in 1:length(comp_df$Clean)){ #iterate through each compentency    length(comp_df$Clean)
    firstmethod<-cosine_token(comp_df$Clean[i],master_df$Clean)  #find similar text in the master_df
    topn1<-retrieve_topn(firstmethod,master_df,comp_df$Search.Terms[i], 5)
    topn1$Competency.Number_TLO.ELO.Number<-as.character(topn1$Competency.Number_TLO.ELO.Number)
    topn1$Similarity.Score<-as.character(topn1$Similarity.Score)
    updated_output_df<-rbind(data.frame(Course.Number="NA",
                                        Competency.Number_TLO.ELO.Number=comp_df$Competency.Number[i],
                                        Performance.Outcome_Objective=comp_df$Performance.Outcome[i], 
                                        Similarity.Score="NA", clean="NA"), 
                             topn1)
    final_df<-rbind(final_df,updated_output_df)
    #print("here4")
  }
return (final_df)}

blank_output <- data.frame(Course.Number=character(),
                           Competency.Number_TLO.ELO.Number=character(),
                           Performance.Outcome_Objective=character(), 
                           Similarity.Score=character(),
                           stringsAsFactors=FALSE)

#clean the MF data set and create a column of "clean" text
MF$Clean<-prep_fun1(MF$TLO.ELO)

#execute code on the each compentency data set:
FAI_COR_1_clean <- clean_comp_data(FAI_COR_1)
FAI_COR_1_complete <- compare_text(MF, FAI_COR_1_clean, blank_output)
FAI_COR_1_complete$clean<-NULL
write.csv(FAI_COR_1_complete, file = "FAC_COR_1_complete.csv", row.names = FALSE)


FAI_COR_2_clean <- clean_comp_data(FAI_COR_2)
FAI_COR_2_complete <- compare_text(MF, FAI_COR_2_clean, blank_output)
FAI_COR_2_complete$clean<-NULL
write.csv(FAI_COR_2_complete, file = "FAC_COR_2_complete.csv", row.names = FALSE)

FAI_COR_3_clean <- clean_comp_data(FAI_COR_3)
FAI_COR_3_complete <- compare_text(MF, FAI_COR_3_clean, blank_output)
FAI_COR_3_complete$clean<-NULL
write.csv(FAI_COR_3_complete, file = "FAC_COR_3_complete.csv", row.names = FALSE)

FAI_PPM_1_clean <- clean_comp_data(FAI_PPM_1)
FAI_PPM_1_complete <- compare_text(MF, FAI_PPM_1_clean, blank_output)
FAI_PPM_1_complete$clean<-NULL
write.csv(FAI_PPM_1_complete, file = "FAC_PPM_1_complete.csv", row.names = FALSE)

FAI_PPM_2_clean <- clean_comp_data(FAI_PPM_2)
FAI_PPM_2_complete <- compare_text(MF, FAI_PPM_2_clean, blank_output)
FAI_PPM_2_complete$clean<-NULL
write.csv(FAI_PPM_2_complete, file = "FAC_PPM_2_complete.csv", row.names = FALSE)

FAI_PPM_3_clean <- clean_comp_data(FAI_PPM_3)
FAI_PPM_3_complete <- compare_text(MF, FAI_PPM_3_clean, blank_output)
FAI_PPM_3_complete$clean<-NULL
write.csv(FAI_PPM_3_complete, file = "FAC_PPM_3_complete.csv", row.names = FALSE)

ITPM_clean <- clean_comp_data(ITPM)
ITPM_complete <- compare_text(MF, ITPM_clean, blank_output)
ITPM_complete$clean<-NULL
write.csv(ITPM_complete, file = "ITPM_complete.csv")


#####################################
#####################################
#####################################

firstmethod<-cosine_token(FAI_COR_1_clean$Clean[2],MF$Clean) 
topn1<-retrieve_topn(firstmethod,MF,FAI_COR_1_clean$Search.Terms[2],5)
topn1<-unique(topn1)
x<-search_for_exact_terms(MF,FAI_COR_1_clean$Search.Terms[2])

topn1$Competency.Number_TLO.ELO.Number<-as.character(topn1$Competency.Number_TLO.ELO.Number)
topn1$Similarity.Score<-as.character(topn1$Similarity.Score)

updated_output_df<-rbind(data.frame(Course.Number="NA",
                                    Competency.Number_TLO.ELO.Number=FAI_COR_1_clean$Competency.Number[1],
                                    Performance.Outcome_Objective=FAI_COR_1_clean$Performance.Outcome[1], 
                                    Similarity.Score="NA", clean="NA"), 
                         topn1)


x<-data.frame(Course.Number="NA",
           Competency.Number_TLO.ELO.Number=as.double(FAI_COR_1_clean$Competency.Number[1]),
           Performance.Outcome_Objective=FAI_COR_1_clean$Performance.Outcome[1], 
           Similarity.Score="New Performance Outcome", clean="New Performance Outcome")

y<-rbind(x,topn1)





as.double(FAI_COR_1_clean$Competency.Number[1])








term_split<-strsplit(FAI_COR_1_clean$Search.Terms[1], split=", ")[[1]]
term_split
[1] "source selection plan, acquisition strategy"
[1] "source selection plan" "acquisition strategy" #correct 



topn1$Performance.Outcome_Objective
t<-as.data.frame(as.matrix(firstmethod))
final_df <- as.data.frame(t(t))
final_df$`1`
final_df<-subset(final_df, `1` > 0)
length(final_df$`1`)














#execute the clean function to ensure utf8 encoding is valid on the compentency dataframes
FAI_COR_1_clean <- clean_comp_data(FAI_COR_1)
FAI_COR_2_clean <- clean_comp_data(FAI_COR_2)
FAI_COR_3_clean <- clean_comp_data(FAI_COR_3)
FAI_PPM_1_clean <- clean_comp_data(FAI_PPM_1)
FAI_PPM_2_clean <- clean_comp_data(FAI_PPM_2)
FAI_PPM_3_clean <- clean_comp_data(FAI_PPM_3)
FAI_ITPM_clean <- clean_comp_data(ITPM)






test1<-"Ability to participate on an acquisition strategy or source selection panel under close monitoring, if requested."
key_words1<-c("source selection plan","acquisition strategy")
test_df<-data.frame(clean=test1)
test_df$clean<-as.character(test_df$clean)
firstmethod<-cosine_token(test_df$clean, fai_data$clean)
topn1=retrieve_topn(firstmethod,fai_data,key_words1,5)
rownames(topn1) = 1:nrow(topn1)
topn1<-cbind(data.frame(Compentency_Number = 1, Compentency = test1, Key_Words = t(key_words1)), topn1)
write.csv(topn1, file = "Test1.csv")








y<-FAI_COR_1_clean$Search.Terms[1]
y
x<-strsplit(FAI_COR_1_clean$Search.Terms[1], ", ")[[1]]
x[1]


MF$Index[2]
MF$Course.Number[2]



