library(readtext)
library(magrittr)
library(assertr)
library(widyr)
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(reshape2)
library(wordcloud)
library(plyr)
library(text2vec)
library(tm)
library(readr)
library(SnowballC)
library(dplyr)
library(tidytext)
library(RSiteCatalyst)
library(proxy)
library(cluster)
library(fpc)

library(chron)
library(faraway)
library(dplyr)
library(MASS)
library(ggplot2)
library(data.table)
library(splitstackshape)
library(tidyr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(adabag)
library(recipes)
library(caret)
library(ada)
library(devtools)
library(gbm)
library(xgboost)
library(nnet)
library(neuralnet)
library(randomForest)
library(faraway)
library(textdata)
library(stringr)
library(leaps)
tinytex::install_tinytex()
tinytex:::install_prebuilt()

comment_df<-read.csv("/Users/strickla/Documents/LNK_Project/LNK_Comments/LNK_Comments_final.csv", header=T)

q1<-na.omit(as.character(comment_df$Q1))
q1r<-rep("q1", length(q1))
q2<-na.omit(as.character(comment_df$Q2))
q2r<-rep("q2", length(q2))
q3<-na.omit(as.character(comment_df$Q3))
q3r<-rep("q3", length(q3))
q4<-na.omit(as.character(comment_df$Q4))
q4r<-rep("q4", length(q4))
q5<-na.omit(as.character(comment_df$Q5))
q5r<-rep("q5", length(q5))
q6<-na.omit(as.character(comment_df$Q6))
q6r<-rep("q6", length(q6))
q7<-na.omit(as.character(comment_df$Q7))
q7r<-rep("q7", length(q7))

text_all<-data.frame(Text=c(q1,q2,q3,q4,q5,q6,q7), 
                            Question=c(q1r,q2r,q3r,q4r,q5r,q6r,q7r))

text_lines<-tibble(line=1:length(text_all$Text), 
                   text = as.character(text_all$Text))
text_lines
#add sentiments
all_lines <- text_lines  %>% ungroup() %>% 
  unnest_tokens(word, text)# %>% 
  #mutate(linenumber = row_number())
all_lines
s <- all_lines %>% filter(!word %in% stop_words$word) %>%
  inner_join(get_sentiments("afinn"), by="word") %>%  
  group_by(line) %>% 
  summarise(sentiment = sum(value)) %>% mutate(method="afinn")
s

#use bing sentiment
#s <- all_lines %>% #filter(!word %in% stop_words$word) %>%
#  inner_join(get_sentiments("bing"), by="word") %>%  
#  group_by(line) %>%
#  count(line, index=line, sentiment) %>%
#  spread(sentiment, n, fill=0) %>%
#  mutate(sentiment = positive - negative) %>% summarise(sentiment = sum(sentiment))
#s


sentiment<-c()
count<-0
for (i in 1:length(text_all[,1])){
  if (i==s$line[i-count] & i<=length(s$line)){
    sentiment[i]<-s$sentiment[i-count]
  }
  else{
    sentiment[i]<-0
    count<-count+1
  }
}
text_all$sentiment<-sentiment
text_lines2<-tibble(line=1:length(text_all$Text), 
          text = as.character(text_all$Text), 
          sentiment = as.character(text_all$sentiment))

#text_lines2
y <- text_lines2 %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)
y

 #word count: 
 #word_count<-y %>% count(word, sort=T)

#network of word pairs appearing in the same sentence:
word_pairs <- y %>% 
  pairwise_count(word, line, sort=T, upper=F) #get pairs of words
word_pairs$Occurence<-word_pairs$n
word_pairs %>% filter(n>=5) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout="fr") +
  geom_edge_link(aes(edge_alpha=Occurence, edge_width = Occurence), 
                 edge_colour="cyan4")+
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(.25, "lines")) #+
  #guides(colour = guide_legend(override.aes = list(alpha=1)))

#most positive and negative words
y$sentiment<-NULL
bing_word_counts <- y %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(word, sentiment, sort = TRUE) 
bing_word_counts

#build wordcloud
set.seed(1233)
y %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)

#sentiment by questions
#text_q <- tibble(id = text_all$Question, 
#                    text = as.character(text_all$Text))
text_lines2[69,]
#bigram network (n=2)
comment_bigrams <-  text_lines2 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- comment_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

 #negation_words <- c("not", "no", "never", "without")

 #negated_words <- bigrams_separated %>%
  # filter(word1 %in% negation_words) %>%
  #inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  #count(word1, word2, value, sort = TRUE)
 #negated_words

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigrams_filtered<-na.omit(bigrams_filtered)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
 #bigram_counts


avg_sent_total<-c()
for (i in 1:length(bigram_counts$word1)){
  avg_sent<-c()
  for (j in 1:length(bigrams_filtered$sentiment)){
    if (bigram_counts$word1[i]==bigrams_filtered$word1[j] & 
        bigram_counts$word2[i]==bigrams_filtered$word2[j]){
      avg_sent<-c(avg_sent, as.integer(bigrams_filtered$sentiment[j]))
    }
  }
  avg_sent_total[i]<-mean(avg_sent)
}
bigram_counts$sentiment<-avg_sent_total
bigram_counts$s<-bigram_counts$sentiment
bigram_counts$s[bigram_counts$sentiment<0] = "negative sentiment" 
bigram_counts$s[bigram_counts$sentiment>0] = "positive sentiment" 
bigram_counts$s[bigram_counts$sentiment==0] = "neutral" 
bigram_counts$Sentiment<-as.factor(bigram_counts$s)
bigram_graph <- bigram_counts %>%
  filter(n >= 2) %>%
  graph_from_data_frame()

set.seed(1233)
a <- grid::arrow(type = "closed", length = unit(.1, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_colour=Sentiment), show.legend = T,
                 arrow = a, end_cap = circle(.05, 'inches')) +
  geom_node_point(color = "grey", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) #+
  theme_dark()

###############################################################
##############-Comparison-#####################################
###############################################################
  #group comments in each question based on the number of similar comments (5 or more) or similarity score (.4 or higher)
myvars<-c('Q1')
test<-na.omit(comment_df[myvars])
test

t<-prep_fun1(test)
t

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

jaccard_token= function(x,y){                  #"clean" columns of a dataframe
  it1 = itoken(x, progressbar = FALSE)
  it2 = itoken(y, progressbar = FALSE)
  v = create_vocabulary(it2) %>% prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 2)
  vectorizer = vocab_vectorizer(v)
  
  dtm1 = create_dtm(it1, vectorizer)
  
  dtm2 = create_dtm(it2, vectorizer)
  
  d1_d2_jac_sim = sim2(dtm1, dtm2, method = "jaccard", norm = "none")
  
  return (d1_d2_jac_sim)}

cosine_token= function(x,y){                     
  it1 = itoken(x, progressbar = FALSE)#, tokenizer = BigramTokenizer )
  it2 = itoken(y, progressbar = FALSE)#, tokenizer = BigramTokenizer )
  v = create_vocabulary(it2) %>% prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 2)
  vectorizer = vocab_vectorizer(v)
  
  dtm1 = create_dtm(it1, vectorizer)
  
  dtm2 = create_dtm(it2, vectorizer)
  
  d1_d2_cos_sim = sim2(dtm1, dtm2, method = "cosine", norm = "l2")  #changed to "cosine"
  
  return (d1_d2_cos_sim)}

w<-"Leading my peers and seeing how I felt and acted as their leader. Also receiving feedback on said leadership."
words<-Corpus(VectorSource(w)) 
words <- tm_map(words, content_transformer(tolower))
words<-  tm_map(words, stripWhitespace)
words <- tm_map(words, removePunctuation)
words[1]$content
words <- tm_map(words, removeWords, stopWords)
words[1]$content
words <- tm_map(words, stemDocument)
words[1]$content



test<-na.omit(comment_df[myvars])
test
q_df<-as.character(test[,1])
q_df[46]
t<-prep_fun1(q_df)
t
x<-cosine_token(t[1],t)
x
tester<-as.data.frame(as.matrix(x))
final_df <- as.data.frame(t(tester))
colnames(final_df) <- "score"
final_df$names <-as.numeric(rownames(final_df))
f<-subset(final_df, score>.3)
f




###############################################################################
###############################################################################
t<-c()
for (i in 1:length(text_all$Text)){
  t[i]<-prep_fun1(text_all$Text[i])
  
}

text_all$t<-t
#corpus = tm::Corpus(tm::VectorSource(text_all$t[text_all$Question=='q1'])) 
corpus = tm::Corpus(tm::VectorSource(text_all$t)) 
tdm <- tm::DocumentTermMatrix(corpus) 
tdm.tfidf <- tm::weightTfIdf(tdm)
tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999) 
tfidf.matrix <- as.matrix(tdm.tfidf) 
predict_df<-as.data.frame(tfidf.matrix)

test_df<-read.csv("/Users/strickla/Documents/LNK_Project/LNK_Comments/LNK_Comments_Themes2.csv", header=T)
r<-test_df$Theme

t1<-c()
for (i in 1:length(test_df$Comments)){
  t1[i]<-prep_fun1(test_df$Comments[i])
  
}

test_df$t<-t1
corpus = tm::Corpus(tm::VectorSource(test_df$t)) 
tdm <- tm::DocumentTermMatrix(corpus) 
tdm.tfidf <- tm::weightTfIdf(tdm)
tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999) 
tfidf.matrix <- as.matrix(tdm.tfidf) 
response_df<-as.data.frame(tfidf.matrix)
response_df$response<-r

boost_model<-boosting(response~., response_df, boos=T, mfinal=100)

p_boost<-predict(boost_model, predict_df, type="prob")
text_all$Class<-p_boost$class
write.csv(text_all, file ="/Users/strickla/Documents/LNK_Project/LNK_Comments/Test_first_iter.csv")
##########################################################
##########################################################
##########################################################



2**.5

best_pred<-apply(p_boost$prob,1,which.max)
f_test$pred<-best_pred
f_test$PLAY_CALL_n<-as.numeric(f_test$PLAY_CALL)
f_test$pred_error<-f_test$PLAY_CALL_n-f_test$pred
p<-(f_test$pred_error==0)
sum(as.numeric(p))/length(f_test$FORMATION)

app_df<-as.data.frame(tfidf.matrix)



dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

clustering.kmeans <- kmeans(tfidf.matrix, 25, nstart=25) 
clustering.kmeans$size
names(clustering.kmeans$cluster[clustering.kmeans$cluster==4])

points <- cmdscale(dist.matrix, k = 2) 
master.cluster<-clustering.kmeans$cluster
plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 


plotcluster(dist.matrix, clustering.kmeans$cluster)

plot(tfidf.matrix, col = clustering.kmeans$cluster)
clustering.kmeans#
clusplot(tfidf.matrix, clustering.kmeans$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

row.names(clustering.kmeans$cluster==1)

cost_df <- data.frame()
for(i in 1:200){
  #Run kmeans for each level of i, allowing up to 100 iterations for convergence
  kmeans<- kmeans(x=tfidf.matrix, centers=i, iter.max=100)
  
  #Combine cluster number and cost together, write to df
  cost_df<- rbind(cost_df, cbind(i, kmeans$tot.withinss))
  
}
names(cost_df) <- c("cluster", "cost")

plot(cost_df$cluster, cost_df$cost)

sum(clustering.kmeans$withinss)
clustering.kmeans$tot.withinss

names(clustering.kmeans$cluster[clustering.kmeans$cluster==2])




master.cluster <- clustering.kmeans$cluster 

points <- cmdscale(dist.matrix, k = 3)
points

plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     #mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 

library(cluster)
library(fpc)

data(iris)
dat <- iris[, -5] # without known classification 
# Kmeans clustre analysis
clus <- kmeans(dat, centers=3)
# Fig 01
plotcluster(dat, clus$cluster)





clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
clustering.hierarchical$order

t[1]
length(t)
comment_df
test<-na.omit(comment_df[myvars])
length(rownames(test))

c<-colnames(comment_df)
c 
output_df<-data.frame(Line.Number=character(), Line.Text=character(),
                         Match.Text=character(), Similarity.Score=character())

for (i in 1:length(colnames(comment_df))){
  #d<-data.frame(Line.Numer=c[i], Line.Text=c[i], Match.Text=c[i], Similarity.Score=c[i])
  #output_df<-rbind(output_df, d) 
  q_df<-na.omit(comment_df[c[i]])
  q_df<-as.character(q_df[,1])
  t<-prep_fun1(q_df)
  cat(t)
  for (j in 1:round(length(t), 0)){
    sim_items<-cosine_token(t[j], t)
    tester<-as.data.frame(as.matrix(sim_items))
    final_df <- as.data.frame(t(tester))
    colnames(final_df) <- "score"
    final_df$names <-as.numeric(rownames(final_df))
    f<-subset(final_df, score>=.05)
    f<-f[order(-f$score),]
    if (length(f$score)>4){
      if (max(f$score[2])>.5){
      ln<-c()
      lt<-c()
      mt<-c()
      ss<-c()
      for (k in 1:length(f$names)){
        if (f$score[k]>=.3){
        ln[k]<-j
        lt[k]<-q_df[j]
        mt[k]<-q_df[f$names[k]]
        ss[k]<-f$score[k]
        }
      }
  
  d<-data.frame(Line.Numer=ln, Line.Text=lt, Match.Text=mt, Similarity.Score=ss)
  output_df<-rbind(output_df, d) 
      }
    }
  }
}  

write.csv(output_df, file ="/Users/strickla/Documents/LNK_Project/LNK_Comments/LNK_Comments_grouped2.csv")
      





#cat("here")
      ln[j]<-i
      lt[j]<-q_df[i]
      for (k in 1:length(f$names)){
        mt[j]<-q_df[f$names[k]]
        ss[j]<-f$score[k]
      }
    }
  }
}

output_df<-data.frame(Line.Number=ln, Line.Text=lt,
                      Match.Text=mt, Similarity.Score=ss)








retrieve_top_matches<-function(line_text, t, text_vector, p){  #input line of "prepped" text and list of "prepped" text to iterate compare to
  x<-cosine_token(line_text, t)
  tester<-as.data.frame(as.matrix(x))
  final_df <- as.data.frame(t(tester))
  colnames(final_df) <- "score"
  final_df$names <-as.numeric(rownames(final_df))
  f<-subset(final_df, score>p)
  comments<-c()
  for (i in 1:length(f$names)){
    comments[i]<-as.character(text_vector[f$names[i]])
  }
  f$text<-comments
return (f)}


x<-retrieve_top_matches(t[2], t, comment_df$Q1, .2)
x

build_final_df<-function(t, df2, blank_output){
  df_final<-blank_output
  line_number<-c()
  for (i in 1:length(t)){
    df<-retrieve_top_matches(t[i],df2)
    updated_df<-Line.Number=
}
  
}

blank_output<-data.frame(Line.Number=character(), Line.Text=character(),
                        Match.Text=character(), Similarity.Score=character())






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

search_for_exact_terms= function(master_df, term_list){    #mat,n,df){
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
    topn1<-retrieve_topn(firstmethod,master_df,comp_df$Search.Terms[i],5)
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


######################################################################
######################################################################
######################################################################














#

words <- c("one", "two", "three", "four")
probs <- c(0.1, 0.2, 0.3, 0.4)

set.seed(99)

raw <- tibble(
  word1 = sample(words, 500, TRUE, prob = probs),
  word2 = sample(words, 500, TRUE, prob = probs)
)

counts <- count(raw, word1, word2, sort = TRUE)

counts

counts %>% filter(n>=23) %>% graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(
    aes(edge_alpha = n, edge_width=n), show.legend = T) 








text<-read.csv("/Users/strickla/Desktop/LNK_Comments2.csv", header=F)
text$V1<-col_concat(text, sep="")
text2<-data.frame(Text=text$V1)
#text2<-text2[!apply(text2 == "", 1, all),]
text3<-data.frame(Text=text2)
write.csv(text3, "/Users/strickla/Desktop/LNK_Comments3.csv")
x<-as.character(text3$Text)
y<-tibble(line=1:length(text3$Text), text = x)
y

y <- y %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)
y

#word count: 
word_count<-y %>% count(word, sort=T)
y
#networks:
word_pairs <- y %>% pairwise_count(word, line, sort=T, upper=F) #get pairs of words
word_pairs

set.seed(1234)
word_pairs %>% filter(n>=3) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout="fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()
 
  
#most positive and negative words
bing_word_counts <- y %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(word, sentiment, sort = TRUE) 
bing_word_counts
    
    #build wordcloud
y %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "blue"),
                   max.words = 100)





all_lines <- y  %>% ungroup() %>% unnest_tokens(word, text) %>% mutate(linenumber = row_number())
all_lines
s <- all_lines %>% 
  inner_join(get_sentiments("afinn"), by="word") %>%  
  group_by(line) %>% 
  summarise(sentiment = sum(value)) #%>% mutate(method="afinn")
s
text3$sentiment1<-s

s <- all_lines %>% 
  inner_join(get_sentiments("bing"), by="word") %>%  
  group_by(line) %>%
  count(line, index=linenumber, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive - negative) %>% summarise(sentiment2 = sum(sentiment))
as.integer(s[1,1])

v<-c()
for (i in 1:length(text3$Text)){
  cat(i)
  cat("/")
   if (i==as.integer(s[i,1])){
    v[i]<-as.integer(s[i,2])
  }
  else{
    v[i]<-NA
  }
}
length(v)

.



















te<-c("That it would be difficult.  Yes! It met them and went beyond! I was challenged in ways that I knew I need to be, plus I was getting evaluated on instantly upon testing which provided the best feedback.",
"I think we were considered in an odd purgatory phase between OCS and TBS and I would have rather been treated either fully as a 2nd Lt or fully as an officer candidate. I also think the Marine Corps requires a level of perseverance and grit that is not necessary throughout leatherneck, and is thus untested so many individuals are not weeded out of the process.",
"The ruck/hike was far too soft. Given the massive amount of mids wanting to go Marines, this would have been a great opportunity to weed out weakness.", 
"Rifle cleaning was very common and seemed a bit tedious at times.",
"I would have liked the combative training to be longer and allow people to practice more.")
te
y<-tibble(line=1:length(te), text = te)
y

all_lines <- y  %>% ungroup() %>% unnest_tokens(word, text) %>% mutate(linenumber = row_number())
all_lines

s <- all_lines %>% 
  inner_join(get_sentiments("afinn"), by="word") #%>%  
  #group_by(line) %>% 
  #summarise(sentiment = sum(value)) %>% mutate(method="afinn")
s

s <- all_lines %>% 
  inner_join(get_sentiments("bing"), by="word") %>%  
  group_by(line) %>%
  count(line, index=linenumber, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive - negative) %>% summarise(sentiment2 = sum(sentiment))
s

#############################################################################
#############################################################################
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

d_df<-read.csv("/Users/strickla/Documents/LNK_Project/LNK_Comments/Test_Data_Set.csv", header=T)

t<-c()
for (i in 1:length(d_df$Text)){
  t[i]<-prep_fun1(d_df$Text[i])
  
}

d_df$t<-t

corpus1 = tm::Corpus(tm::VectorSource(d_df[1:45,5])) 
tdm1 <- tm::DocumentTermMatrix(corpus1) 
tdm.tfidf1 <- tm::weightTfIdf(tdm1)
tdm.tfidf1 <- tm::removeSparseTerms(tdm.tfidf1, 0.999) 
tfidf.matrix1 <- as.matrix(tdm.tfidf1)

train_df<-as.data.frame(tfidf.matrix1)
train_df$response<-d_df[1:45,3]
train_df$`-`<-NULL
train_df$scenario<-NULL
train_df$debrief<-NULL

corpus2 = tm::Corpus(tm::VectorSource(d_df[46:315,5])) 
tdm2 <- tm::DocumentTermMatrix(corpus2) 
tdm.tfidf2 <- tm::weightTfIdf(tdm2)
tdm.tfidf2 <- tm::removeSparseTerms(tdm.tfidf2, 0.999) 
tfidf.matrix2 <- as.matrix(tdm.tfidf2)

test_df<-as.data.frame(tfidf.matrix2)
#test_df$response<-d_df[46:315,3]
test_df$`-`<-NULL

#train_df<-response_df[1:45,]
#test_df<-response_df[46:315,]

boost_model<-boosting(response~., train_df, boos=T, mfinal=100)

p_boost<-predict(boost_model, train_df, type="prob")
p<-p_boost$class
p
p_boost<-predict(boost_model, test_df, type="prob", )
p<-c(p,p_boost$class)
d_df$Predicted.Class<-p
write.csv(d_df, file ="/Users/strickla/Documents/LNK_Project/LNK_Comments/Test3.csv")
2**.5
32/46
best_pred<-apply(p_boost$prob,1,which.max)
f_test$pred<-best_pred