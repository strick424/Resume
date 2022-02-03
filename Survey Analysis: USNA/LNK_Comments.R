library(readtext)
library(magrittr)
library(assertr)
library(widyr)
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
library(MASS)
library(data.table)
library(splitstackshape)
library(tidyr)
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
library(textdata)
library(stringr)
library(leaps)

comment_df<-read.csv("LNK_Comments_final.csv", header=T)

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
                 point.padding = unit(.25, "lines"))

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
test_df<-read.csv("LNK_Comments_Themes2.csv", header=T)
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
write.csv(text_all, file ="Test_first_iter.csv")

##########################################################
##########################################################
##########################################################

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

build_final_df<-function(t, df2, blank_output){
  df_final<-blank_output
  line_number<-c()
  for (i in 1:length(t)){
    df<-retrieve_top_matches(t[i],df2)
    updated_df<-Line.Number=
} 
}
