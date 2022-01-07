**Overview**

Leatherneck is an annual training activity at USNA in which participants perform a series of training events for evaluation. At the end of the training, a survey is conducted in which participants can provide feedback to the trainers. This project showcases a series of Natural Language Processing techniques to analyze the survey responses.

**Features** 

Using R, we create an n-gram network overlayed with sentiment scores (positive, neutral, and negative) to identify key responses in the survey. The network enables us to better understand the frequency and sentiment of specific bigrams in the survey responses. We use similarity comparison (cosine similarity) to group comments and find trends among the responses (unsupervised learning). We are able to identify 11 common themes throughout the survey - highlighting actionable concerns brought forth from the participants. Finally, we use machine learning to train a model on the 11 common themes, and predict categories for responses not yet identified. 

**Impact**

The results of this survey played a critical role in the planning of furture iterations of USNA summer training. Specifically, more exposure to specific activities such as physical fitness, aviation, and peer leadership were redesigned in the subsequent years: 2020 and 2021. Additionally, the results were presented to the USNA Data Science seminar where multiple organization began requesting help for analyzing their own surveys - spawning a new era of text mining at USNA. 

**File Overview**

1. *Analysis_of_Leatherneck_Comments.pdf*: final report submitted to senior Marine Corps leadership at USNA.
2. *LNK_Comments.R*: R code used to evaluate and analyze survey responses. 
3. *LNK_Comments_Sample.csv*: a sample data set of survey responses.
4. *Text_Analysis.Rmd*: a 'How-To' R markdown file provided as to help guide USNA analyst who want to learn more about text mining. 
5. *Text_Processing.pdf*: presentation given to the USNA Data Science Seminar on December 5, 2019. 
