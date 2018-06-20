setwd("C:/Users/Joanna/Desktop/SICSS")
# Check the working directory was changed

##Load data
load(url("https://cbail.github.io/Trump_Tweets.Rdata"))
trump_approval<-read.csv("https://projects.fivethirtyeight.com/trump-approval-data/approval_topline.csv")

library(tidytext)
library(dplyr)
library(ggplot2)

#TidyText
tidy_trump_tweets<- trumptweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text)

tidy_trump_tweets <- trumptweets

#Remove stop-words
data("stop_words")
tidy_trump_tweets<-tidy_trump_tweets %>%
  anti_join(stop_words, by=c("text" = "word"))

#Remove non-words


tidy_trump_tweets<-
  tidy_trump_tweets[-grep("https|t.co|amp|rt",
                          tidy_trump_tweets$word),]


#Remove numbers
tidy_trump_tweets<-tidy_trump_tweets[-grep("\\b\\d+\\b", tidy_trump_tweets$word),]

# Removing whitespaces
tidy_trump_tweets$word <- gsub("\\s+","",tidy_trump_tweets$word)

# Stemming
library(SnowballC)
tidy_trump_tweets<-tidy_trump_tweets %>%
  mutate_at("word", funs(wordStem((.), language="en")))

#Document-Term Matrix
tidy_trump_DTM<-
  tidy_trump_tweets %>%
  count(created_at, word) %>%
  cast_dtm(created_at, word, n)

#topicmodels
library(topicmodels)
DT_topic_model<-LDA(tidy_trump_DTM, k=10, control = list(seed = 321))


### Visualize
library(tidytext)
library(dplyr)
library(ggplot2)

DT_topics <- tidy(DT_topic_model, matrix = "beta")

dt_top_terms <- 
  DT_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


dt_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


