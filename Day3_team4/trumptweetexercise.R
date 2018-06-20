load(url("https://cbail.github.io/Trump_Tweets.Rdata"))
install.packages('tidytext')
install.packages('dplyr')
library(tidytext)
library(dplyr)
tidy_trump_tweets<- trumptweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text)

data("stop_words")

trump_tweet_top_words<-
  tidy_trump_tweets %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))


trump_tweet_top_words<-
  trump_tweet_top_words[-grep("https|t.co|amp|rt",
                              trump_tweet_top_words$word),]

head(tidy_trump_tweets)

tidy_trump_tfidf<- trumptweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text) %>%
  anti_join(stop_words) %>%
  count(word, created_at) %>%
  bind_tf_idf(word, created_at, n)

trumptweets %>% select(created_at,text) 

top_tfidf<-tidy_trump_tfidf %>%
  arrange(desc(tf_idf))

top_tfidf$word[1]
head(top_tfidf$word)