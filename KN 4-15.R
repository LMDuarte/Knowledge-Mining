rm(list=ls()) 

library(rtweet)
library(dplyr)
library(tidyr)
install.packages("tidytext")
library(tidytext)
install.packages("textdata")
library(textdata)
library(ggplot2)
library(purrr)


twitter_token <- rtweet::create_token(
  app = "ricechicken",
  consumer_key <- "fZxHNciyOQx7JpQknh5lH23rR",
  consumer_secret <- "cDpC4gutEQqkTFXP813r8XnoNIEvWIpj8C1UPd4PRG9bhD00el",
  access_token_key <- "1282900852823523328-ISVIHvnKk0nfp1oD65375tu0osCXVv",
  access_secret <- "445Yt7i2TU4tBFsiDpkxoz44Pxy2yW1ksla9t1aNT1mBZ")



#Sentiment analysis////////////////////////////////////////////////////////////////////////////////////////////////////////
#Sentiment analysis////////////////////////////////////////////////////////////////////////////////////////////////////////
#Sentiment analysis////////////////////////////////////////////////////////////////////////////////////////////////////////

MC <- get_timelines(c("LeaderMcConnell"), n = 100)
AOC <- get_timelines(c("AOC"), n = 100)
Hank <- get_timelines(c("hankgreen"), n = 100)



t.MC =MC%>% select(screen_name,text)
t.AOC =AOC%>% select(screen_name,text)
t.AOC

#Cleaning tweets///////////////////////////MC
t.MC$stripped_text1<-gsub("http\\S+","",t.MC$text)
#
t.MC_stem<-t.MC %>% select(stripped_text1) %>% unnest_tokens(word,stripped_text1)

head(t.MC_stem)
#
cleanned_tweets.MC<-t.MC_stem%>%anti_join(stop_words)

head(cleanned_tweets.MC)
head(t.MC$text)

#Cleaning tweets///////////////////////////AOC
t.AOC$stripped_text1<-gsub("http\\S+","",t.AOC$text)
#
t.AOC_stem<-t.AOC %>% select(stripped_text1) %>% unnest_tokens(word,stripped_text1)

head(t.AOC_stem)
#
cleanned_tweets.AOC<-t.AOC_stem%>%anti_join(stop_words)

head(cleanned_tweets.AOC)
head(t.AOC$text)

#top words/////////////////////////////////////////////////////////////////
cleanned_tweets.MC %>%
  count(word, sort= TRUE)%>%
  top_n(10) %>%
  mutate(word =reorder(word, n))%>%
  ggplot(aes(x = word, y= n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  theme_classic()+
labs(x ="Count",
     y = "unique words",
     title = "word count McConnell")
#////////
cleanned_tweets.AOC %>%
  count(word, sort= TRUE)%>%
  top_n(10) %>%
  mutate(word =reorder(word, n))%>%
  ggplot(aes(x = word, y= n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()+
  theme_classic()+
  labs(x ="Count",
       y = "unique words",
       title = "word count AOC")

#Sentiment analysis//////////////////////////////////

get_sentiments("bing") %>% filter(sentiment=="positive")

get_sentiments("bing") %>% filter(sentiment=="negative")


get_sentiments("afinn") %>% filter(value=="3")

get_sentiments("afinn") %>% filter(value=="-3")

#///////////////////////////////////////////////////////
#bing sentiment analysis MC
bing_MC = cleanned_tweets.MC %>%
  inner_join(get_sentiments("bing"))%>%
  count(word, sentiment, sort= TRUE)%>%
  ungroup()
bing_MC
#graph positive and negative words in tweets///////////////////////////////////////////MC/////////////
bing_MC%>%
  group_by(sentiment)%>%
  top_n(10)%>%
  ungroup%>%
  mutate(word = reorder(word,n))%>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment, scales="free_y") +
  labs(title = "Tweets McConnell", y =" sentiment", x= NULL)+ coord_flip()+ theme_bw()


#bing sentiment analysis AOC
bing_AOC = cleanned_tweets.AOC %>%
  inner_join(get_sentiments("bing"))%>%
  count(word, sentiment, sort= TRUE)%>%
  ungroup()
bing_AOC
#graph positive and negative words in tweets//////////////////////////////////////////AOC/////////////

bing_AOC%>%
  group_by(sentiment)%>%
  top_n(10)%>%
  ungroup%>%
  mutate(word = reorder(word,n))%>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment, scales="free_y") +
  labs(title = "Tweets AOC", y =" sentiment", x= NULL)+ coord_flip()+ theme_bw()

#FUNCTION////////////////////////////////////////////////////////////////////////////////////////////////////
sentiment_bing = function(twt){
  twt_tbl= tibble(text = twt)%>%
    mutate( stripped_text = gsub("http\\S+","",text))%>%
    unnest_tokens(word,stripped_text)%>%
    anti_join(stop_words)%>%
    inner_join(get_sentiments("bing"))%>%
    count(word,sentiment, sort =TRUE)%>%
    ungroup()%>%
    
    mutate(
      score= case_when(
        sentiment =='negative'~n*(-1),
        sentiment =='positive'~n* 1)
      ) 
  
    sent.score = case_when(
      nrow(twt_tbl)==0~0,
      nrow(twt_tbl)>0~sum(twt_tbl$score)
    )
    
    zero.type = case_when(
      nrow(twt_tbl) == 0~"Type 1:",
      nrow(twt_tbl)>0~"Type2"
    )
   list(score = sent.score, type= zero.type, twt_tbl = twt_tbl)
}
########################
MC_sent = lapply(MC$text, function(x){sentiment_bing(x)})
MC_sent
  
AOC_sent = lapply(AOC$text, function(x){sentiment_bing(x)})
AOC_sent  

Hank_sent= lapply(Hank$text, function(x){sentiment_bing(x)})
########################

sentiment= bind_rows(
  tibble(
    Account = 'MC',
    score = unlist(map (MC_sent,'score')),
    type= unlist(map (MC_sent,'type'))
  ),
  tibble(
    Account = 'AOC',
    score = unlist(map (AOC_sent,'score')),
    type= unlist(map (AOC_sent,'type'))
  )
)
  
sentiment  

#graph/////////Should show over all positivity or negativity on tweeter accounts.
#The librarty has assigned negative and positive scores to individual words
#AOC appears to be more neutral than mitch mc connel, but AOC its not positive either
ggplot(sentiment, aes(x =score, fill = Account))+ geom_histogram(bins = 15, alpha =.6)+
  facet_grid(~Account) + theme_bw()



#testing graph with a positive non political entertainment twitter account
sentiment_test= bind_rows(
  tibble(
    Account = 'MC',
    score = unlist(map (MC_sent,'score')),
    type= unlist(map (MC_sent,'type'))
  ),
  tibble(
    Account = 'Hank',
    score = unlist(map (Hank_sent,'score')),
    type= unlist(map (Hank_sent,'type'))
  )
)
  
#graph/////////
ggplot(sentiment_test, aes(x =score, fill = Account))+ geom_histogram(bins = 15, alpha =.6)+
  facet_grid(~Account) + theme_bw()






















