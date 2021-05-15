
rm(list=ls()) 
#install.packages("textdata")
#install.packages("tidytext") 

library(rtweet)
library(dplyr)
library(tidyr)
library(tidytext) 
library(textdata)
library(ggplot2)
library(purrr)
setwd("/Users/magal/Desktop/knowledge mining/project/R code")

twitter_token <- rtweet::create_token(
  app = "ricechicken",
  consumer_key <- "fZxHNciyOQx7JpQknh5lH23rR",
  consumer_secret <- "cDpC4gutEQqkTFXP813r8XnoNIEvWIpj8C1UPd4PRG9bhD00el",
  access_token_key <- "1282900852823523328-ISVIHvnKk0nfp1oD65375tu0osCXVv",
  access_secret <- "445Yt7i2TU4tBFsiDpkxoz44Pxy2yW1ksla9t1aNT1mBZ")

load("TW.Rdata")

#save(list = c("TC", "MR", "MTG","SRP","AOC", "BRN", "JB", "KH", "IO", "VP","PT","AOC_People","Bernie_People",
             # "Ilhan_People","RP_People","DM_People","SRP_People3","MTG_People","MR_People3","MR_People",
             # "Ted_People","Kamala_People","Joe_People"), file = "TW.Rdata")

  #Politicians//////////////////////////////////////////
#Republicans
#TC <- get_timelines(c("tedcruz"), n = 2500)
#MR <- get_timelines(c("marcorubio"), n = 600)
#MTG<- get_timelines(c("mtgreenee"), n = 2000)
#SRP<- get_timelines(c("RandPaul"), n = 500)

#Democrats
#AOC <- get_timelines(c("AOC"), n = 1400)
#BRN<- get_timelines(c("SenSanders"), n = 400)
#JB<- get_timelines(c("JoeBiden"), n = 900)
#KH<- get_timelines(c("KamalaHarris"), n = 900)
#IO<- get_timelines(c("IlhanMN"), n = 2100)

#Presidential
#PT<- get_timelines(c("POTUS"), n = 1000)
#VP<- get_timelines(c("VP"), n = 400)

#merged data sets by party
DM <- rbind(AOC, BRN,JB,KH,IO)
RP<- rbind(TC,MR,MTG,SRP)

tweets_day<-function(username){
  TW=username
  
  Account= username$screen_name
  #by<- 'day'
  
  
  MCB =TW
  MCB$text<-gsub("http\\S+"," ",TW$text)
  
  sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
  sentiment$word<-gsub("trump"," ",sentiment$word)
  
  sentiment_dataset <- get_sentiments("afinn")
  sentiment_dataset <- arrange(sentiment_dataset, -value)
  sentiment <- merge(sentiment, sentiment_dataset, by = 'word')
  sentiment$day<- substr(sentiment$created_at,1,10)
  sentiment$hour<- substr(sentiment$created_at,11,18)
  sentiment$month<- substr(sentiment$created_at,1,7)
  sentiment$word <- NULL
  sentiment$screen_name <- NULL
  

  #plot day
  pivot <- sentiment %>%
    group_by(month) %>%
    summarise(sentiment = mean(value))
  
  p<-ggplot(pivot[-1,], aes(x = month, y = sentiment)) + geom_line(group = 1) +
    geom_point() + theme_minimal() +
    labs(title = paste0('Average sentiment of tweets by @"',Account,'"'),
         subtitle = paste0(pivot$month[2],' - ',pivot$month[nrow(pivot)],' on ', format(sentiment$created_at[1], '%d %B %Y')),
         x = 'Date', y = 'Sentiment', caption = 'Source: Twitter API')+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(p)
}



tweets_day_Party<-function(username){
  TW=username
  
  #Account= username$screen_name
  #by<- 'day'
  
  
  MCB =TW
  MCB$text<-gsub("http\\S+"," ",TW$text)
  
  sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
  sentiment$word<-gsub("trump"," ",sentiment$word)
  
  sentiment_dataset <- get_sentiments("afinn")
  sentiment_dataset <- arrange(sentiment_dataset, -value)
  sentiment <- merge(sentiment, sentiment_dataset, by = 'word')
  sentiment$day<- substr(sentiment$created_at,1,10)
  sentiment$hour<- substr(sentiment$created_at,11,18)
  sentiment$month<- substr(sentiment$created_at,1,7)
  sentiment$word <- NULL
  sentiment$screen_name <- NULL
  
  
  #plot month
  
  pivot <- sentiment %>%
    group_by(month) %>%
    summarise(sentiment = mean(value))
  
  p <-ggplot(pivot[-1,], aes(x = month, y = sentiment)) + geom_line(group = 1) +
    geom_point() + theme_minimal() +
    labs(title = paste0('Average sentiment of tweets '),
         subtitle = paste0(pivot$month[2],' - ',pivot$month[nrow(pivot)],' on ', format(sentiment$created_at[1], '%d %B %Y')),
         x = 'Date', y = 'Sentiment', caption = 'Source: Twitter API')+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  return(p)
}


#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////


tweets_sentiment_words<-function(username){
  
MC2=username
Account= username$screen_name

MC2$st1<-gsub("http\\S+"," ",MC2$text)
MC3<-MC2 %>% select(st1) %>% unnest_tokens(word,st1)
MC3$word<-gsub("trump"," ",MC3$word)


cleanned_tweets.MC<-MC3%>%anti_join(stop_words)




bing_MC = cleanned_tweets.MC %>%
  inner_join(get_sentiments("bing"))%>%
  count(word, sentiment, sort= TRUE)%>%
  ungroup()


bing_MC$percent<- 0
bing_MC$percent<- bing_MC$n/ (sum(bing_MC$n))

a<-bing_MC%>%
  group_by(sentiment)%>%
  top_n(10)%>%
  ungroup%>%
  mutate(word = reorder(word,n))

b<-ggplot(data=a,aes(word, percent, fill = sentiment))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment, scales="free_y") +  scale_y_continuous(labels=scales::percent) +
  labs(title = paste0('Top ten Positive and Negative tweets by @ "',Account,'"'), 
       y =" sentiment", x= NULL)+ coord_flip()+ theme_bw()


return(b)
}


#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////

#////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////

tweets_score_pt<-function(username){
  
  MC2= username
  Account= username$screen_name
  
  MC2$st1<-gsub("http\\S+"," ",MC2$text)
  MC3<-MC2 %>% select(st1) %>% unnest_tokens(word,st1)
  MC3$word<-gsub("trump"," ",MC3$word)
  
  
  cleanned_tweets.MC<-MC3%>%anti_join(stop_words)
  
  
  
  afinn_cm= cleanned_tweets.MC %>%
    inner_join(get_sentiments("afinn"))
  
  afinn_plot <- afinn_cm %>%
    group_by(value) %>%
    summarise(word_count = n()) %>%
    ungroup() %>%
    mutate(value = reorder(value, word_count)) 
  
  
  afinn_plot$value <- factor(afinn_plot$value,levels = c("-5", "-4", "-3", "-2", "-1", 
                                                         "1", "2", "3","4","5"))
  
  afinn_plot$percent<- 0
  
  afinn_plot$percent<- afinn_plot$word_count/ (sum(afinn_plot$word_count))
  
  b = ggplot(data = afinn_plot,aes(x=value, y= percent, fill= value )) +
    geom_bar(stat="identity", color= "black")+ 
    #scale_y_continuous(labels=scales::percent) +
    ylim(0, .30)+
    labs(title=paste0('Sentiment of tweets by "',Account,'"'), 
         x="Sentiment score", y = "Percentage")+ scale_fill_brewer(palette="RdBu")
  
  
  
  # scale_fill_manual(values=c("indianred4", 
  #   "indianred", 
  #  "indianred3", 
  #  "indianred1",
  #  "slategray2",
  # "slategray3", 
  # "slategray4", 
  # "lightslategray", 
  #  "grey30",
  #  "darkseagreen4"))
  
  b
  
  return(b)
}
#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////

tweets_score<-function(username){

MC2= username
Account= username$screen_name

MC2$st1<-gsub("http\\S+"," ",MC2$text)
MC3<-MC2 %>% select(st1) %>% unnest_tokens(word,st1)
MC3$word<-gsub("trump"," ",MC3$word)


cleanned_tweets.MC<-MC3%>%anti_join(stop_words)



afinn_cm= cleanned_tweets.MC %>%
  inner_join(get_sentiments("afinn"))

afinn_plot <- afinn_cm %>%
  group_by(value) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(value = reorder(value, word_count)) 


afinn_plot$value <- factor(afinn_plot$value,levels = c("-5", "-4", "-3", "-2", "-1", 
                                                       "1", "2", "3","4","5"))

afinn_plot$percent<- 0

afinn_plot$percent<- afinn_plot$word_count/ (sum(afinn_plot$word_count))

b = ggplot(data = afinn_plot,aes(x=value, y= percent, fill= value )) +
  geom_bar(stat="identity", color= "black")+ 
  #scale_y_continuous(labels=scales::percent) +
  ylim(0, .40)+
  labs(title=paste0('Sentiment of tweets by "',Account,'"'), 
       x="Sentiment score", y = "Percentage")+ scale_fill_brewer(palette="RdBu")
  
  
  
 # scale_fill_manual(values=c("indianred4", 
                          #   "indianred", 
                           #  "indianred3", 
                           #  "indianred1",
                           #  "slategray2",
                            # "slategray3", 
                            # "slategray4", 
                            # "lightslategray", 
                           #  "grey30",
                           #  "darkseagreen4"))

b

return(b)
}
tweets_score(Ted_People)

tweets_score

##########################################
##########################################
tweets_score_p<-function(username){
  
  MC2= username
  Account= username$screen_name
  
  MC2$st1<-gsub("http\\S+"," ",MC2$text)
  MC3<-MC2 %>% select(st1) %>% unnest_tokens(word,st1)
  MC3$word<-gsub("trump"," ",MC3$word)
  
  
  cleanned_tweets.MC<-MC3%>%anti_join(stop_words)
  
  
  
  afinn_cm= cleanned_tweets.MC %>%
    inner_join(get_sentiments("afinn"))
  
  afinn_plot <- afinn_cm %>%
    group_by(value) %>%
    summarise(word_count = n()) %>%
    ungroup() %>%
    mutate(value = reorder(value, word_count)) 
  
  
  afinn_plot$value <- factor(afinn_plot$value,levels = c("-5", "-4", "-3", "-2", "-1", 
                                                         "1", "2", "3","4","5"))
  
  afinn_plot$percent<- 0
  
  afinn_plot$percent<- afinn_plot$word_count/ (sum(afinn_plot$word_count))
  
  b = ggplot(data = afinn_plot,aes(x=value, y= percent, fill= value )) +
    geom_bar(stat="identity", color= "black")+ 
    scale_y_continuous(labels=scales::percent) +
    #ylim(0, .40)+
    labs(title=paste0('Sentiment of tweets by "',Account,'"'), 
         x="Sentiment score", y = "Percentage")+ scale_fill_brewer(palette="RdBu")
  
  
  
  # scale_fill_manual(values=c("indianred4", 
  #   "indianred", 
  #  "indianred3", 
  #  "indianred1",
  #  "slategray2",
  # "slategray3", 
  # "slategray4", 
  # "lightslategray", 
  #  "grey30",
  #  "darkseagreen4"))
  
  b
  
  return(b)
}
##########################################
##########################################
##########################################
##########################################
#Sentiment toward politicians




#AOC_People<- rtweet::search_tweets(q = "AOC", n = 5000, lang = "en", token = twitter_token)

####//////////////////////////////

#Bernie_People<- rtweet::search_tweets(q = "Bernie Sanders", n = 5000, lang = "en", token = twitter_token)

####/////////////////////////////

#Ilhan_People<- rtweet::search_tweets(q = "Ilhan Omar", n = 5000, lang = "en", token = twitter_token)

####////////////////////////////

#Joe_People<- rtweet::search_tweets(q = "Joe Biden", n = 5000, lang = "en", token = twitter_token)

####////////////////////////////
#Kamala_People<- rtweet::search_tweets(q = "Kamala Harris", n = 5000, lang = "en", token = twitter_token)


#Republicans///////////////////////////////////////////////////////////////////////////////////////////
#Ted_People<- rtweet::search_tweets(q = "Ted Cruz", n = 5000, lang = "en", token = twitter_token)



####////////////////////////////
#MR_People3<- rtweet::search_tweets(q = "Marco Rubio", n = 5000, lang = "en", token = twitter_token)


####////////////////////////////
#MTG_People<- rtweet::search_tweets(q = "Marjorie Taylor Greene", n = 5000, lang = "en", token = twitter_token)


####////////////////////////////
#SRP_People3<- rtweet::search_tweets(q = "Rand Paul", n = 5000, lang = "en", token = twitter_token)



#Political Parties///////////////////////////////////////////////////////////////////////////////////////////


####////////////////////////////
#RP_People<- rtweet::search_tweets(q = "Republicans", n = 5000, lang = "en", token = twitter_token)



#save(list = c("TC", "MR", "MTG","SRP","AOC", "BRN", "JB", "KH", "IO", "VP","PT","AOC_People","Bernie_People",
              #"Ilhan_People","RP_People","DM_People","SRP_People","MTG_People","MR_People","MR_People",
              #"Ted_People","Kamala_People","Joe_People"), file = "TW.Rdata")

#Politicians//////////////////////////////////////////
#Republicans
#TC <- get_timelines(c("tedcruz"), n = 2500)
#MR <- get_timelines(c("marcorubio"), n = 600)
#MTG<- get_timelines(c("mtgreenee"), n = 2000)
#SRP<- get_timelines(c("RandPaul"), n = 500)

#Democrats
#AOC <- get_timelines(c("AOC"), n = 1400)
#BRN<- get_timelines(c("SenSanders"), n = 400)
#JB<- get_timelines(c("JoeBiden"), n = 900)
#KH<- get_timelines(c("KamalaHarris"), n = 900)
#IO<- get_timelines(c("IlhanMN"), n = 2100)

#Presidential
#PT<- get_timelines(c("POTUS"), n = 1000)
#VP<- get_timelines(c("VP"), n = 400)

#merged data sets by party
#DM <- rbind(AOC, BRN,JB,KH,IO)
#RP<- rbind(TC,MR,MTG,SRP)




#AOC sentiment combination public and timeline1111111111111111111111111111111111111111111

TW=AOC

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PAOC <- sentiment 


PAOC$SP= PAOC$value
PAOC$value <- NULL
PAOC$created_at <- NULL
PAOC$screen_name <- NULL
PAOC$Politician = 1

TW=AOC_People

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PAOC2 <- sentiment 

PAOC2$SPP= PAOC2$value
PAOC2$value <- NULL
PAOC2$created_at <- NULL
PAOC2$screen_name <- NULL

PAOC2$Politician = 1

PAOC3=PAOC2[sample(nrow(PAOC2), size = 3229, replace = FALSE),]
PAOC$SPP=0
PAOC$SPP=PAOC3$SPP

summary(PAOC$SPP)
summary(PAOC$SP)




#TED CRUZ sentiment merging2222222222222222222222222222222222222222222222222222222222
TW=TC

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PTC <- sentiment 


PTC$SP= PTC$value
PTC$value <- NULL
PTC$created_at <- NULL
PTC$screen_name <- NULL
PTC$Politician = 2

TW=Ted_People

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PTC2 <- sentiment 

PTC2$SPP= PTC2$value
PTC2$value <- NULL
PTC2$created_at <- NULL
PTC2$screen_name <- NULL

PTC2$Politician = 2

PTC3=PTC2[sample(nrow(PTC2), size = 4268, replace = FALSE),]
PTC$SPP=0
PTC$SPP=PTC3$SPP

summary(PTC$SPP)
summary(PTC$SP)

#marcorubio merging 3333333333333333333333333333333333333333333333333333333333
TW=MR

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PMR <- sentiment 


PMR$SP= PMR$value
PMR$value <- NULL
PMR$created_at <- NULL
PMR$screen_name <- NULL
PMR$Politician = 3

TW=MR_People3

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PMR2 <- sentiment 

PMR2$SPP= PMR2$value
PMR2$value <- NULL
PMR2$created_at <- NULL
PMR2$screen_name <- NULL

PMR2$Politician = 3

PMR3=PMR2[sample(nrow(PMR2), size = 1202, replace = FALSE),]
PMR$SPP=0
PMR$SPP=PMR3$SPP

summary(PMR2$SPP)
summary(PMR$SP)
#mtgreenee 4444444444444444444444444444444444444444444444444444444444444444444444444444
TW=MTG

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PMTG <- sentiment 


PMTG$SP= PMTG$value
PMTG$value <- NULL
PMTG$created_at <- NULL
PMTG$screen_name <- NULL
PMTG$Politician = 4

TW=MTG_People

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PMTG2 <- sentiment 

PMTG2$SPP= PMTG2$value
PMTG2$value <- NULL
PMTG2$created_at <- NULL
PMTG2$screen_name <- NULL

PMTG2$Politician = 4

PMTG3=PMTG2[sample(nrow(PMTG2), size = 4408, replace = FALSE),]
PMTG$SPP=0
PMTG$SPP=PMTG3$SPP

summary(PMTG$SPP)
summary(PMTG$SP)


#SRP 55555555555555555555555555555555555555555555555555555555555555555555555555
TW=SRP

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PSRP <- sentiment 


PSRP$SP= PSRP$value
PSRP$value <- NULL
PSRP$created_at <- NULL
PSRP$screen_name <- NULL
PSRP$Politician = 5

TW=SRP_People3

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PSRP2 <- sentiment 

PSRP2$SPP= PSRP2$value
PSRP2$value <- NULL
PSRP2$created_at <- NULL
PSRP2$screen_name <- NULL

PSRP2$Politician = 5

PSRP3=PSRP2[sample(nrow(PSRP2), size = 1239, replace = FALSE),]
PSRP$SPP=0
PSRP$SPP=PSRP3$SPP

summary(PSRP$SPP)
summary(PSRP$SP)




#Bernie666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666
TW=BRN

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PBRN <- sentiment 


PBRN$SP= PBRN$value
PBRN$value <- NULL
PBRN$created_at <- NULL
PBRN$screen_name <- NULL
PBRN$Politician = 6

TW=Bernie_People

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PBRN2 <- sentiment 

PBRN2$SPP= PBRN2$value
PBRN2$value <- NULL
PBRN2$created_at <- NULL
PBRN2$screen_name <- NULL

PBRN2$Politician = 6

PBRN3=PBRN2[sample(nrow(PBRN2), size = 1174, replace = FALSE),]
PBRN$SPP=0
PBRN$SPP=PBRN3$SPP

summary(PBRN$SPP)
summary(PBRN$SP)


#Ilhan 7777777777777777777777777777777777777777777777777777777777777777777777777777777
TW=IO

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PIO <- sentiment 


PIO$SP= PIO$value
PIO$value <- NULL
PIO$created_at <- NULL
PIO$screen_name <- NULL
PIO$Politician = 7

TW=Ilhan_People

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PIO2 <- sentiment 

PIO2$SPP= PIO2$value
PIO2$value <- NULL
PIO2$created_at <- NULL
PIO2$screen_name <- NULL

PIO2$Politician = 7

PIO3=PIO2[sample(nrow(PIO2), size = 4709, replace = FALSE),]
PIO$SPP=0
PIO$SPP=PIO3$SPP

summary(PIO$SPP)
summary(PIO$SP)


#KH88888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
TW=KH

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PKH <- sentiment 


PKH$SP= PKH$value
PKH$value <- NULL
PKH$created_at <- NULL
PKH$screen_name <- NULL
PKH$Politician = 8

TW=Kamala_People

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PKH2 <- sentiment 

PKH2$SPP= PKH2$value
PKH2$value <- NULL
PKH2$created_at <- NULL
PKH2$screen_name <- NULL

PKH2$Politician = 8

PKH3=PKH2[sample(nrow(PKH2), size = 2106, replace = FALSE),]
PKH$SPP=0
PKH$SPP=PKH3$SPP

summary(PKH$SPP)
summary(PKH$SP)


#JB9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
TW=JB

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PJB <- sentiment 


PJB$SP= PJB$value
PJB$value <- NULL
PJB$created_at <- NULL
PJB$screen_name <- NULL
PJB$Politician = 9

TW=Joe_People

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PJB2 <- sentiment 

PJB2$SPP= PJB2$value
PJB2$value <- NULL
PJB2$created_at <- NULL
PJB2$screen_name <- NULL

PJB2$Politician = 9

PJB3=PJB2[sample(nrow(PJB2), size = 1955, replace = FALSE),]
PJB$SPP=0
PJB$SPP=PJB3$SPP

summary(PJB$SPP)
summary(PJB$SP)
#POTUS10101010101010101010101010101010
TW=PT

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PPT <- sentiment 


PPT$SP= PPT$value
PPT$value <- NULL
PPT$created_at <- NULL
PPT$screen_name <- NULL
PPT$Politician = 10

TW=Joe_People

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PPT2 <- sentiment 

PPT2$SPP= PPT2$value
PPT2$value <- NULL
PPT2$created_at <- NULL
PPT2$screen_name <- NULL

PPT2$Politician = 10

summary(PPT2$SPP)
summary(PPT$SP)


#Republicans////////////////////////////////////////////////////////////////////////////////
TW=RP

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PRP <- sentiment 


PRP$SP= PRP$value
PRP$value <- NULL
PRP$created_at <- NULL
PRP$screen_name <- NULL
PRP$Politician = 10

TW=RP_People

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PRP2 <- sentiment 

PRP2$SPP= PRP2$value
PRP2$value <- NULL
PRP2$created_at <- NULL
PRP2$screen_name <- NULL

PRP2$Politician = 10

summary(PRP2$SPP)
summary(PRP$SP)



#Democrats#########################################################################
TW=DM

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PDM <- sentiment 


PDM$SP= PDM$value
PDM$value <- NULL
PDM$created_at <- NULL
PDM$screen_name <- NULL
PDM$Politician = 10

TW=DM_People

MCB =TW
MCB$text<-gsub("http\\S+"," ",TW$text)

sentiment <- MCB[,3:5] %>% unnest_tokens(output = 'word', input = 'text')
sentiment$word<-gsub("trump"," ",sentiment$word)

sentiment_dataset <- get_sentiments("afinn")
sentiment_dataset <- arrange(sentiment_dataset, -value)
sentiment <- merge(sentiment, sentiment_dataset, by = 'word')

PDM2 <- sentiment 

PDM2$SPP= PDM2$value
PDM2$value <- NULL
PDM2$created_at <- NULL
PDM2$screen_name <- NULL

PDM2$Politician = 10

summary(PDM2$SPP)
summary(PDM$SP)
#////////////////////////////////////////////////////////////////////////////////
Data<-rbind(PAOC,PJB,PBRN,PIO,PKH,PTC,PMR,PSRP,PMTG)
Data$word <- NULL


library(stargazer)


fit.1 = lm(SPP~ SP, data = Data)
stargazer(fit.1, type="text", title= "original fit", out= "table1.txt")
library(coefplot)


coef<-fit.1$coefficients
coef
coefplot(fit.1, intercept=FALSE)

plot(Data$SP,Data$SPP)


Data2<- aggregate(.~Politician, Data, mean)

fit.1 = lm(SPP~ SP, data = Data2)
stargazer(fit.1, type="text", title= "original fit", out= "table1.txt")

coef<-fit.1$coefficients
coef
coefplot(fit.1, intercept=FALSE)
plot(Data2$SP,Data2$SPP,
     xlim=c(-0.5,0.75),
     ylim= c(-2.0,0.1))




#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#"AOC"
tweets_sentiment_words(AOC_People)
tweets_sentiment_words(AOC)

tweets_score(AOC_People)
tweets_score(AOC)

summary(PAOC2$SPP)
summary(PAOC$SP)
####//////////////////////////////
#"Bernie Sanders"
tweets_sentiment_words(Bernie_People)
tweets_sentiment_words(BRN)

tweets_score(Bernie_People)
tweets_score(BRN)

summary(PBRN2$SPP)
summary(PBRN$SP)
####/////////////////////////////
#"Ilhan Omar"


tweets_sentiment_words(Ilhan_People)
tweets_sentiment_words(IO)

tweets_score(Ilhan_People)
tweets_score(IO)

summary(PIO2$SPP)
summary(PIO$SP)
####////////////////////////////
#"Joe Biden"

tweets_sentiment_words(Joe_People)
tweets_sentiment_words(JB)
tweets_sentiment_words(PT)


tweets_score(JB)
tweets_score(Joe_People)
tweets_score(PT)


summary(PJB2$SPP)
summary(PJB$SP)


tweets_day(PT)

####////////////////////////////
#"Kamala Harris"
tweets_score(Kamala_People)
tweets_score(KH)


tweets_sentiment_words(Kamala_People)
tweets_sentiment_words(KH)

tweets_score(Kamala_People)
tweets_score(IO)

summary(PKH2$SPP)
summary(PKH$SP)

#Republicans///////////////////////////////////////////////////////////////////////////////////////////
#"Ted Cruz"
tweets_score(Ted_People)
tweets_score(TC)
tweets_sentiment_words(Ted_People)
tweets_day(TC)


####////////////////////////////
# "Marco Rubio"
tweets_score(MR_People)
tweets_score_p(MR_People3)
tweets_score(MR)


tweets_sentiment_words(MR)
tweets_sentiment_words(MR_People)
tweets_sentiment_words(MR_People3)

summary(PMR2$SPP)
summary(PMR$SP)

####////////////////////////////
# "Marjorie Taylor Greene"
tweets_score(MTG_People)
tweets_score(MTG)

tweets_sentiment_words(MTG_People)
tweets_sentiment_words(MTG)


summary(PMTG2$SPP)
summary(PMTG$SP)
####////////////////////////////
#"Rand Paul"
tweets_score_p(SRP_People3)
tweets_score_p(SRP)

tweets_sentiment_words(SRP_People3)
tweets_sentiment_words(SRP)

summary(PSRP2$SPP)
summary(PSRP$SP)
#Political Parties///////////////////////////////////////////////////////////////////////////////////////////
#"Democrats"
tweets_score_pt(DM)
tweets_score_pt(DM_People)

tweets_sentiment_words(DM_People)
tweets_sentiment_words(DM)



####////////////////////////////
# "Republicans"
tweets_score_pt(RP_People)
tweets_score_pt(RP)
tweets_sentiment_words(RP_People)

tweets_sentiment_words(RP_People)

tweets_day(RP)



#Sentiment over time

#/////////////////////////////// 11-12m
#2020 10 06 2500 4.4m
tweets_day(TC)
#2020-10-19  600 4.2m
tweets_day(MR)
#2020-12-24 2000 401.4k
tweets_day(MTG)
#2020-10-09 500 3.1m
tweets_day(SRP)

#///////////////////////////////  73.2m
#2020-10-12 1400 12.7m
tweets_day(AOC)
#2020 10 23 400 12.1M
tweets_day(BRN)
#2020-10-23 1000  30.1m
tweets_day(JB)
#2020-10-10 1000  18.3m
tweets_day(KH)
#2020-11-07 2500  2.9m
tweets_day(IO)
#/////////////////////////////////
tweets_day(PT)
tweets_day(VP)








#install.packages("ggrepel")
library(ggrepel)
library(plyr)

Data2$Politician<- revalue(as.factor(Data2$Politician), c("1"= "AOC", "2" = "Ted Cruz", "3" = "Marco Rubio", "4"= "MTGreen",
                                                          "5"= "Rand Paul", "6"="Bernie", "7"="Ilhan. O", "8"= "Harris",
                                                          "9"="Joe Biden"))

rownames(Data2)<-Data2$Politician

ggplot(Data2,aes(SP,SPP))+geom_point(shape =19, color= "salmon", size=3)+scale_x_continuous(position ="top")+
  xlab("Sentiment scores: Politician Tweets")+ ylab("Public Sentiment Scores") + 
  geom_text(label=rownames(Data2), nudge_x= 0.03, nudge_y =-0.05)








