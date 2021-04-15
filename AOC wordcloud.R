install.packages("qdapRegex")
install.packages("wordcloud2")
rm(list=ls()) 

library(RColorBrewer)
library(rtweet)        # Used for extracting the tweets
library(tm)            # Text mining cleaning
library(stringr)       # Removing characters
library(qdapRegex)     # Removing URLs 
library(wordcloud2)
library(igraph)
library(tidyverse)
library(ggraph)
library(data.table)


twitter_token <- rtweet::create_token(
  app = "ricechicken",
  consumer_key <- "fZxHNciyOQx7JpQknh5lH23rR",
  consumer_secret <- "cDpC4gutEQqkTFXP813r8XnoNIEvWIpj8C1UPd4PRG9bhD00el",
  access_token_key <- "1282900852823523328-ISVIHvnKk0nfp1oD65375tu0osCXVv",
  access_secret <- "445Yt7i2TU4tBFsiDpkxoz44Pxy2yW1ksla9t1aNT1mBZ")
#get tweets by AOC
AOC2 <- rtweet::search_tweets(q = "AOC", n = 100, lang = "en", token = twitter_token)


AOC <- get_timelines(c("AOC"), n = 100)
#Just the text collum collapsed
text <- str_c(AOC$text, collapse = "")
#remove words and spaces
text <- 
  text %>%
  str_remove("\\n") %>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%             # Remove any @ mentions
  removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
  removeNumbers() %>%
  stripWhitespace()%>%
  removePunctuation()%>%
  removeWords(c("'s","&","&amp;",";","amp","S","s","'","'re"))                      # Final cleanup of other small changes

# Convert the data into a summary table
textCorpus <- Corpus(VectorSource(text))%>%TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)


wordcloud <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
wordcloud

#Formula//////////////////////////////////////////////////////////////////////////////////////////

TweetsToWordcloud <- function(username){
  
  tweets <- get_timelines(username, n = 3200)
  
  # Clean the data
  text <- str_c(tweets$text, collapse = "") %>%
    str_remove("\\n") %>%                   # remove linebreaks
    rm_twitter_url() %>%                    # Remove URLS
    rm_url() %>%
    str_remove_all("#\\S+") %>%             # Remove any hashtags
    str_remove_all("@\\S+") %>%             # Remove any @ mentions
    str_remove_all("-\\S+") %>%             # Remove any @ mentions
    
    removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
    removeNumbers() %>%
    stripWhitespace()%>%
    removePunctuation()%>%
    removeWords(c("'s","&","&amp;",";","amp","S","s","'","'re"))                   # Final cleanup of other small changes
  
  # Convert the data into a summary table
  textCorpus <- 
    Corpus(VectorSource(text)) %>%
    TermDocumentMatrix() %>%
    as.matrix()
  
  textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
  textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)
  
  wordcloud <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
  return(wordcloud)
}

TweetsToWordcloud(username = "LeaderMcConnell")
TweetsToWordcloud(username = "AOC")
TweetsToWordcloud(username = "BernieSanders")



#//////////////////////////////////////////////////////////////////////////////////////////
library(rtweet)
library(igraph)
library(tidyverse)
library(ggraph)
library(data.table)
### Create igraph object from Twitter data using user id and mentioned id.
AOC2 <- rtweet::search_tweets(q = "LeaderMcConnell", n = 1000, lang = "en", token = twitter_token)

filter(AOC2, retweet_count > 0 ) %>% 
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  graph_from_data_frame() -> AOC_g
V(AOC_g)$node_label <- unname(ifelse(degree(AOC_g)[V(AOC_g)] > 20, names(V(AOC_g)), "")) 
V(AOC_g)$node_size <- unname(ifelse(degree(AOC_g)[V(AOC_g)] > 20, degree(AOC_g), 0)) 



ggraph(AOC_g, layout = 'dh') + 
  geom_edge_arc(edge_width=0.1, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="#ffffff66", segment.colour="light blue",
                  color="red", repel=TRUE) +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  labs(title="McConnell Twitter Plot", subtitle="Edges=volume of retweets. Screenname size=influence") +
  theme_bw() +
  theme(legend.position="none") 


#////////////////////////////////////////////////////////////////////#////////////////////////////////////////////////////////////////////

cm <- rtweet::search_tweets("communist OR communism", n = 100,
                            retryonratelimit = TRUE)


text <- str_c(cm$text, collapse = "")
#remove words and spaces
text <- 
  text %>%
  str_remove("\\n")%>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%             # Remove any @ mentions
  removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
  removeNumbers() %>%
  stripWhitespace()%>%
  removePunctuation()%>%
  removeWords(c("'s","&","&amp;",";","amp","S","s","'","'re","communist","communism","Communist","Communism","The"))                      # Final cleanup of other small changes

# Convert the data into a summary table
textCorpus <- Corpus(VectorSource(text))%>%TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)


wordcloud <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
wordcloud


#socialism//////////////////////////////////////////////////////////////////////////////////////////////////////////
sm <- rtweet::search_tweets("socialism OR socialist", n = 100,
                            retryonratelimit = TRUE)


text <- str_c(sm$text, collapse = "")
#remove words and spaces
text <- 
  text %>%
  str_remove("\\n")%>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%             # Remove any @ mentions
  removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
  removeNumbers() %>%
  stripWhitespace()%>%
  removePunctuation()%>%
  removeWords(c("'s","&","&amp;",";","amp","S","s","'","'re","socialism","socialist","Socialist","Socialism","The"))                      # Final cleanup of other small changes

# Convert the data into a summary table
textCorpus <- Corpus(VectorSource(text))%>%TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)


wordcloud <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.6)
wordcloud




#find equivalent library 
#scikit learn


#assign values to words as positive or negative, compare twitter activity on politicians,
#assign scores to their activity, chart that score trought a period of time, see when they are more negative???






