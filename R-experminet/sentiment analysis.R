install.packages("twitteR")
install.packages("ROAuth")
install.packages("httr")
install.packages("rjson")
install.packages("devtools")
install.packages("base64enc")
install.packages("streamR")
install.packages("RCurl")
install.packages("stringr")
install.packages("httpuv")
install.packages("tm")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("tidytext")
install.packages("dplyr")
install.packages("wordcloud2")
install.packages("RColorBrewer")
install.packages("htmlwidgets")
install.packages("gridExtra")
install.packages("wordcloud")
install.packages("lubridate")
install.packages("purrr")
install.packages("magrittr")
install.packages("syuzhet")
install.packages("scales")
install.packages("reshape2")
install.packages("textdata")
install.packages("languageserver")

# Load the library
library(twitteR)
library(ROAuth)
library(httr)
library(rjson)
library(devtools)
library(base64enc)
library(streamR)
library(RCurl)
library(stringr)
library(httpuv)
library(tm)
library(ggplot2)
library(tidyr)
library(tidytext)
library(dplyr)
library(wordcloud2)
library(RColorBrewer)
library(htmlwidgets)
library(gridExtra)
library(wordcloud)
library(lubridate)
library(data.table)
library(purrr)
library(magrittr)
library(syuzhet)
library(scales)
library(reshape2)
library(textdata)


devtools::install_github("lchiffon/wordcloud2")

setwd("C:/local/folder")

setup_twitter_oauth(consumer_key, consumer_secret, access_token = NULL, access_secret = NULL)

# extract tweets based on a search term
searchTerm<-"#bitcoin + #btc + #crypto"
trendingTweets = searchTwitter(searchTerm,n=10000)


trendingTweets.df = twListToDF(trendingTweets)
trendingTweets.df$text <- sapply(trendingTweets.df$text, function(x) iconv(x, to = 'UTF-8'))
trendingTweets.df$created <- ymd_hms(trendingTweets.df$created)
head(trendingTweets.df)
write.csv(trendingTweets.df, "cryptoTweets.csv")


#Read the Twitter data
tweets.df <- trendingTweets.df

set.seed(1)

# Convert text to data frame, and then corpus of documents 
corpus <- iconv(tweets.df$text, "LATIN2", to = "UTF-8")
corpus <- Corpus(VectorSource(corpus))




#Transform to lower case 
corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "said", "also", "will", "last", "new", "one", "many", "nrt"))

#Remove the URL's
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
corpus <- tm_map(corpus, removeURL)

# Remove the punctuations
corpus <- tm_map(corpus, removePunctuation)

#Remove the numbers
cleanset <- tm_map(corpus, removeNumbers)


# replace oldword with newword 
replaceWord <- function(corpus, oldword, newword) { tm_map(corpus, content_transformer(gsub), pattern = oldword, replacement = newword) }
myCorpus <- replaceWord(cleanset, "europe", "eu")




# Create Term Document Matrix from clean corpus
tweet.tdm <- TermDocumentMatrix(myCorpus)
tweet.tdm <- as.matrix(tweet.tdm)


# Top  5 terms? Visualise with ggplot 2 barchart
# Convert the TDM into a matrix first, then data frame to plot 
freq.terms <- rowSums(as.matrix(tweet.tdm))
tweets.plot <- data.frame(term = names(freq.terms), freq = freq.terms)

tweets.plot %>%
    arrange(freq) %>%
    top_n(5) %>%
    ggplot(aes(x = term, y = freq, fill = term)) +
    geom_bar(stat = "identity") +
    xlab("Terms") +
    ylab("Counts") +
    scale_fill_brewer(palette = "Set1") +
    coord_flip()



# 'Letter cloud' of frequent (> 15 freq) terms in shape 'elections'
tweets.plot %>%
    filter(freq > 15) %>%
    letterCloud(word = "VOTE", size = 3)





# Sentiment analysis with tidytext - How do people feel about election in india in these tweets?
# Join the words with NRC sentiment lexicon

all_tweets <- tweets.plot %>%
    mutate(word = term) %>%
    left_join(get_sentiments("nrc")) %>%
    group_by(word) %>%
    filter(sentiment != "NA") %>%
    ungroup()
all_tweets

filterplot_positive <- all_tweets %>%
    filter(sentiment == "positive")

filterplot_negative <- all_tweets %>%
    filter(sentiment == "negative")
# Plot overall sentiment counts for tweets
tweet_plot <-
    ggplot(data = all_tweets, aes(x = as.factor(sentiment), y = freq, fill = sentiment)) +
    geom_bar(stat = "identity") +
    xlab("Sentiment") +
    ylab("Frequency") +
    ggtitle("Sentiment Profile of all tweets") +
    theme(legend.position = "none") +
    coord_flip()
tweet_plot


# Plot top 10 words classified 'negative' sentiment in tweets
negative <-
    ggplot(data = filterplot_negative, aes(x = as.factor(sentiment), y = freq, fill = sentiment)) +
    geom_bar(stat = "identity") +
    xlab("Sentiment") +
    ylab("Frequency") +
    theme(legend.position = "none") +
    coord_flip()

# Plot top 10 words classified 'positive' sentiment in tweets
positive <-
    ggplot(data = filterplot_positive, aes(x = as.factor(sentiment), y = freq, fill = sentiment)) +
    geom_bar(stat = "identity") +
    xlab("Sentiment") +
    ylab("Frequency") +
    theme(legend.position = "none") +
    coord_flip()

# Let's compare those
grid.arrange(positive, negative)

 # Word cloud 
library(wordcloud)
w <- sort(rowSums(tweet.tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 100,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(2, 0.9),
          rot.per = 0.1)
