#' Title: GME Case
#' Author: Maria Fernanda Pernillo
#' email: mpernillodominguez2019@student.hult.edu
#' Date: March 10 2021


# WD
setwd("~/Personal/hult_NLP_student/cases/session II/WallStreetBets")


# Libs
library(tidytext)
library(dplyr)
library(stringr)
library(tibble)
library(ggplot2)
library(tm)
library(lexicon)
library(echarts4r)
library(tidyr)
library(corpus)
library(ggthemes)


#Get sentiments from 3 separate libraries 
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


#Call Supporting Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Build the stop words function
stops <- c(stopwords('SMART'), 'just','like', 'shit', 'fucking', 'fuck', 'make', 'big','put','gamestop', 'gme',
           'thing','made','thing', 'wsb')

# Data
#txt <- read.csv(file.choose('CASE_gme.csv'))
#txt$allText <- paste(txt$comment, txt$comment)

# Read in Data, clean & organize
text      <- read.csv('CASE_gme.csv')
txtCorpus <- VCorpus(VectorSource(text$comment))
txtCorpus <- cleanCorpus(txtCorpus, stops)
gmeTDM  <- TermDocumentMatrix(txtCorpus)
gmeTDMm <- as.matrix(gmeTDM)

# Frequency Data Frame
gmeSums <- rowSums(gmeTDMm)
gmeFreq <- data.frame(word=names(gmeSums),frequency=gmeSums)

# Review a section
gmeFreq[50:55,]

# Remove the row attributes meta family
rownames(gmeFreq) <- NULL
gmeFreq[50:55,]

# Simple barplot; values greater than 15
topWords      <- subset(gmeFreq, gmeFreq$frequency >= 15) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)


##Inspect word associations with term CITRON
associations <- findAssocs(gmeTDM, 'citron', 0.30) #from line 50
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3) 

## Inspect word associations with term NOK
associations <- findAssocs(gmeTDM, 'nok', 0.30) #from line 50
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3)

## Inspect word associations with term RETAIL
associations <- findAssocs(gmeTDM, 'retail', 0.40) #from line 50
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3)



## Inspect word associations with term SHORT
associations <- findAssocs(gmeTDM, 'short', 0.40) #from line 50
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3)



## Inspect word associations with term SHORT
associations <- findAssocs(gmeTDM, 'tesla', 0.40) #from line 50
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3)
#Analyze the mentions of gme during January 
library(readr)
library(lubridate)
reddit <-read_csv(url('https://raw.githubusercontent.com/kwartler/hult_NLP_student/main/cases/session%20II/WallStreetBets/CASE_gme.csv'))


# Mentions by month
reddit$gmeMention <- grepl('gme', reddit$comment, ignore.case = T)
x <- aggregate(gmeMention~comm_date_yr+comm_date_month, reddit, sum)
x <- x[order(x$comm_date_yr, x$comm_date_month),]
plot(x$gmeMention, type = 'l')


# One Month
oneMonth <- subset(reddit,  reddit$comm_date_yr==2021 & 
                     reddit$comm_date_month==1)
oneMonth <- oneMonth[order(oneMonth$comm_date),]
y <- aggregate(gmeMention~+comm_date, oneMonth, sum)
plot(y$gmeMention, type = 'l')
plot(cumsum(y$gmeMention), type = 'l') 



# Clean and Organize the old way instead of cleanMatrix for the sentiment analysis
txt <- read.csv('CASE_gme.csv')
table(txt$comment) 


#Examine the emotional words used in these articles
txtDTM <- VCorpus(VectorSource(txt$comment))
txtDTM <- cleanCorpus(txtDTM, stops)
txtDTM <- DocumentTermMatrix(txtDTM)

# Examine 
as.matrix(txtDTM[1:10,100:110])
dim(txtDTM)

#txtDTM      <- as.DocumentTermMatrix(txtDTM, weighting = weightTf ) 
tidyCorp <- tidy(txtDTM)
tidyCorp[100:110,]
dim(tidyCorp)

# Get bing lexicon
# "afinn", "bing", "nrc", "loughran"
bing <- get_sentiments(lexicon = c("bing"))
head(bing)

# Perform Inner Join
bingSent <- inner_join(tidyCorp, bing, by=c('term' = 'word'))
bingSent

# Quick Analysis
table(bingSent$sentiment) #tally ignoring count
table(bingSent$sentiment, bingSent$count) #only a few with more than 1 term
aggregate(count~sentiment,bingSent, sum) #correct way to sum them


# Get afinn lexicon
afinn<-get_sentiments(lexicon = c("afinn"))
head(afinn)

# Perform Inner Join
afinnSent <- inner_join(tidyCorp,afinn, by=c('term' = 'word'))
afinnSent

# Examine the quantity
afinnSent$afinnAmt     <- afinnSent$count * afinnSent$value

# Compare w/polarity and bing
mean(afinnSent$afinnAmt)

# FAKE EXAMPLE: if the documents were related and temporal, make sure they are sorted by time first!
# Example use case : i.e. over time how was the emotional content for a topic i.e. Pakistan articles
afinnTemporal          <- aggregate(afinnAmt~document, afinnSent, sum)
afinnTemporal$document <- as.numeric(afinnTemporal$document)
afinnTemporal          <- afinnTemporal[order(afinnTemporal$document),]

# Quick plot
plot(afinnTemporal$afinnAmt, type="l", main="Quick Timeline of Identified Words") 


# Quick Check with the pptx for a reminder.

# Get nrc lexicon; deprecated in tidytext, use library(lexicon)
#nrc <- read.csv('nrcSentimentLexicon.csv')
nrc <- nrc_emotions
head(nrc)

# Tidy this up
nrc <- nrc %>% pivot_longer(-term, names_to = "emotion", values_to = "freq")
nrc <-subset(nrc, nrc$freq>0 )
head(nrc)
nrc$freq <- NULL #no longer needed

# Perform Inner Join
nrcSent <- inner_join(tidyCorp,nrc, by=c('term' = 'term'))
nrcSent

# Radar chart
table(nrcSent$emotion)
emos <- data.frame(table(nrcSent$emotion))
names(emos) <- c('emotion', 'termsCt')
emos %>% 
  e_charts(emotion) %>% 
  e_radar(termsCt, max = max(emos$termsCt), name = "Emotions") %>%
  e_tooltip(trigger = "item") %>% e_theme("dark-mushroom")

# Other Emotion Lexicons Exist
emotionLex <- affect_wordnet
emotionLex
table(emotionLex$emotion)
table(emotionLex$category)

emotionLex <- subset(emotionLex, 
                     emotionLex$emotion=='Positive'|emotionLex$emotion=='Negative')

# More emotional categories, fewer terms
lexSent <- inner_join(tidyCorp,emotionLex, by=c('term' = 'term'))
lexSent
emotionID <- aggregate(count ~ category, lexSent, sum)
emotionID %>% 
  e_charts(category) %>% e_theme("dark-mushroom") %>%
  e_radar(count, max =max(emotionID$count), name = "Emotional Categories") %>%
  e_tooltip() %>%
  e_theme("dark-mushroom") 

# End










