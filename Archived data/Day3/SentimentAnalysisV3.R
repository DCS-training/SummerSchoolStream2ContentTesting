# Load required packages
library(tidyverse)
#library(tm)
library(syuzhet)
library(RColorBrewer)
# Step 1: Read CSV file
data <- read_csv("Day3/export/ReviewWithDistilleriesInfoV2.csv")



sentiment_scores <- get_nrc_sentiment(data$Reviews)


summary(sentiment_scores)


#Bar Chart by Emotion
barplot(
  colSums(prop.table(sentiment_scores[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Whisky reviews",
  sub = "Sentiments within reviews",
  xlab="emotions", ylab = NULL)


#Link back with reviews
FullSentiment<-cbind(data,sentiment_scores)


# Reshape the data from wide to long format, preserving other columns
long_dataWhisky <- gather(data = FullSentiment, 
                    key = "sentiment", 
                    value = "value", 
                    -(Distillery:Rate))

#because we want to look at proportion of the different sentiment let's remove positive/negative
 
long_dataWhisky_Sentiments<-filter(long_dataWhisky,sentiment!="positive" & sentiment!="negative")



# Create a box plot using ggplot
ggplot(long_dataWhisky_Sentiments, aes(x = sentiment, y=value, fill=sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Sentiment Proportions",
       x = "Sentiment", y = "value") +
  theme_minimal()+
  facet_wrap(~Region, scales = "free_y")


ggplot(long_dataWhisky_Sentiments, aes(x = sentiment, y=value, fill=sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Sentiment Proportions",
       x = "Sentiment", y = "value") +
  theme_minimal()+
  facet_wrap(~Owner, scales = "free_y")

#not very much of a difference can we look at the most recurring words within each sentiment 
# Because it is a quite big dataset we will do it ona  subset so we look only at the Lowland sample

Lowland<-subset(data, Region=="Lowland" )

library(tidytext)
words_list <- Lowland %>%
  unnest_tokens(word, Reviews) %>%
  filter(nchar(word) >3 & !str_detect(word, "^[0-9]+$")) %>%
  pull(word)


Lowlandsentiment_scoresT <- get_nrc_sentiment(words_list)

JoyWords <- words_list[Lowlandsentiment_scoresT$joy> 0]
head(JoyWords)


JoyWordsOrder <- sort(table(unlist(JoyWords)), decreasing = TRUE)
head(JoyWordsOrder, n = 12)


# Count the occurrences of each word
JoyTable <- table(JoyWords)

# Convert the table to a data frame
JoyDF <- as.data.frame(JoyTable)

# Rename the columns
names(JoyDF) <- c("word", "frequency")

install.packages("Polychrome")

library(Polychrome)
color_palette <- glasbey.colors(32)
library(wordcloud)
wordcloud(words = JoyDF$word,
          freq = JoyDF$frequency,
          scale=c(8,2),# rage of font sizes
          colors=color_palette,
          random.order=FALSE, rot.per=0.35,
          max.words =100)

#or if I want to plot frequencies

First20JoyDF<-JoyDF%>%
  arrange(desc(frequency))%>%
  head(20)
#aes(x = reorder(word,-frequency), y = frequency))
ggplot(First20JoyDF, aes(x = reorder(word,-frequency), y = frequency, colour=reorder(word, -frequency))) + # Plotting with ggplot2
  geom_point(size=5) +
  theme_bw() +
  labs(x = "Term", y = "Frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1))

 
# What is this telling about the words associate with Joy in our sample?
# What about terms associated with disgust? 
# Write your code below


#So far we looked at specific sentiments what about negative and positive

long_dataWhisky_PN<-filter(long_dataWhisky,sentiment=="positive" | sentiment=="negative")



ggplot(long_dataWhisky_PN, aes(x = sentiment, y=value, fill=sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Sentiment Proportions",
       x = "Sentiment", y = "value") +
  theme_minimal()+
  facet_wrap(~Region, scales = "free_y")

#again not too easy to look at let's use a pipe to create better proportions
proportionsArea <- long_dataWhisky_PN %>%
  group_by(Region,sentiment)%>%
  summarise(count= sum(value))%>%
  mutate(Ratio=(round(count/sum(count),2)*100))
  

ggplot(proportionsArea, aes(x = sentiment, y=Ratio, fill=sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Sentiment Proportions",
       x = "Sentiment", y = "value") +
  geom_text(aes(label = Ratio), position = position_stack(vjust = 0.5), color = "black") +
  theme_minimal()+
  facet_wrap(~Region)


#This is only counting words or proportion that has a negative or positive association what if we try to use a different sentiment dictionary 

#NB there is an incredible amount of dictionaries and sentiment analysis models out there some more refined than others so if you are using sentiment analysis for your research you always have to read the published article associated with the methods and identify the one that would better suit your research questions. Some are more advanced than other with some recent model than allows you to implement machine learning and Bayesian techniques into your processing but this is way beyond what we can cover in this 90 minutes if you are interested in that aspect you can have a look at this blog post https://www.r-bloggers.com/2016/01/sentiment-analysis-with-machine-learning-in-r/

# that been said let's have a look at a different library to do sentiment analysis i.e. the sentiment extension of the quanteda package 
library(tidyverse)
library(quanteda)
#library(quanteda.textmodels)
library(quanteda.sentiment)

ToksReviewsQuanteda <- data$Reviews %>% 
  corpus() %>%
  tokens(
    what = "word",
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE
  ) %>% 
  tokens_remove(., pattern = stopwords("en"))

ToksReviewsQuanteda
# the data dictionary that we are using has 4 categories, positive, negative and neg_positive and neg_negative
#https://quanteda.io/reference/data_dictionary_LSD2015.html

# We subset the data dictionary for only the positive and negative words, so we can calculate polarity scores as you have seen James doing earlier today 
polarity(data_dictionary_LSD2015) <- 
  list(pos = c("positive", "neg_negative"), neg = c("negative", "neg_positive"))

# Now we can calculate the polarity of each text in toks, to get a sense of how positive vs negative each text is
Polarity1 <- as.data.frame(textstat_polarity(ToksReviewsQuanteda, data_dictionary_LSD2015, fun = sent_logit))
# this is the default one so we do not really have to specify the fun used but to completeness
Polarity1 
Polarity2 <- textstat_polarity(ToksReviewsQuanteda, data_dictionary_LSD2015,  fun = sent_abspropdiff)
Polarity2
Polarity3 <- textstat_polarity(ToksReviewsQuanteda, data_dictionary_LSD2015,  fun = sent_relpropdiff)
Polarity3

summary(Polarity1$sentiment) #can go from -n +n 
summary(Polarity2$sentiment) #can go from -1 +1
summary(Polarity3$sentiment) #can go from -1 +1


# And plot it
ggplot(data = Polarity1, aes(x = sentiment, y = doc_id, colour=sentiment)) +
  geom_point()+
  theme_bw()+
  geom_vline(xintercept = 0, colour="red", size=1)+
  scale_colour_gradientn(colors=rainbow(7)) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())#I removed the thick and labels of the y axis cause we don't really need to know which is which 

# Now try to visualise it for the other methods, does it change anything 
  

#Compare rate with polarity 

#Because we have a rate number on a scale between 0 and 100 associated with each review why don't we compare the results of the different methods with that 


# Linear transformation to scale between 0 and 100
#we cannot use polarity 1 cause we do not have fixed scale 

transformedPol2 <- ((Polarity2$sentiment + 1)) * 50
transformedPol3 <- ((Polarity3$sentiment + 1)) * 50


#If you want to look by areas we need to put it back in 

FullPolarity<-cbind(data,doc_id=Polarity1$doc_id,PolAbspropdiff=transformedPol2, PolRelpropdiff=transformedPol3)




#lets add the syuzet one as well 


FullSentiment <- FullSentiment %>%
  mutate(scaledPolarity =  ((positive - negative) / (positive +negative)))


#Add it to the polarity dataset in a scale between 0 and 100

FullPolarity$syuzhetPolarity <- ((FullSentiment$scaledPolarity + 1)) * 50

#Breakout of the transformed values
summary(FullPolarity$syuzhetPolarity)

#Test reviews vs sentiment review 
# Remove rows with NA values in Column1
df_clean <- FullPolarity[complete.cases(FullPolarity$Rate), ]

#need to swap order between doc_id and Rate
df_cleanSub <- df_clean[,c(1:8,10,9,11:13)]

# Reshape data from wide to long format
data_long <- tidyr::gather(df_cleanSub, key = "Rate_Sentiment", value = "value", -(Distillery:doc_id))



# Plot the graph
ggplot(data_long, aes(x = doc_id, y = value, color = Rate_Sentiment,group = Rate_Sentiment)) +
  geom_line() +
  labs(title = "Linear Relationship of Three Variables",
       x = "X", y = "Value") +
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.text.x = element_blank(),   # Remove x-axis tick labels
    axis.ticks.x = element_blank()   # Remove x-axis tick marks
  )


# Plot the graph but this time by region

ggplot(data_long, aes(x = doc_id, y = value, color = Rate_Sentiment,group = Rate_Sentiment)) +
  facet_wrap(~Region, scales = "free_x", ncol=2)+
  geom_line() +
  labs(title = "Linear Relationship of Three Variables",
       x = "X", y = "Value") +
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),  # Remove x-axis label
    axis.text.x = element_blank(),   # Remove x-axis tick labels
    axis.ticks.x = element_blank()   # Remove x-axis tick marks
  )


#Final question for the day, 
#Which method do you think is more accurate or better better reflect the rate given to the bottle 