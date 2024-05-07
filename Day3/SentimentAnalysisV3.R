# Load required packages
library(tidyverse)
library(tm)
library(syuzhet)
library(RColorBrewer)
# Step 1: Read CSV file
data <- read_csv("Day3/export/data/ReviewWithDistilleriesInfo.csv")

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
                    -(Distillery:Reviews))

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

#not very much of a difference what about positive and negative

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
  geom_text(aes(label = Ratio)), position = position_stack(vjust = 0.5), color = "black") +
  theme_minimal()+
  facet_wrap(~Region)
