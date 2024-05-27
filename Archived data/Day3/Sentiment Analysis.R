#Sentiment Analysis


# 1. Getting Setup ====================
## 1.1. Libraries needed--------------
install.packages("syuzhet")
install.packages("vader")
install.packages("wordcloud")
install.packages("tm")
library(syuzhet)
library(vader)
library(tidyverse)
library(wordcloud)
library(tm)

# Import the dataset 
whiskyReview1<-read_csv("Day3/export/data/ReviewWithDistilleriesInfo.csv")
wiskyReview2<-read_csv("Day1/DataWrangling/Data/scotch_review_manual_clean.csv")

#our full dataset are too big so we are going to work on a subset
whiskyReview1S<-sample_n(whiskyReview1, 300, set.seed=1234)


library(vader)
library(parallel)
library(doParallel)

# Initialize a cluster with desired number of workers
cl <- makeCluster(4, type = 'PSOCK', outfile = '')

# Register the cluster for parallel computation
registerDoParallel(cl)

# Apply the function get_vader in parallel
result <- foreach(review = Whisky1) %dopar% {
  vader::get_vader(review)
}

# Stop the cluster
stopCluster(cl)

# Combine the results if needed
combined_result <- do.call(rbind, result)



library(foreach)
library(doParallel)
library(vader)

# Initialize a cluster
cl <- makeCluster(detectCores())  # Use all available cores
registerDoParallel(cl)

# Apply sentiment analysis in parallel
result <- foreach(review = whiskyReview1S$Reviews) %dopar% {
  vader::get_vader(review)
}

# Stop the cluster
stopCluster(cl)



## 2.2. Calculate sentiment scores with VADER --------
Whisky1 <- whiskyReview1S$Reviews%>%
  lapply(get_vader)# lapply= apply across the data set

## 2.3. Add compound sentiment score to the data frame --------
# On Scotland data set
df <- df %>%
  mutate(compound = HistoryScores %>%
           sapply(function(v) { as.numeric(v["compound"]) }))


## 2.4. Statistical overview -------
# Mean score and range of scores for each data set
# On Uk Data
mean(df$compound)
range(df$compound)


# Print the titles of the top negative and top positive posts for each data set
# Create a new object for Scotland
df<-df[order(df$compound, decreasing = TRUE), ]
# Print head and tail
head(df$Title, 5)
tail(df$Title, 5)

# Create a new object for UK
SentimentUK<-SentimentUK[order(SentimentUK$compound, decreasing = TRUE), ]
# Print head and tail
head(SentimentUK$title, 1)
tail(SentimentUK$title, 1)



# 3. Sentiment scores by date ========
## 3.1. Data Cleaning -----------
### 3.1.1. Standardise date format + remove time stamp 
# On the Scotland Data set 

df$date <- mdy(df$Date)
df$MonthYear <- format(as.Date(df$date, format="%Y-%m-%d"),"%Y-%m")  
require(zoo)
df$Month<-format(as.yearmon(df$MonthYear, format="%Y-%m"),"%b" )
df$Year<-format(as.yearmon(df$MonthYear, format="%Y-%m"),"%Y" )
### 3.1.3. Group by --------
# Scotland
ByMonthYearScotland<-df%>%
  group_by(Month, Year)%>%
  mutate(Month=as.factor(Month),
         Year=as.factor(Year))%>%
  summarise(meanSent=mean(compound))


levels(ByMonthYearScotland$Month)
# Reorder levels
#Reorder
ByMonthYearScotland$Month <- factor(ByMonthYearScotland$Month, levels=c('Jan', "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


#Graph 1 
ggplot(ByMonthYearScotland, aes(Month, Year, fill= meanSent)) + 
  geom_tile(colour="grey28")+
  theme_bw()+
  geom_text(colour="grey28",aes(label=round(meanSent,2)))+
  scale_fill_gradient(low = "blue", high = "green")+
  labs(title="Sentiment Posts", x= "Month")





## 3.2. Create plots ------------
### 3.2.1. Scotland
# Define my gradient 
gradient <- colorRampPalette(c("red", "green"))
ggplot(ByMonthYearScotland, aes(x = MonthYear, y = meanSent, colour=meanSent)) +
  geom_path(aes(group=1), colour="black", size=1)+
  geom_hline(yintercept=0, linetype='dotted')+
  geom_point(size = 7) +
  scale_colour_gradientn(colours = gradient(10)) +
  labs(title='r/Scotland VADER Compound Sentiment', x = "Post Date", y = "Sentiment Score") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





# Let's try it with Wisky 








# 4. Methods for filtering the data ============
## 4.1. Subset the data frame by posts from a certain date (year = 2023, data=Scotland)------------
Scotland23<-Scotland %>%
  filter(str_detect(date, "2023"))


## 4.2. Plot the 2023 posts ---------------
# Define my gradient 
gradient <- colorRampPalette(c("red", "green"))
ggplot(Scotland23, aes(x = date, y =Sentiment, colour=Sentiment)) +
  geom_point(size = 7) +
  scale_colour_gradientn(colours = gradient(10)) +
  labs(title='r/Scotland VADER Compound Sentiment', x = "Post Date", y = "Sentiment Score") +
  theme_minimal()+
  geom_hline(yintercept=0, linetype='dotted')

## 4.2.Exercise 1 --------------------
# With your table, create a plot for a subset of your choice (e.g. r/UK posts from 2022, r/Scotland posts with compound >=0.5, etc). Discuss your findings
#Then, share your plot with the class.

# 5. Compare the top positive and negative posts ====
# in r/Scotland
## 5.1. Subset the top 25 positive and top 25 negative posts---------
top_positive <- SentimentScotland[1:25, ]
top_negative <- SentimentScotland[(nrow(SentimentScotland)-24):nrow(SentimentScotland), ]
## 5.2. Clean and tokenize ------
# First the subset positive data:
library(quanteda)
pos_subset <- gsub("http\\S+\\s*", "", top_positive)
pos_subset <- tolower(pos_subset)
pos_subset <- quanteda::tokens(pos_subset)
pos_subset <- tokens_remove(pos_subset, stopwords("en"))
pos_subset <- tokens_select(pos_subset, stopwords("en"), selection = "remove")
pos_subset <- tokens_select(pos_subset, min_nchar = 3)
## 5.3. Generate the word cloud -----
wordcloud(pos_subset,
          random.order=FALSE,
          max.words=100,
          colors=brewer.pal(n=5, name = "Dark2"))
#Repeat for the subset negative data:
neg_subset <- gsub("http\\S+\\s*", "", top_negative)
neg_subset <- tolower(neg_subset)
neg_subset <- quanteda::tokens(neg_subset)
neg_subset <- tokens_remove(neg_subset, stopwords("en"))
neg_subset <- tokens_select(neg_subset, stopwords("en"), selection = "remove")
neg_subset <- tokens_select(neg_subset, min_nchar = 3)
# Generate the word cloud
wordcloud(neg_subset, random.order=FALSE, max.words=100, colors=brewer.pal(n=5, name = "Set2"))

#Bonus (if time): with your table, repeat the process for the r/UK datset. Then, discuss your findings.

