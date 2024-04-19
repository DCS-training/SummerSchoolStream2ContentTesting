# Load requested packages (this loads them into your environment)
library(tidyverse)
library(rvest)

## The Basics of Rvest ===================
# The first part of web scraping with Rvest is usually to dowload the HTML from your desired webpage. This can be done using the read_html function, with using the website's URL as the first parameter.

## Scraping web forums ===================

# Load the index page into an object
wpage <- read_html('https://historum.com/tags/scotland')
# And have a look
wpage

# We will notice this web page has a variety of sub forums.How do we get the posts from all of these? The easiest way of doing this is to navigate down the tree



# Let's look there now 
#... open the link ...

# The first page
page1 <- 'https://historum.com/tags/scotland'
# The first part of the urls
start_format <- 'https://historum.com/tags/scotland/page-'
# the numbers for the rest of the pages (here we are using a sample so it's more managable later on)
n <- c(2:7)


# Paste the three objects together to create a list of URLS complete. Use paste0 so there is no space between the two strings
pages <- paste0(start_format, n)
pages <- c(page1, pages)

# Have a look at the beginning of the list
head(pages)
length(pages)

# Unlike Python, the preferred approach to web scraping in R will be functional, not iterative. This means instead of for loops, we will write and map functions over our HTML.

# In RVest, the typical approach is to figure out the task we want to perform, write a function to perform this task, and then map the function over a list of objects
# Our first task is to get the links to each article on each of our pages. We can write a function for this, then then map it over the pages


# Read the pages ===================
get.article.links <- function(x){
  links <- read_html(x) %>%
    html_nodes('.contentRow-title a') %>%
    html_attr('href')
}

ArticleLinks <- map(pages, get.article.links)

head(ArticleLinks)
length(ArticleLinks)

# What is wrong with this object?
# We get a list of lists, not a list. This will make it difficult to map over our variable, since each item has multiple items within it.

# So we need to use the flatten function to deal with this
ArticleLinksFlat <- ArticleLinks %>% 
  flatten()

head(ArticleLinksFlat)
length(ArticleLinksFlat)

# That looks better! Now I still need to add the https://historum.com bit

ArticleLinksFlatFull<-paste0("https://historum.com", ArticleLinksFlat)

head(ArticleLinksFlatFull)

# Start scraping posts ===================
# Now we move one branch down on the tree, and we can map a new function on to our list of URLs to extract the content. First the title, then the date, then the body text.

# To make it more manageable, we will test each function on a subset of our pages before running it on the full thing. This lets us modify the function if we don't get the output we want.

# Create test set of links 
test <- head(ArticleLinksFlatFull)
test

# Write a function to get titles
get.title <- function(x){
  title <- read_html(x) %>% 
    html_node('.p-title-value') %>%
    html_text()
}

# Test the function
ArticlesTest_Title <- map(test, get.title)
ArticlesTest_Title

# Write a function to get the date
get.date <- function(x){
  date <- read_html(x) %>%
    html_node('.u-dt') %>%
    html_text()
}

# Test the function
ArticlesTest_Date <- map(test, get.date)
ArticlesTest_Date


# Write a function to get the text
get.text <- function(x){
  body <- read_html(x) %>%
    html_node('.bbWrapper') %>%
    html_text()
}

# Test the function
ArticlesTest_Text <- map(test, get.text)
ArticlesTest_Text

# Now that we know they work, time to apply them to the full data set
# Note that a big issue with web scraping is that we can get blocked if we scrape too quickly, since websites take measures against people who access them too frequently. 
Articles_Title <- map(ArticleLinksFlatFull, get.title)
Articles_Date <- map(ArticleLinksFlatFull, get.date)
ArticlesTest_Text <- map(ArticleLinksFlatFull, get.text)


# Turn this into a df
df <- do.call(rbind, Map(data.frame, Title=Articles_Title, Text=ArticlesTest_Text, Date=Articles_Date))


#Export file

write.csv(df, "Data/HistoryForum.csv")

# Sentiment Analysis




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



## 2.2. Calculate sentiment scores with VADER --------
HistoryScores <- df$Text %>%
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

