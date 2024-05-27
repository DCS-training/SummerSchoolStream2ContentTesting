############ TEXT ANALYSIS  ##################


# PART 2: TOPIC MODELLING ###########
# 1. Setting up ===================
## 1.1. Libraries needed -----------
install.packages("quanteda.textstats")
install.packages("syuzhet")
install.packages("topicmodels")
library(quanteda)
library(tidyverse)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(tm)
library(topicmodels)
library(syuzhet)
library(RColorBrewer)
library(topicmodels)


## 1.1. Load the data------------
Parish <- read_csv("Day2/data/Parishes.csv")

## 1.2. Extract the text column ------
# Subset the text column for the Edinburgh data and save it as an object:
ParishText<-Parish$text[Parish$Area=='Edinburgh']

## 1.3. Create a tm Corpus -----------------
# Prepare the data for analysis, creating and cleaning a tm Corpus object:
ParishCorpus <- VCorpus(VectorSource(ParishText))# transform our data set in a corpus
ParishCorpus<- tm_map (ParishCorpus, content_transformer(tolower))# remove capitalised letters
ParishCorpus <- tm_map (ParishCorpus, removePunctuation)# remove punctuation
ParishCorpus<- tm_map (ParishCorpus, removeWords, stopwords('english')) # remove English stopwords
ParishCorpus <- tm_map (ParishCorpus, removeWords, c('s', 't', '@\\w+', 'http.+ |http.+$','amp')) # remove specific words/symbols
ParishCorpus<- tm_map (ParishCorpus, removeNumbers)# remove numbers
ParishCorpus <- tm_map (ParishCorpus, stripWhitespace) # remove multiple white spaces

## 1.4. Create a term frequency matrix --------------
# Summarise the occurence of each word in the corpus, create a matrix, and print the 30 most frequent keywords.
LdaDtmParish <- DocumentTermMatrix(ParishCorpus)
inspect(LdaDtmParish) 

LdaDtmParishMx<- as.matrix(LdaDtmParish)
term_freq_P <- colSums(LdaDtmParishMx)
term_freq_P <- sort(term_freq_P, decreasing=TRUE)
term_freq_P[0:30]

#What can term frequencies tell us? 

# 2. Topic Modelling=================================
## 2.1. Create a document term matrix (dtm) of the corpus------
# A DTM is a mathematical matrix that describes the frequency of terms that occur in a collection of documents.  
# Rows correspond to documents in the collection and columns correspond to terms.
# We are going to include only terms that occur more than 500 times in the corpus.
min_freq <- 500
LdaDtmParish <- DocumentTermMatrix(ParishCorpus, control = list(bounds = list(global = c(min_freq, Inf))))

dim(LdaDtmParish) #print the dimensions of the dtm 
Terms(LdaDtmParish) #print terms in the dtm

# Remove null rows that contain no text:
complete_rows <- slam::row_sums(LdaDtmParish) > 0
LdaDtmParish_complete <- LdaDtmParish[complete_rows, ]

## 2.2.1. Create the topic model:
# Fit the LDA model
lda_model_5 <- LDA(LdaDtmParish_complete, k = 5, method = "Gibbs") #k is the number of topics to be created, and method is the type of LDA that will be performed

#View the top 15 terms for each topic.
top_terms_5 <- terms(lda_model_5, 15)
top_terms_5

### 2.2.2. Create more topics--------------
# Let's try the LDA again, expanding the number of topics to 10
lda_model_10 <- LDA(LdaDtmParish_complete, k = 10, method = "Gibbs")

#View the top 15 terms for each topic.
top_terms_10 <- terms(lda_model_10, 15)
top_terms_10

# What can we observe about the effect of adding more topics? 
# With your table, come up with a label for each topic.  What can we learn about our data using LDA? 

### 2.3.1. Remove custom stopwords and repeat k=5 topic modelling --------------
# Let's remove some of the words appearing that aren't telling us much about the data, and re-run LDA:
ParishCorpus2 <- tm_map(ParishCorpus, removeWords, c('edinburgh', 'parish', 'may', 'many', 'now', 'two', 'also', 'per', 'several'))
LdaDtmParish2 <- DocumentTermMatrix(ParishCorpus2)
lda_model_2 <- LDA(LdaDtmParish2, k = 5, method = "Gibbs")

#View the top 15 terms for each topic.
top_terms_2 <- terms(lda_model_2, 15)

#Compare the results to the first k=5 topic model:
top_terms_5
top_terms_2

# Discuss the results with your table. From a human perspective, did removing extra words improve the topic modelling analysis?


# 3. Word Relationships =================================
## 3.1. Explore association with a series of topics -----------
# Print the terms associated with a keyword by correlation coefficient:
# (A correlation coefficient shows the strength of the relationship between two items on a scale of 0 to 1)
findAssocs(LdaDtmParish, "morningside", 0.25)
findAssocs(LdaDtmParish, "abbeyhill", .25)
findAssocs(LdaDtmParish, "newhaven", .25)

## 3.2. Comparing two complete lists of associations -------
# We want to select the highest associations of one term and the lowest associations of another
# Create a new object containing our results
AssociationSea<-data.frame(findAssocs(LdaDtmParish, "sea", .01))
AssociationSeaCleaned<- data.frame(Term=rownames(AssociationSea), ValueSea=AssociationSea[,1], Association= "Sea")

AssociationCity<-data.frame(findAssocs(LdaDtmParish, "city", .01))
AssociationCityCleaned<- data.frame(Term=rownames(AssociationCity), ValueCity=AssociationCity[,1], Association="City")

### 3.3.1. Combining the data sets with the "merge" function (which we will cover tomorrow, so don't worry about understanding how it works for now!)
Merged_datasets <- merge(AssociationSeaCleaned, AssociationCityCleaned, by.x = 'Term', by.y = 'Term') 

MoreSea<-subset(Merged_datasets,ValueSea>0.1 &ValueCity<0.1)
MoreSea

Merged_datasets$Comparison <- ifelse(Merged_datasets$ValueSea > 0.15 & Merged_datasets$ValueCity < 0.1, "Sea", 
                                     ifelse(Merged_datasets$ValueCity> 0.15 & Merged_datasets$ValueSea < 0.1, "City", 
                                            "Undefined"))

### 3.3.2. Subset only the words associated with the keywords "sea" or "city"
Extreme<-subset(Merged_datasets, Comparison!="Undefined")

# Now I need to have one single value for each
Extreme$Value<-ifelse(Extreme$Comparison == "Sea association", Extreme$ValueSea, Extreme$ValueCity)

### 3.3.3. Visualise our results ---------------
ggplot(Extreme, aes(y=Term, x=Value, colour=Comparison))+
  geom_point(size=5)+
  theme_bw()

# What can we see in the graph? 


# Exercise 2 ========
# So far, we worked on the parish dataset for Edinburgh. Let's have a look at the data for another area of Scotland. 
# With your table, repeat the process of subsetting the Parish data by an area of your choice.
# Then, repeat the exercises using this data.

#Wrap-up discussion:
#1. What are the pros and cons of the methods we have used in this block?
#2. What can text analysis tell us about datasets? What information do we not have? How could we find it?
