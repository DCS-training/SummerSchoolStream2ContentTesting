#Text Analysis 

#libraries

## 1.1. Load required libraries ------------
install.packages("lexicon")
install.packages("wordcloud")
install.packages("textstem")
install.packages("quanteda.textmodels")
install.packages("quanteda.textplots")
install.packages("tidytext")
library(quanteda)
library(quanteda.textplots)
library(quanteda.textmodels)
library(lexicon)
library(tidyverse)
library (tm)


#Import data
Parish<-read_csv("Day2/data/Parishes.csv")

# 3. Extract Information about the Corpus ########
# Create a Quanteda corpus of the 'article text' column from our data set:
CorpusStat<-corpus(Parish$text)

# Some methods for extracting information about the corpus:
# Print doc in position 5 of the corpus
summary(CorpusStat, 5)
# Check how many docs are in the corpus
ndoc(CorpusStat) 
# Check number of characters in the first 10 documents of the corpus
nchar(CorpusStat[1:10]) 
# Check number of tokens in the first 10 documents
ntoken(CorpusStat[1:10]) 

# Create a new vector with tokens for all articles and store the vector as a new data frame with four columns (Ntoken, title, Area, Parish)
NtokenStats<-as.vector(ntoken(CorpusStat))
TokenScotland <-data.frame(Tokens=NtokenStats, title=Parish$title, Area=Parish$Area, Parish=Parish$Parish)

# Now we want to explore the parish data by geographic area. We can do that with pipes:
BreakoutScotland<- TokenScotland %>%
  group_by(Area)%>%
  summarize(NReports=n(), MeanTokens=round(mean(Tokens)))

# We can now plot the trends using gpplot:

ggplot(BreakoutScotland, aes(x=Area, y=NReports))+ # Select data set and coordinates we are going to plot
  geom_point(aes(size=MeanTokens, fill=MeanTokens),shape=21, stroke=1.5, alpha=0.9, colour="black")+ # Type of graph I want 
  labs(x = "Areas", y = "Number of Reports", fill = "Mean of Tokens", size="Mean of Tokens", title="Number of Reports and Tokens in the Scotland Archive")+ # Rename x/y labels and title
  scale_size_continuous(range = c(5, 15))+ # Resize the dots (enlarge)
  geom_text(aes(label=MeanTokens))+ # Add the mean of tokens in the dots
  scale_fill_viridis_c(option = "plasma")+ # Change the colour coding
  theme_bw()+ # B/W Background
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")+ # Rotate x-axis labels and move them slightly down; move legend to the bottom 
  guides(size = "none") # Remove the size from the legend 

# Discussion question: what is the graph telling us about our data?

# 4. Preparing the data for text analysis ############
# Now, we can tokenise the corpus, which will break the textual data into separate words grouped by document. We are also remove symbols, URLs, and punctuation.
Report_tokens <- quanteda::tokens(Parish$text, 
                                  remove_symbols=TRUE, 
                                  remove_url=TRUE, 
                                  remove_punct=TRUE,
                                  remove_numbers = FALSE,
                                  split_hyphens = TRUE)

# Take a look at our tokens list by printing the second document:
Report_tokens[2]

# Remove tokens under 3 characters. (Shorter words won't tell us much about our data, and because we removed punctuation, we want to get rid of the 
#truncated contractions--e.g. I'm -->'I', 'm')
Report_tokens <- tokens_select(Report_tokens, min_nchar = 3)

#Remove stop words
Report_tokens <-tokens_remove(Report_tokens, c(stopwords("english"), "statistical", "account", "parish", "one", "years"))


# 5 Visualise the Results ##################
# Convert to document-feature matrix (aka "dfm")
dfm_Report <- dfm(Report_tokens)

# Plot a word cloud
textplot_wordcloud(dfm_Report,
                   max_words=100,
                   color='black')

# Improving the word cloud
textplot_wordcloud(dfm_Report, rotation = 0.25,
                   max_words=50,
                   color = rev(RColorBrewer::brewer.pal(10, "Spectral")))#adding some colour and rotating results


# 8. Further Cleaning =====================
# Further steps for cleaning: stemming vs. lemmatization
## 8.1. Stemming ===========
nostop_toks <- tokens_select(Report_tokens, pattern = stopwords("en"), selection = "remove")
stem_toks <- tokens_wordstem(nostop_toks, language=quanteda_options('language_stemmer'))
stem_dfm <- dfm(stem_toks)
# Let's see the top features
topfeatures(stem_dfm, 30)

## 8.2. Lemmatization ================
lemmas <- tokens_replace(nostop_toks, pattern = lexicon::hash_lemmas$token, replacement = lexicon::hash_lemmas$lemma)
lemma_dfm <- dfm(lemmas)

topfeatures(stem_dfm, 20)
topfeatures(lemma_dfm, 20)

# Discussion: What can we observe about stemming and lemmatization? Which method (if any) do you prefer, and why?

# Make a word cloud of the lemmatized results:
textplot_wordcloud(lemma_dfm, rotation = 0.25,
                   max_words=50,
                   color = rev(RColorBrewer::brewer.pal(10, "Paired")))

# 9. Plot Frequency #########
# Plot the top 20 words (lemmatized) in another way:
top_keys <- topfeatures (lemma_dfm, 20)
data.frame(list(term = names(top_keys), frequency = unname(top_keys))) %>% # Create a data.frame for ggplot
  ggplot(aes(x = reorder(term,-frequency), y = frequency)) + # Plotting with ggplot2
  geom_point() +
  theme_bw() +
  labs(x = "Term", y = "Frequency") +
  theme(axis.text.x=element_text(angle=90, hjust=1))


# 6 Keywords in Context #######################
# keyword search examples (using kwic aka "keyword in context")
Witches<-kwic(Report_tokens , #on what
           c("witch", "spell", "enchantment","magic"), # Regex pattern
           valuetype = "regex",# use Regex to do so
           window = 10)


# Add them back to our main data set 
TokenScotland<-rownames_to_column(TokenScotland, var = "ID")

# Save row names as variables
TokenScotland$ID<-paste0("text",TokenScotland$ID)#add text to the ID
# Merge the two datasets
Merged <-merge(Witches,TokenScotland, by.x="docname", by.y="ID")
# Merge the text before and after the keyword
Merged$NewText<-paste0(Merged$pre, Merged$post)
# Now I want to see which area of Scotland contains the most keyword matches for 'witch,' 'spell,' 'enchantment,' or 'magic.' I can do this by using pipes again:
BreakoutWitches<- Merged %>%
  group_by(Area,pattern)%>%
  summarize(NMention=n())

# Now we can plot the trends. Again, I am using ggplot to do so.
ggplot(BreakoutWitches, aes(x=Area, y=NMention, colour=pattern))+ # Select data set and coordinates we are going to plot
  geom_point(aes(alpha=NMention), size=5)+ # Which graph I want 
  labs(x = "Areas", y = "Number of Keywords", colour = "Keyword matching", title="Witchcraft Keywords in the Scotland Archive")+ # Rename labs and title
  facet_wrap(~pattern, ncol=1)+
  theme_bw()+ # B/W Background
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")+ # Rotate labels of x and move them slightly down. Plus move the position to the bottom 
  guides(size = "none")+# Remove the Size from the Legend 
  scale_alpha(guide = 'none') #Remove alpha from legend


#Now let's do the same for keywords related to whisky:
Whisky<-kwic(Report_tokens , #on what
             c("drunk","intemperance","wisky|whisky|whiskey|whysky", "alembic", "spirit"), # Regex pattern
             valuetype = "regex",# use Regex to do so
             window = 30)


# Merge the two datasets
Merged2 <-merge(Whisky,TokenScotland, by.x="docname", by.y="ID")
# Merge the text before and after the keyword
Merged2$NewText<-paste(Merged2$pre,Merged2$keyword, Merged2$post)


# Now I want to see which area of Scotland in the dataset contains the most whisky keywords. I will use pipes to arrange the dataset by area:
BreakoutWhisky<- Merged2 %>%
  group_by(Area,pattern)%>%
  summarize(NMention=n())



# Now we can plot the trends. Again I am using ggplot to do so
ggplot(BreakoutWhisky, aes(x=Area, y=NMention, colour=pattern))+ # Select data set and coordinates we are going to plot
  geom_point(aes(alpha=NMention), size=5)+ # Which graph I want 
  labs(x = "Areas", y = "Number of Keywords", colour = "Keyword matching", title="Whisky Keywords in the Scotland Archive")+ # Rename labs and title
  facet_wrap(~pattern, ncol=1)+
  theme_bw()+ # B/W Background
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")+ # Rotate labels of x and move them slightly down. Plus move the position to the bottom 
  guides(size = "none")+# Remove the Size from the Legend 
  scale_alpha(guide = 'none') #Remove alpha from legend

#There are a lot of spirits let's have a check

spirits<-subset(Merged2, keyword=="spirit")

spirits$NewText

# There is a problem: a spirit can be booze but can also refer to the holy spirit or the human spirit!
# For now, let's just remove it from the pool 

#Now let's do the same on whisky
Whisky<-kwic(Report_tokens , #on what
             c("drunk","intemperance","wisky|whisky|whiskey|whysky", "alembic"), # Regex pattern
             valuetype = "regex",# use Regex to do so
             window = 30)


# Merge the two datasets
Merged3<-merge(Whisky,TokenScotland, by.x="docname", by.y="ID")
# Merge the before and after the keyword
Merged3$NewTextWithKeyword<-paste(Merged3$pre,Merged3$keyword, Merged3$post)
Merged3$NewText<-paste(Merged3$pre, Merged3$post)


# Now I want to see in which area of Scotland there are more instances of the keywords to see if anything has changed after removing "spirit." I can do this by using pipes again:
BreakoutWhisky<- Merged3 %>%
  group_by(Area,pattern)%>%
  summarize(NMention=n())


# Now we can plot the trends wiht ggplot:
ggplot(BreakoutWhisky, aes(x=Area, y=NMention, colour=pattern))+ # Select data set and coordinates we are going to plot
  geom_point(aes(alpha=NMention), size=5)+ # Which graph I want 
  labs(x = "Areas", y = "Number of Keywords", colour = "Keyword matching", title="Whisky Keywords in the Scotland Archive")+ # Rename labels and title
  facet_wrap(~pattern, ncol=1)+
  theme_bw()+ # B/W Background
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")+ # Rotate labels of x and move them slightly down. Plus move the position to the bottom 
  guides(size = "none")+# Remove the Size from the Legend 
  scale_alpha(guide = 'none') #Remove alpha from legend



# Next step is to look at what is around our keywords. We will start by creating a new dataframe:

KWIC_tokens <- quanteda::tokens(Merged3$NewText, 
                                  remove_symbols=TRUE, 
                                  remove_url=TRUE, 
                                  remove_punct=TRUE,
                                  remove_numbers = FALSE,
                                  split_hyphens = TRUE)



# Remove tokens under 3 characters. (Shorter words won't tell us much about our data, and because we removed punctuation, we want to get rid of the 
#truncated contractions--e.g. I'm -->'I', 'm')
KWIC_tokens <- tokens_select(KWIC_tokens, min_nchar = 3)

#Remove stop words
KWIC_tokens <-tokens_remove(KWIC_tokens, c(stopwords("english"), "statistical", "account", "parish", "one", "years"))

KWIC_dfm <- dfm(KWIC_tokens)


# Make a word cloud of the lemmatized results:
textplot_wordcloud(KWIC_dfm , rotation = 0.25,
                   max_words=50,
                   color = rev(RColorBrewer::brewer.pal(10, "Paired")))



#Wrap-up activity: practice & table discussion
#1. Subset the 'Parish' dataset by rows containing data about the Edinburgh area.(Hint: Edi_data <- subset(Parish, Area == 'Edinburgh'))

#2. Perform a keyword search on the Edinburgh data 'text' column for a keyword (or series of keywords) of your choice. (Don't forget to tokenise the text first!)

#3. Plot the keyword matches by 'Parish,' following the steps outlined in the lesson (performing the keyword search, merging the rows with a keyword match with the full dataset, using pipes to organise the data, and plotting with ggplot). 

#4. With your table, examine your plot. What does the visualisation show us about the topics related to your keyword(s) in the Edinburgh dataset? What do we still not know?

#5. Thinking about the text analysis methods we practiced today, can you think of an example (perhaps from your research) of a use case for one of these methods?

# Export our data for Friday visualisation ##########
write_csv(uk_data_clean, "Day5/data/TextDataVis.csv")



