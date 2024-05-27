remotes::install_github("quanteda/quanteda.sentiment", force=TRUE)

print(data_dictionary_geninqposneg, max_nval = 8)

# Load required packages
library(tm)

# Step 1: Read CSV file
data <- read_csv("Day3/export/data/ReviewWithDistilleriesInfo.csv")

# Step 2: Create a corpus
corpus <- Corpus(VectorSource(data$Reviews))

# Step 3: Add Metadata
metadata <- data.frame(
  Distillery = data$Distillery,
  Location = data$Location,
  Region = data$Region,
  Founded=data$Founded, 
  BottleName=data$BottleName,
  stringsAsFactors = FALSE
)

metadata$doc_ids<-rownames(metadata)

# Step 4: Combine Text and Metadata
doc_ids <- meta(corpus)$docid
metadata$doc_id <- doc_ids
rownames(metadata) <- doc_ids


# Create corpus with metadata
my_corpus <- corpus(data$Reviews, docnames = metadata$doc_ids)
docvars(my_corpus) <- metadata

# View the corpus with metadata
summary(my_corpus)


# Now you have a data frame 'metadata' containing metadata and a corpus 'corpus' containing text data.

library(quanteda)

Corpus<-corpus(whiskyReview1)

head(corpus) |>
  textstat_polarity(dictionary = data_dictionary_geninqposneg)

library(quanteda.sentiment)
1#Libraries needed
library(tidytext)
library(tidytext)
library(tidyverse)

affin<-get_sentiments("afinn")

get_sentiments("bing")

get_sentiments("nrc")

inaugural<-data_corpus_inaugural


# Import the dataset 
whiskyReview1<-read_csv("Day3/export/data/ReviewWithDistilleriesInfo.csv")
wiskyReview2<-read_csv("Day1/DataWrangling/Data/scotch_review_manual_clean.csv")



Reviews<-whiskyReview1S$Reviews
  
TidyReviews%>%
  inner_join(affin) %>%
  count(word, sort = TRUE)


TidyReviews <- whiskyReview1 %>%
  group_by(Region,Distillery,BottleName) %>%
  ungroup() %>%
  unnest_tokens(word, Reviews)

#Check the most recurring and 
AffinReview<-TidyReviews%>%
  inner_join(affin) %>%
  count(word, sort = TRUE)




Review_sentiment <- TidyReviews %>% # Load reviews dataset and start a chain of operations to create a Review_sentiment dataset
  inner_join(get_sentiments("bing"), relationship = 'many-to-many') %>% # Join the dataset with the Bing lexicon sentiment dictionary
  group_by(Distillery, Region, BottleName) %>% # Regroup the dataset by Distillery, Region, BottleName
  count(sentiment) %>%  # Count the occurrences of each sentiment within each segment
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% # Reshape the data from long to wide format
  mutate(sentiment = positive - negative)  # Calculate sentiment score (positive - negative) for each segment

Review_sentiment$labels <- sapply(str_split(Review_sentiment$BottleName, "\\s+"), function(x) paste(x[1:3], collapse = " ")) #Short the names keeping only the first 3 words



ggplot(Review_sentiment,aes(labels, sentiment, fill = Region)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Region, ncol = 3, scales = "free_x")+
  theme(axis.text.x = element_text(angle = 90)) 


#it is still very messy so why don't we build a graph for each region
Region <- unique( Review_sentiment$Region)
Region
for (y in Region) {# Iterate over each book in the 'books' vector y is the name we are going to the iteration variable it can be anything as long as you are consistent
  
  p <- # p is just a name we are giving to the plot again you can change it as long as you are consistent
    Review_sentiment %>%
    filter(Region == y) %>% # Filter the 'jane_austen_sentiment' dataset for the current book
    ggplot(aes(labels,sentiment, fill= Region)) +  # Create a ggplot object with chapter, index, and sentiment as aesthetics
    geom_col(show.legend = FALSE)+ 
    theme(axis.text.x = element_text(angle = 90))
    theme_bw()+ # Apply a black-and-white theme
    guides(fill="none")+  # Remove the fill legend
    ggtitle(y) # Add a title to the plot with the current book's name
  
  
  fp <- file.path("Day3/export/dir_out", paste0(y, ".png"))# Define the file path where the plot will be saved
  
  ggsave(plot = p,  # Save the ggplot object as a PNG file
         filename = fp,   # File path where the plot will be saved
         device = "png", # Output device type (PNG format)
         width=5000,# Width of the output in pixels
         height = 2000, # Height of the output in pixels
         units = "px") # Units of width and height (pixels)
  
}# Close the loop

library(magick)
imgs <- list.files("Day3/export/dir_out", full.names = TRUE) # List all file names in the directory 'dir_out' and store them in 'imgs'
img_list <- lapply(imgs, image_read)  # Read each image file from the list of file names using 'image_read' and store them in 'img_list'

img_joined <- image_join(img_list) # Join the list of images into a single animated image using 'image_join'

img_animated <- image_animate(img_joined, fps = 1) # Create an animated image from the joined image with a frame rate of 1 frame per second using 'image_animate'

image_write(image = img_animated,
            path = "Day3/export/distill.gif")

img_animated



