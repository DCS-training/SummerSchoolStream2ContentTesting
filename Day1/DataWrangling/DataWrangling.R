##########  DATA WRANGLING ################

# RESEARCH QUESTIONS #######
# 1.not a question per se but how can we make sure to get the data to a stage that can make them usable for the analysis 
# 2.a corollary is that we want our data to satisfy the FAIR principles so you should never edit your raw data and all the pre- processing that can happen via R is better. This will make sure that if someone else wants to reproduce your steps they will be able to do so

# 1. Getting Set up Importing Data ==============
## 1.1. Packages needed--------------
library(tidyverse)
library(data.table)


# We are going to start with fixing and setting up the unstructured dataset i.e. the statistical accounts of scotland


## 1.2. Import the data------------
# In order to work with your own data, you will need to import it into R

### 1.2.1 First let's work with the Unstructured data 
# Step 1: Prepare the Data set ########

# Set the path to the directory containing our text files
text_files_dir <- "Day1/DataWrangling/data/Accounts" #need to start from your home if you are unsure what your home is you can use getwd()

getwd()
# Create an empty data.table
text_files <- list.files(text_files_dir, pattern = "\\.txt$", full.names = TRUE)

Scotdata <- data.table(title = character(), text = character())

# Iterate through each text file. We do this by using a forloop function

for (file in text_files) {
  # Specify the encoding (e.g., "latin1")
  text <- tolower(iconv(readLines(file, warn = FALSE), from = "latin1", to = "UTF-8", sub = ""))
  title <- gsub(".txt$", "", basename(file))
  Scotdata <- rbindlist(list(Scotdata, data.table(title = title, text = paste(text, collapse = " "))))
}

# Save the raw data frame as a CSV file. We do that for future reference 
write.csv(Scotdata, "Day1/DataWrangling/Export/text_data.csv", row.names = FALSE)


# Step 2: Doing some cleaning and wrangling #########
## 2.1 Fix some formatting issues ============
# Fix the going to the next line issue. i.e. sub "- " with nothing ""
# There are a lot of formatting errors (next line, next paragraph) that we want to clean up
ScotdataClean <- mutate_if(Scotdata, 
                           is.character, #apply the changes only if the data is a "character" type (e.g. text)
                           str_replace_all, 
                           pattern = "-[[:space:]]+","") #What I am searching for+ what I am subbing with 

## 2.2. Extract More info from the dataset ===============
# Extract area and parish from the title
ScotdataClean$Type<- sub(".*(P|C|G|A|F|M|I)\\.(.*?)\\..*", "\\1", ScotdataClean$title)#This is selecting the P|C|G|A|F|M|I
# P=Parish
# C=Miscellanea
# G=General Observations
# A=Appendix
# F=General
# I=Index
# M=Map
# I want to be able to subset the dataset by those and I also want to have them both as a code as a description to do so I need to write a if else clause
ScotdataClean$TypeDescriptive<- ifelse(
  ScotdataClean$Type =="P", "Parish",ifelse(
    ScotdataClean$Type =="C","Miscellanea", ifelse(
      ScotdataClean$Type =="G","General Observations", ifelse(
        ScotdataClean$Type =="A", "Appendix", ifelse(
          ScotdataClean$Type =="F","General", ifelse(
            ScotdataClean$Type =="I", "Index","Map"))))))

# I want the first bit of the title as the RecordId of the document
ScotdataClean$RecordID<- sub("^(StAS\\.\\d+\\.\\d+\\.\\d+).*","\\1",  ScotdataClean$title)

# I also want to extract the area that is the bit after p/c/g/a/f/m/i
ScotdataClean$Area<- sub(".*(P|C|G|A|F|M|I)\\.(.*?)\\..*", "\\2", ScotdataClean$title)# //2 cause I want to select the second bit so after the letters

# Extract the Parish. I can do so by extracting the last bit up until the full stop
ScotdataClean$Parish<- sub(".*\\.", "", ScotdataClean$title)

# Extract dates from the text 
# I know all dates are cluster of 4 numbers starting with 1 (there are no recent dates)
ScotdataClean$Year<-sapply(str_extract_all(ScotdataClean$text, "\\b1[1-9]{3}\\b"), function(matches) {
  if (length(matches) > 0) {
    paste(matches, collapse = ", ")
  } else {
    NA
  }
})

# Extract tables form text 
# Extract all text between <table> and </table>
ScotdataClean$Tables <- str_match(ScotdataClean$text, "<table>(.*?)</table>")[, 2]


## 2.3 Further Clean the Dataset ===========

# We will now start to look at what is inside but before starting our analysis we want to work only on the parish observations since a lot of the other documents are part of indexes or summaries
Parish<-subset(ScotdataClean, Type =="P")


#Export Parishes

write_csv(Parish,"Day1/DataWrangling/Export/Parishes.csv")

#ok now that we have cleaned the unstructured data let's do some work on the structured dataset
# Create your dataframe
Distilleries<-read.csv("scotches.csv")

# Function to get the column names where the value is 1 or more, separated by comma
get_info <- function(row, column_names) {
  selected_columns <- column_names[row == 1]
  if (length(selected_columns) > 0) {
    return(paste(selected_columns, collapse = ", "))
  } else {
    return("None")
  }
}

# Specify the column names you want to consider
Colour <- names(Distilleries)[3:16]
Nose<-names(Distilleries)[16:28]
Body<-names(Distilleries)[29:36]
Palate<-names(Distilleries)[37:51]
Fin<-names(Distilleries)[52:70]
Area<-names(Distilleries)[77:85]
# Add a new column "colourcheck" which contains the names of specified columns where the value is 1, separated by comma
Distilleries$colour <- apply(Distilleries[, Colour], 1, get_info, column_names = Colour)
Distilleries$nose <- apply(Distilleries[, Nose], 1, get_info, column_names = Nose)
Distilleries$body <- apply(Distilleries[, Body], 1, get_info, column_names = Body)
Distilleries$palate <- apply(Distilleries[, Palate], 1, get_info, column_names = Palate)
Distilleries$fin <- apply(Distilleries[, Fin], 1, get_info, column_names = Fin)
Distilleries$area <- apply(Distilleries[, Area], 1, get_info, column_names = Area)

ScotDist<-Distilleries[,c(1:2,71:76,86:95)]
#Remove all the .1.2 from repeating column names
ScotDist <- mutate(ScotDist, across(where(is.character), ~gsub(".1|.2", "", .)))


#Export the data

write_csv(ScotDist, "ScotDistilleries.csv")

library(tidyverse)
library(readr)

library(reshape2)
library(stringr)#
library(fuzzyjoin)


scotch_review <- read_csv("scotch_review_manual_clean.csv")
Operating_whisky_distilleries <- read_csv("List_of_whisky_distilleries_in_Scotland_manual_clean.csv")


## Step 1 - tidy

summary(scotch_review)

unique(scotch_review$price)
scotch_df <- scotch_review %>% 
  mutate(category = as.factor(category))
summary(scotch_df)


# merge with operating distilleries if `name` in reviews matches distillery name 

unique(scotch_review$name)
unique(Operating_whisky_distilleries$Distillery)

df <- scotch_df %>% fuzzy_inner_join(Operating_whisky_distilleries,by=c("name" = "Distillery"),match_fun = str_detect) %>% select(-description)

df_2 <- df %>% group_by(name) %>%
  reframe(#mean_price = mean(price),
    #log_mean_price = log(mean_price),
    price = price,
    log_price = log(price),
    review.point = review.point,
    Owner = Owner,
    category = category,
    Distillery = Distillery, 
    Location = Location,
    Region = Region,
    Founded = Founded#,
    # mean_review.point = mean(review.point)
  ) %>%
  ungroup() %>%
  mutate(ABV = as.numeric(gsub(".*?(\\d+\\.?\\d*)%.*", "\\1", name)), # creating ABV varaible (alcohol strength) from whisky name.
         # category = as.factor(category),
         # Distillery = as.factor(Distillery),
         # Founded = as.factor(Founded),
         # Location = as.factor(Location),
         # Region = as.factor(Region),
         # n_distillery = count(Distillery)
         #log_price = log(price)
         # uncomment to include - but figured this stuff could be left for the attendees
  ) %>% 
  add_count(Distillery) %>%
  mutate(n_Distillery = n) %>%
  distinct() %>% # There are just 2 duplicates in the data, so simpler to remove as apposed to doing anything more sophisticated, as likely just human error.
  select(-n)
#filter(n_Distillery > 1) %>%
na.omit()


summary(duplicated(df_2))    

# Investigating ABV variables
df_2 %>% select(name, ABV) %>% 
  arrange(desc(ABV))
unique(df_2$ABV)
hist(df_2$ABV)

#overall summary view
summary(df_2)

write.csv(df_2, "Whisky_distillery_data.csv")  
