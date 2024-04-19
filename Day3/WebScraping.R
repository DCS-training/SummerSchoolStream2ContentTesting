
library(tidyverse)
library(rvest)

# The firs thing to do is figuring out where are we starting from. In our case is a website dedicated to whiskys lovers that allows you to review and rate single bottle of whisky. Because we are interested only in the whisky coming from Scotland we are actually going to start from a page that collect all the Scottish distilleries. 
# Our website has a hierarchical structure 
# The first page
Homepage <- 'https://www.connosr.com/scotch-whisky'



# Unlike Python, the preferred approach to web scraping in R will be functional, not iterative. This means instead of for loops, we will write and map functions over our HTML.

# In RVest, the typical approach is to figure out the task we want to perform, write a function to perform this task, and then map the function over a list of objects
# Our first task is to get the links to each article on each of our pages. We can write a function for this, then then map it over the pages


# Read the pages ===================
get.article.links <- function(x){
  links <- read_html(x) %>%
    html_nodes('.name') %>%
    html_attr('href')
}

DistLinks <- map(Homepage, get.article.links)

head(DistLinks)

length(DistLinks)

# What is wrong with this object?
# We get a list of lists, not a list. This will make it difficult to map over our variable, since each item has multiple items within it.

# So we need to use the flatten function to deal with this
DistLinksFlat <- DistLinks %>% 
  flatten()

head(DistLinksFlat)
length(DistLinksFlat)

Links <-unlist(DistLinksFlat)
FullLinks<- paste0("https://www.connosr.com", Links)


#Get Names distilleries

get.dist.names <- function(x){
  links <- read_html(x) %>%
    html_nodes('.name') %>%
    html_text()
}

DistNames <- map(Homepage, get.dist.names)

head(DistNames)
length(DistNames)


# So we need to use the flatten function to deal with this
DistNamesFlat <- DistNames %>% 
  flatten()

head(DistNamesFlat)
length(DistNamesFlat)

DistNames <-unlist(DistNames)

head(DistNames)

#Remove al letters at the start 
# Remove initial letters followed by a space
cleanedNames <- sub("^\\w+\\s", "", DistNames)#using regex 


#Get Notes

get.notes <- function(x){
  links <- read_html(x) %>%
    html_nodes('.align-right') %>%
    html_text()
}

Notes <- map(Homepage, get.notes)

head(Notes)


# So we need to use the flatten function to deal with this
NotesFlat <- Notes %>% 
  flatten()

head(NotesFlat)
length(NotesFlat)

Notes<-unlist(NotesFlat)

head(Notes)

# Remove common tags: 
cleanedNotes <- sub("Common tags: ", "", Notes)
unique(cleanedNotes)

#Get RateD

get.rate.dist <- function(x){
  links <- read_html(x) %>%
    html_nodes('.not-small-phone') %>%
    html_text()
}

RateDist <- map(Homepage, get.rate.dist)

head(RateDist)



# So we need to use the flatten function to deal with this
RateDistFlat <- RateDist %>% 
  flatten()

head(RateDistFlat)
length(RateDistFlat)

RateDist<-unlist(RateDistFlat)

head(RateDist)


# Remove Average rating: 
cleanedRateDist <- sub("Average rating: ", "", RateDist)
cleanedRateDist <-as.numeric(cleanedRateDist)

#now we go one level down ####
#Get link from links

get.bottle.link <- function(x){
  links <- read_html(x) %>%
    html_nodes('.name') %>%
    html_attr('href')
}

BottleLinks <- map(FullLinks, get.bottle.link)

head(BottleLinks)



# So we need to use the flatten function to deal with this
BottleLinksFlat <- BottleLinks %>% 
  flatten()

head(BottleLinksFlat)
length(BottleLinksFlat)

BottleLinks <-unlist(BottleLinks)
FullLinksBottle<- paste0("https://www.connosr.com", BottleLinks)



#Get name from links

get.bottle.name <- function(x){
  links <- read_html(x) %>%
    html_nodes('.name') %>%
    html_text()
}

BottleName<- map(FullLinks, get.bottle.name )

head(BottleName)



# So we need to use the flatten function to deal with this
BottleNameFlat <- flatten(BottleName)

head(BottleNameFlat)
length(BottleNameFlat)

BottleNames <-unlist(BottleNameFlat)


# now we go one level down to the reviews 

# get reviews

get.bottle.reviews <- function(x){
  links <- read_html(x) %>%
    html_nodes('.simple-review-content p') %>%
    html_text()
}

BottleReview<- map(FullLinksBottle, get.bottle.reviews)





#Merge together all reviews of each bottle
# Merge all sublists within each list
merged_list <- lapply(BottleReview, unlist)

# Convert merged list to character vectors and concatenate them into a single string
merged_strings <- sapply(merged_list, function(x) paste(x, collapse = " "))

# Create a dataframe with one column named "review"
ReviewByBottle <- data.frame(review = merged_strings)




# Create a dataframe with the original vector and its index
WithBottleNames <- data.frame(BottleName = BottleNames, review= ReviewByBottle$review)





#get brand

get.bottle.dist <- function(x){
  links <- read_html(x) %>%
    html_node('.data a') %>%
    html_text()
}

BottleDist<- map(FullLinksBottle, get.bottle.dist)

head(BottleDist)


# So we need to use the flatten function to deal with this
BottleDistFlat <- BottleDist %>% 
  flatten()

head(BottleDistFlat)
length(BottleDistFlat)

BottleDistFlat<-unlist(BottleDistFlat)



#get ABV

get.bottle.abv <- function(x){
  links <- read_html(x) %>%
    html_node('abbr+ .data') %>%
    html_text()
}

BottleABV<- map(FullLinksBottle, get.bottle.abv)

head(BottleABV)


# So we need to use the flatten function to deal with this
BottleABVFlat <-BottleABV %>% 
  flatten()

head(BottleABVFlat)
length(BottleABVFlat)

BottleABV <-unlist(BottleABVFlat)

# Remove percentage
cleanedBottleABV<- sub("%", "", BottleABV)
cleanedBottleABV<-as.numeric(cleanedBottleABV)


FullDataSet<- data.frame(AVB=cleanedBottleABV, Distillery= BottleDistFlat, BottleName=WithBottleNames$BottleName, Reviews= WithBottleNames$review)



# Remove rows where the Review column contains the specified strings
FullDataSetFiltered<-FullDataSet %>%
  filter(!grepl("There are no community reviews of|Review this whisky", Reviews)) %>%
  na.omit



# import the list of info about distilleries

List_of_whisky_distilleries <- read_csv("List_of_whisky_distilleries.csv")



WithDistInfo<- merge(List_of_whisky_distilleries, FullDataSetFiltered, by= "Distillery")

write_csv(WithDistInfo,"ReviewWithDistilleriesInfo.csv")
