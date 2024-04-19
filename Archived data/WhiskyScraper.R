
# Let's look there now 
#... open the link ...

# The first page
Homepage <- 'https://www.whiskybase.com/whiskies/distilleries?search=null&chr=null&country_id=191&region_id=&wbRanking='

  

# Unlike Python, the preferred approach to web scraping in R will be functional, not iterative. This means instead of for loops, we will write and map functions over our HTML.

# In RVest, the typical approach is to figure out the task we want to perform, write a function to perform this task, and then map the function over a list of objects
# Our first task is to get the links to each article on each of our pages. We can write a function for this, then then map it over the pages


# Read the pages ===================
get.article.links <- function(x){
  links <- read_html(x) %>%
    html_nodes('.clickable a') %>%
    html_attr('href')
}

ArticleLinks <- map(Homepage, get.article.links)

head(ArticleLinks)

ArticleLinks
length(ArticleLinks)



# What is wrong with this object?
# We get a list of lists, not a list. This will make it difficult to map over our variable, since each item has multiple items within it.

# So we need to use the flatten function to deal with this
ArticleLinksFlat <- ArticleLinks %>% 
  flatten()

head(ArticleLinksFlat)
length(ArticleLinksFlat)

Links <-unlist(ArticleLinksFlat)
# now we collect name Whiskies, Votes, Rating 

get.name <- function(x){
  body <- read_html(x) %>%
    html_nodes('.clickable a') %>%
    html_text()
}

Names <- map(Homepage, get.name)

head(Names)

Names
length(Names)

# So we need to use the flatten function to deal with this
NamesFlat <- Names%>% 
  flatten()

head(NamesFlat)
length(NamesFlat)

Distilleries<-unlist(NamesFlat)



# get Whiskies

get.whiskies <- function(x){
  body <- read_html(x) %>%
    html_nodes('td:nth-child(3)') %>%
    html_text()
}

whiskies <- map(Homepage, get.whiskies)

head(whiskies)

whiskies
length(whiskies)

# So we need to use the flatten function to deal with this
whiskiesFlat <-whiskies%>% 
  flatten()

head(whiskiesFlat)
length(whiskiesFlat)

NumberOfWhiskies <-unlist(whiskiesFlat)

# get Votes

get.votes <- function(x){
  body <- read_html(x) %>%
    html_nodes('td:nth-child(4)') %>%
    html_text()
}

votes <- map(Homepage, get.votes)

head(votes)

votes
length(votes)
  
  # So we need to use the flatten function to deal with this
  VotesFlat <-votes%>% 
    flatten()
  
  head(VotesFlat)
  length(VotesFlat)


  
NumberOfVotes <-unlist(VotesFlat)
  
  
# get Rating
  
  get.Rating <- function(x){
    body <- read_html(x) %>%
      html_nodes('td:nth-child(5)') %>%
      html_text()
  }
  
  Rating <- map(Homepage, get.Rating)
  
  head(Rating)
  
  Rating
  length(Rating)
  
  # So we need to use the flatten function to deal with this
  RatingFlat <-Rating%>% 
    flatten()
  
  head(RatingFlat)
  length(RatingFlat)
  
GeneralRating <-unlist(RatingFlat)

#Create a dataset

WhiskyDist<-as.data.frame(cbind(GeneralRating , 
                                NumberOfVotes,
                                NumberOfWhiskies,
                                Distilleries,
                                Links))


write_csv(WhiskyDist, "General_Distilleries.csv")


#Now we look inside each distilleries
DistilleriesList<-WhiskyDist$Links

#Letswork on a test first

Test<-head(DistilleriesList)

#get Name distillery We need this to link back 

get.name.dist <- function(x){
  body <- read_html(x) %>%
    html_node('h1') %>%
    html_text()
}

Namedist <- map(DistilleriesList, get.name.dist)#Test to try
head(Namedist)


# Initialize empty vectors to store the data
list_indices <- c()
sublist_indices <- c()
content <- c()

# Loop through the list and its sublists
for (i in seq_along(Namedist)) {
  for (j in seq_along(Namedist[[i]])) {
    list_indices <- c(list_indices, i)
    sublist_indices <- c(sublist_indices, j)
    content <- c(content, Namedist[[i]][[j]])
  }
}

# Combine the vectors into a dataframe
distilleries <- data.frame(List = list_indices, Sublist = sublist_indices, distName = content)

# get Name bottle

get.name.bottle <- function(x){
  body <- read_html(x) %>%
    html_nodes('.clickable') %>%
    html_text()
}

Namebottle <- map(DistilleriesList, get.name.bottle)

head(Namebottle)
Namebottle
UnlistedNamebottle<-unlist(Namebottle)



# Flatten the list of lists, retaining original indices
flattened_list <- unlist(Namebottle, recursive = FALSE)

# Get the indices for each element in the flattened list
indices <- unlist(lapply(seq_along(Namebottle), function(i) rep(i, length(Namebottle[[i]]))))

# Create a dataframe from the indices and flattened list
result <- data.frame(
  List = indices,
  Sublist = sequence(sapply(Namebottle, length)),
  Content = unlist(flattened_list)
)



# Create a function to extract indices and values from a sublist
extract_values <- function(index, sublist) {
  n <- length(sublist)
  if (n == 0) {
    return(list(index, NA, NA))
  } else {
    indices <- rep(index, n)
    return(Map(c, indices, seq_along(sublist), sublist))
  }
}

# Use Map to apply the function to each sublist
result <- unlist(Map(extract_values, seq_along(Namebottle), Namebottle), recursive = FALSE)

# Combine the result into a dataframe
final_dataset <- do.call(rbind, result)
final_dataset <- as.data.frame(final_dataset)
colnames(final_dataset) <- c("List", "Sublist", "Content")

# Print final_dataset
print(final_dataset)

# Print the result
print(result)



# Check for NULL values in the original list
null_indices <- which(sapply(my_list, is.null))
if (length(null_indices) > 0) {
  print(paste("NULL values found at indices:", null_indices))
}

# Check for empty sublists in the original list
empty_indices <- which(sapply(my_list, function(x) length(x) == 0))
if (length(empty_indices) > 0) {
  print(paste("Empty sublists found at indices:", empty_indices))
}


# Initialize empty vectors to store the data
list_indices <- c()
sublist_indices <- c()
content <- c()

# Loop through the list and its sublists
for (i in seq_along(Namebottle)) {
  for (j in seq_along(Namebottle[[i]])) {
    list_indices <- c(list_indices, i)
    sublist_indices <- c(sublist_indices, j)
    content <- c(content, Namebottle[[i]][[j]])
  }
}

# Combine the vectors into a dataframe
bottles <- data.frame(List = list_indices, Sublist = sublist_indices, bottlename = content)

#our total should be 120794 so let's remove duplicates
# Example dataset

# Identify duplicate rows based on the first two columns
duplicate_rows <- duplicated(bottles[, c("List", "Sublist")])

# Subset the original dataset to remove duplicate rows
bottlesClean <- bottles[!duplicate_rows, ]




#now we do a join to link the two dataset 

bottleAndDist<- merge(bottlesClean, distilleries, by = "List", all.x = TRUE)[,c("bottlename","distName")]


#Now we get the link of reviews

# Read the pages ===================
get.review.links <- function(x){
  links <- read_html(x) %>%
    html_nodes('.clickable') %>%
    html_attr('href')
}

LinkReview <- map(DistilleriesList, get.review.links)

head(LinkReview)

length(LinkReview)
# So we need to use the flatten function to deal with this
LinkReviewFlat <-LinkReview%>% 
  flatten()
length(LinkReviewFlat)

LinkReview <-unlist(LinkReviewFlat)

# Function to unlist and handle empty lists
unlist_with_null <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  } else {
    return(unlist(x))
  }
}

# Apply the function to each element of the list
LinkReview<- lapply(LinkReviewFlat, unlist_with_null)





# GetAge ===================
get.age <- function(x){
  links <- read_html(x) %>%
    html_nodes('td:nth-child(4)') %>%
    html_text
}

Age <- map(DistilleriesList, get.age)

head(Age)

length(Age)
# So we need to use the flatten function to deal with this
AgeFlat <-Age%>% 
  flatten()
length(AgeFlat)

#BottleAge<-unlist(AgeFlat)

# Function to unlist and handle empty lists
unlist_with_null <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  } else {
    return(unlist(x))
  }
}

# Apply the function to each element of the list
BottleAge <- lapply(AgeFlat, unlist_with_null)




# Strength ===================
get.strength <- function(x){
  links <- read_html(x) %>%
    html_nodes('td:nth-child(5)') %>%
    html_text
}

Strength <- map(DistilleriesList, get.strength)

head(Strength)

length(Strength)
# So we need to use the flatten function to deal with this
StrengthFlat <-Strength%>% 
  flatten()
length(StrengthFlat)

StrengthBottle<-unlist(StrengthFlat)

# YEar===================
get.year <- function(x){
  links <- read_html(x) %>%
    html_nodes('td:nth-child(6)') %>%
    html_text
}

Year <- map(DistilleriesList, get.year)

head(Year)

length(Year)
# So we need to use the flatten function to deal with this
YearFlat <-Year%>% 
  flatten()
length(YearFlat)

YearBottle<-unlist(YearFlat)


# Rating===================
get.rating <- function(x){
  links <- read_html(x) %>%
    html_nodes('td:nth-child(8)') %>%
    html_text
}

RatingB<- map(DistilleriesList, get.rating)

head(RatingB)

length(RatingB)
# So we need to use the flatten function to deal with this
RatingBFlat <-RatingB%>% 
  flatten()
length(RatingBFlat)

RatingBottle<-unlist(RatingBFlat)


#Let's get the reviews as well

test2<-LinkReview[1:5000]

# Rating===================
get.review <- function(x){
  # Use tryCatch to handle errors
  tryCatch({
    links <- read_html(x) %>%
      html_nodes('.wb--free-text') %>%
      html_text()
    return(links)
  }, error = function(e) {
    # If an error occurs (e.g., bad link), return NA
    return(NA)
  })
}


Review<- map(test2, get.review)

head(Review)

length(Review)
# So we need to use the flatten function to deal with this
RatingBFlat <-RatingB%>% 
  flatten()
length(RatingBFlat)

RatingBottle<-unlist(RatingBFlat)



# Combine the vectors into a dataframe
final_dataset <- data.frame(Distillery = bottleaAndDistdistillery, Bottle = bottles,
                            LinkReview = LinkReview, BottleAge=BottleAge,
                            StrengthBottle=StrengthBottle, YearBottle=YearBottle,
                            RatingBottle=RatingBottle)







# So we need to use the flatten function to deal with this
RatingFlat <-Rating%>% 
  flatten()

head(RatingFlat)
length(RatingFlat)

GeneralRating <-unlist(RatingFlat)



