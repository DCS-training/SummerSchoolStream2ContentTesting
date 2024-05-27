#Geographical Data vis

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

# Create a new vector with tokens for all articles and store the vector as a new data frame with three columns (Ntoken, title)
NtokenStats<-as.vector(ntoken(CorpusStat))
TokenScotland <-data.frame(Tokens=NtokenStats, title=Parish$title, Area=Parish$Area, Parish=Parish$Parish)

# Now we want to see how much material we have for each area. We can do that through pipes
BreakoutScotland<- TokenScotland %>%
  group_by(Area)%>%
  summarize(NReports=n(), MeanTokens=round(mean(Tokens)))

# Now we can plot the trends. This is done through the use of the ggplot package that is a very handy package that will allow you to print a very big variety of graphs

ggplot(BreakoutScotland, aes(x=Area, y=NReports))+ # Select data set and coordinates we are going to plot
  geom_point(aes(size=MeanTokens, fill=MeanTokens),shape=21, stroke=1.5, alpha=0.9, colour="black")+ # Which graph I want 
  labs(x = "Areas", y = "Number of Reports", fill = "Mean of Tokens", size="Mean of Tokens", title="Number of Reports and Tokens in the Scotland Archive")+ # Rename labs and title
  scale_size_continuous(range = c(5, 15))+ # Resize the dots to be bigger
  geom_text(aes(label=MeanTokens))+ # Add the mean of tokens in the dots
  scale_fill_viridis_c(option = "plasma")+ # Change the colour coding
  theme_bw()+ # B/W Background
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom")+ # Rotate labels of x and move them slightly down. Plus move the position to the bottom 
  guides(size = "none") # Remove the Size from the Legend 


# 4. Getting the data ready for text analysis ############
# Now, we can tokenise the corpus, which will break the textual data into separate words grouped by document. We are also removing symbols, URLs, and punctuation.
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


# 5 Working with Geographical Data #################

## 5.1 Where can we find more mentions of ilness related events =====
# Check for keywords and add them to the data dataset
Parish$Ilness<- ifelse(grepl("ill|ilness|sick|cholera", Parish$text, ignore.case = T), "yes","no")

# Group by Ilness and geographical area
IlnessGroup <- Parish %>%
  group_by(Area) %>%
  summarise(Total = n(), count = sum(Ilness == "yes")) %>%
  mutate(per = round(count/Total, 2))

# Read Geographical Data (geopackage)
ParishesGeo <- readOGR(dsn = here("Spatial/Parishes.gpkg"))

#Merge the two dataset
MergedGeo <-merge(ParishesGeo,IlnessGroup, by.x="JOIN_NAME_", by.y="Area",all.x = TRUE)# nb this is left join cause I want to preserve all the records present in ParishGeo

# Create a continuous color palette
color.palette <- colorRampPalette(c("white", "red"))

# Plot using spplot
spplot(MergedGeo,
       "per",
       col.regions = color.palette(100),
       main = "Ilness Report",
       key.space = "right",
       sp.layout = list("sp.polygons", MergedGeo, col = "black", pch = 19),
       scales = list(draw = TRUE))


## 5.2 Now with Witches ============

# Where can we find more mentions of witches related events
#Check for keywords and add them to the data data set

Parish$witches<- ifelse(grepl("witch|spell|witches|enchantemt|magic", Parish$text, ignore.case = T), "yes","no")

# Group by witch and area
WitchGroup <- Parish %>%
  group_by(Area) %>%
  summarise(Total = n(), count = sum(witches == "yes")) %>%
  mutate(per = round(count / Total, 2))


#Merge the two dataset
MergedGeo2 <-merge(ParishesGeo,WitchGroup, by.x="JOIN_NAME_", by.y="Area",all.x = TRUE)# nb this is left join cause I want to preserve all the records present in ParishGeo

# Create a continuous color palette
color.palette2 <- colorRampPalette(c("white", "purple"))

# Plot using spplot
spplot(MergedGeo2,
       "per",
       col.regions = color.palette2(100),
       main = "Witches Reports",
       key.space = "right",
       sp.layout = list("sp.polygons", MergedGeo, col = "black", pch = 19),
       scales = list(draw = TRUE))
