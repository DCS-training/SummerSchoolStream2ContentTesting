
# Geographical Data Visualisation

## 5.1 Where can we find more mentions of ilness related events =====
# Check for keywords and add them to the data dataset
Parish$Ilness<- ifelse(grepl("ill|ilness|sick|cholera", Parish$text, ignore.case = T), "yes","no")

# Group by Ilness and geographical area
IlnessGroup <- Parish %>%
  group_by(Area) %>%
  summarise(Total = n(), count = sum(Ilness == "yes")) %>%
  mutate(per = round(count/Total, 2))

# Read Geographical Data (geopackage)
ParishesGeo <- readOGR(dsn = here("Day5/Data/Spatial/Parishes.gpkg"))

PointsDistilleries<- readOGR(dsn =here("Day5/Data/Spatial/ScottishDistilleries.gpkg"))

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


## 5.3 Now with Booze ============

# Where can we find more mentions of witches related events
#Check for keywords and add them to the data data set

Parish$Booze<- ifelse(grepl("illicit still|illicit distillery|drunkness|intemperance|wisky|whisky|whiskey|whysky ",Parish$text, ignore.case = T), "yes","no")

# Group by witch and area
BoozeGroup <- Parish %>%
  group_by(Area) %>%
  summarise(Total = n(), count = sum(Booze == "yes")) %>%
  mutate(per = round(count / Total, 2))


#Merge the two dataset
MergedGeo3 <-merge(ParishesGeo,BoozeGroup, by.x="JOIN_NAME_", by.y="Area",all.x = TRUE)# nb this is left join cause I want to preserve all the records present in ParishGeo



# Create a continuous color palette
color.palette3 <- colorRampPalette(c("white", "Brown"))

# Plot using spplot

spplot(MergedGeo3,
       "per",
       col.regions = color.palette3(100),
       main = "Booze Reports",
       key.space = "right",
       sp.layout = list("sp.polygons", MergedGeo3, col = "black", pch = 19),
       scales = list(draw = TRUE))


#Add the second dataset 
spplot(MergedGeo3,
       "per", 
       col.regions = color.palette3(100),
       main = "Booze Reports",
       key.space = "right",
       sp.layout = list("sp.polygons",
                        PointsDistilleries,
                        col = "black", pch = 19, labels = PointsDistilleries$Name),
       scales = list(draw = TRUE))
