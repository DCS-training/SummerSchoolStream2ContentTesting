library(rgdal)
library(sp)
install.packages("tmap")
library(tmap)
library(ggmap)

getwd()
setwd("your own path")

Parish <- read_csv("data/Parishes.csv")
View(Parish)


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
ParishesGeo <- readOGR(dsn = "data/Spatial/Parishes.gpkg")

# Always a good habit to view your geospatial data first before visualising
## To view our data, you can use simple plot
plot(ParishesGeo, main = "Scottish Parishes")

## You can also change the basic format of the symbols
plot(ParishesGeo, col = "black", lwd = 1, border = "white", main = "Scottish Parishes")
plot(ParishesGeo, col = rgb(0.2, 0.6, 0.6), lwd = 1, border = "white", main = "Scottish Parishes")

# Merge the two dataset
MergedGeo <-merge(ParishesGeo,IlnessGroup, by.x="JOIN_NAME_", by.y="Area",all.x = TRUE) # nb this is left join cause I want to preserve all the records present in ParishGeo

# Check data to have merged properly(optional)
head(MergedGeo, max.level = 2)

# Create a continuous color palette
color.palette <- colorRampPalette(c("white", "red"))

# Plot using spplot
spplot(MergedGeo,
       "per",
       col.regions = color.palette(100),
       main = "Ilness Report",
       key.space = "right",
       scales = list(draw = TRUE))


# Let's try changing the colour of the filled regions using predifined colours
## There are predifined colour palettes you can use directly. Commonly used palettes include: rainbow(), heat.colors(), topo.colors(), and terrain.colors()
### Beware of the representation of colours. You might need to reverse the colour band to make the representations more intuitive
spplot(MergedGeo,
       "per",
       col.regions = rev(heat.colors(100)),
       main = "Ilness Report",
       key.space = "right",
       scales = list(draw = TRUE))

# You could also change the colour using RColorBrewer
library(RColorBrewer)
display.brewer.all()
color.palette <- brewer.pal(n = 9, name = "YlOrRd")

# Replot using the new palette
spplot(MergedGeo, 
       "per", 
       cuts = 8,
       col.regions = color.palette,
       main = "Ilness Report",
       sp.layout = list("sp.polygons", MergedGeo, col = "black"),
       scales = list(draw = TRUE))


# Try creating your own colour range and replot the map

# Changing the spacing of the interval
# Use classInt to make custom cuts if you don't want evenly spaced cutoffs between colors
library(classInt)
breaks <- classIntervals(MergedGeo$per, n = 5, style = "equal")
breakpoints <- breaks$brks
spplot(MergedGeo, 
       "per", 
       col.regions = color.palette(100), 
       colorkey = list(at = breakpoints)
       )

# style could also be adjusted based on quantile and at different 'cuts'. Try adjusting these values and explore the effects

## 5.2 Now with Witches ============

# Where can we find more mentions of witches related events
# Check for keywords and add them to the data data set

Parish$witches<- ifelse(grepl("witch|spell|witches|enchantemt|magic", Parish$text, ignore.case = T), "yes","no")

# Group by witch and area
WitchGroup <- Parish %>%
  group_by(Area) %>%
  summarise(Total = n(), count = sum(witches == "yes")) %>%
  mutate(per = round(count / Total, 2))


#Merge the two dataset
MergedGeo2 <-merge(ParishesGeo,WitchGroup, by.x="JOIN_NAME_", by.y="Area",all.x = TRUE)# nb this is left join cause I want to preserve all the records present in ParishGeo

# Create a continuous color palette
color.palette2 <- colorRampPalette(c("white", "purple"), alpha = 0.5)

# Plot using spplot
spplot(MergedGeo2,
       "per",
       col.regions = color.palette2(100),
       main = "Witches Reports",
       key.space = "top",
       scales = list(draw = TRUE))


# Adding scale bar and north arrow
library(maptools)

## Check dimensions of the boundary box to help guide where to put the labels
MergedGeo2@bbox 

## Code for scale bar (you could adjust size of text just like how you can adjust the size of symbols)
scalebar1 <- list("SpatialPolygonsRescale", layout.scale.bar(), scale = 100000, fill = c("transparent", "black"), offset = c(50000, 1100000))

text1 <- list("sp.text", c(50000, 1120000), "0km", cex = 0.7)
text2 <- list("sp.text", c(150000, 1120000), "25km", cex = 0.7)

## Code for north arrow
arrow <- list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(50000, 1150000), scale = 45000)

## Code for adding scalebar and north arrow to the map
spplot(MergedGeo2,
       "per", 
       col.regions = color.palette2(100),
       sp.layout = list(scalebar1, text1, text2, arrow),
       scales = list(draw = TRUE))

## 5.3 Now with Booze ============

# Load geospatial information for the location of distilleries
PointsDistilleries<- readOGR(dsn = "data/Spatial/ScottishDistilleries.gpkg")

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
       scales = list(draw = TRUE))


# Add the second dataset 
spplot(MergedGeo3,
       "per", 
       col.regions = color.palette3(100),
       main = "Booze Reports",
       key.space = "right",
       sp.layout = list("sp.polygons",
                        PointsDistilleries,
                        col = "black", pch = 16, labels = PointsDistilleries$Name),
       scales = list(draw = TRUE))

# pch controls the symbology of point data. Here are the most common ones: 
## pch = 1: Circle (default); pch = 2: Triangle point up; pch = 3: Plus sign; pch = 4: Cross; pch = 5: Diamond; pch = 6: Square; pch = 7: Cross rotated 45 degrees; pch = 8: Asterisk; pch = 9: Circle filled; pch = 10: Triangle point up filled
### Try changing the symbology to a different style, you could also adjust the size using cex.
spplot(MergedGeo3,
       "per", 
       col.regions = color.palette3(100),
       main = "Booze Reports",
       key.space = "right",
       sp.layout = list("sp.polygons",
                        PointsDistilleries,
                        col = "black", pch = 16, labels = PointsDistilleries$Name, cex = 1),
       scales = list(draw = TRUE))

# Bespoke symbol for distillery locations
library(png)
iconfile1 <- download.file("https://www.iconarchive.com/download/i123361/pictogrammers/material/bottle-tonic.512.png", destfile = "icon1.png", mode = "wb")
icon1 <- readPNG('icon1.png')

spplot(MergedGeo3,
       "per", 
       col.regions = color.palette3(100),
       main = "Booze Reports",
       key.space = "right",
       sp.layout = list("sp.polygons",
                        PointsDistilleries,
                        col = "black", pch = "icon1", labels = PointsDistilleries$Name, cex = 2),
       scales = list(draw = TRUE))

# Try another symbol yourself that looks more like a wine bottle maybe!


# Try adding scale bar and north arrow and the distilleries yourself!
## Check dimensions of the boundary box to help guide where to put the labels
MergedGeo3@bbox 

## Code for scale bar (you could adjust size of text just like how you can adjust the size of symbols)
scalebar <- list("SpatialPolygonsRescale", layout.scale.bar(), scale = 100000, fill = c("transparent", "black"), offset = c(50000, 1100000))

text1 <- list("sp.text", c(50000, 1120000), "0km", cex = 0.7)
text2 <- list("sp.text", c(150000, 1120000), "25km", cex = 0.7)

## Code for north arrow
arrow <- list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(50000, 1150000), scale = 45000)

## To combine the PointsDistilleries layer and the map information, you will to create a separate dataframe and call it in spplot instead
geo3_layout <- list(c("sp.polygons",
                    PointsDistilleries,
                    col = "black", pch = 19, labels = PointsDistilleries$Name), scale, text1, text2, arrow)

## Code for adding everything to the map
spplot(MergedGeo3,
       "per", 
       col.regions = color.palette3(100),
       sp.layout = geo3_layout,
       scales = list(draw = TRUE)
       )
