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