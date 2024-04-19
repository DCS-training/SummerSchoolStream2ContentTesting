
#This is some code to look at the notes within distilleries 

DistilleriesDataset<-read_csv("Day3/export/distilleries.csv")

# Split the words by comma and create a list column
DistSplit <- DistilleriesDataset %>%
  mutate(DistNotes = strsplit(DistNotes, ", "))

# Unnest the list column to create multiple rows
DistUnnest <- unnest(DistSplit, DistNotes)

DistUnnest$DistNotes<-as.factor(DistUnnest$DistNotes)


#remove all notes for which we have less than 5 recurrences

# Count the frequency of each factor level
factor_counts <- table(DistUnnest$DistNotes)

# Find factor levels with frequency >= 5
valid_factors <- names(factor_counts)[factor_counts >= 5]

# Filter the original dataset based on valid factor levels
Notesfiltered <- subset(DistUnnest,DistNotes%in% valid_factors)


# now we can plot it as boxplot 

ggplot(Notesfiltered, aes(x=DistNotes, y=DistRate, fill=DistNotes))+
  geom_boxplot() +
  coord_flip()+
  theme_bw()+ 
  theme(legend.position="none")+
  geom_hline(aes(yintercept = mean(DistRate)), color = "black", linetype = "dashed")+
  stat_summary(fun.y=mean, geom="point", shape=8, size=4)

#Let's look just the averages
RateByNote<-Notesfiltered %>%
  group_by(DistNotes)%>%
  summarise(mean=mean(DistRate))%>%
  arrange(desc(mean))

ggplot(RateByNote, aes(fill=DistNotes, y=mean, x=reorder(DistNotes, -mean))) + 
  geom_bar(position="dodge", stat="identity")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x = "Notes", y="Mean Rate")
  
#now we want to look by region

# we need to join it with the list of info 
DistilList<-read_csv("Day3/data/List_of_whisky_distilleries.csv")

MergedDist<-merge(DistilList, DistUnnest, by.x="Distillery", by.y="DistNames")

#subset again

#remove all notes for which we have less than 5 recurrences

# Count the frequency of each factor level
factor_counts <- table(MergedDist$DistNotes)

# Find factor levels with frequency >= 5
valid_factors <- names(factor_counts)[factor_counts >= 5]

# Filter the original dataset based on valid factor levels
Notesfiltered2 <- subset(MergedDist,DistNotes%in% valid_factors)

#now we facet by region

ggplot(Notesfiltered2, aes(x=DistNotes, y=DistRate, fill=DistNotes))+
  geom_boxplot() +
  facet_wrap(~Region)+
  coord_flip()+
  theme_bw()+ 
  theme(legend.position="none")+
  geom_hline(aes(yintercept = mean(DistRate)), color = "black", linetype = "dashed")+
  stat_summary(fun.y=mean, geom="point", shape=8, size=4)


#Let's look just the averages
RateByNote2<-Notesfiltered2 %>%
  group_by(DistNotes, Region)%>%
  summarise(mean=mean(DistRate))%>%
  arrange(desc(mean))

ggplot(RateByNote2, aes(fill=reorder(DistNotes, -mean), y=mean, x=Region)) + 
  geom_bar(position="dodge", stat="identity", colour="black")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(Fill = "Notes", y="Mean Rate")+
  coord_cartesian(ylim=c(75,95))+
  geom_hline(aes(yintercept = mean(mean)), color = "black", linetype = "dashed")
