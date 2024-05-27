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
