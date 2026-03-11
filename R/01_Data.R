library(tidyverse)
library(skimr)
library(janitor)



##################  1. load the data ##################  
df <- read_csv("credit_scroring_training.csv") # permit to read csv file
df <- clean_names(df)

head(df) # top 5
dim(df) # row x columns
names(df) # names of variables 
str(df)  # type of variables
summary(df)
skim(df)

##################  2. basic cleaning ##################  

df <- df %>% select(-x1)
# rename target into a factor
df <- df %>% rename(default = serious_dlqin2yrs) #target variable
#df$default <- factor(df$default, levels = c(0, 1), labels = c("No", "Yes")) #target into a chr

table(df$default)
prop.table(table(df$default))
summary(df)

##################  3. missing values ##################  
sum(is.na(df))
colSums(is.na(df))

# fill missing values 
df$monthly_income[is.na(df$monthly_income)] <- median(df$monthly_income, na.rm = TRUE) #median
df$number_of_dependents[is.na(df$number_of_dependents)] <- median(df$number_of_dependents, na.rm = TRUE) #median

# check
colSums(is.na(df))
sum(is.na(df))

######################################################  
write.csv(df, "credit_scoring_clean.csv", row.names = FALSE)
