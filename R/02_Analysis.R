library(tidyverse)
library(skimr)

df <- read_csv("credit_scoring_clean.csv")


##################  1.Graphs  ##################  

# default 
ggplot(df, aes(default)) + geom_bar() + xlab("Default") + ylab("Count") + 
  ggtitle("Default distribution")

# credit utilization 
ggplot(df, aes(revolving_utilization_of_unsecured_lines)) + geom_histogram(bins = 30) + 
  xlim(0, 1.5) + xlab("Credit utilization") + ylab("Count") + 
  ggtitle("Credit Utilization Distribution")

#age 
ggplot(df, aes(age)) + geom_histogram(bins = 30) + xlim(0, 110) + xlab("Age") +
  ylab("Count") +ggtitle("Age distribution")

# monthly income 
ggplot(df, aes(monthly_income)) + geom_histogram(bins = 10000) + 
  coord_cartesian(xlim = c(0, 20000)) + xlab("Monthly income") + ylab("Count") + 
  ggtitle("Monthly Income Distribution")

# debt ratio 
ggplot(df, aes(debt_ratio)) + geom_histogram(bins = 30) + xlim(0, 4) + xlab("Debt ratio") + 
  ylab("Count") + ggtitle("Debt ratio distribution")

#
ggplot(df, aes(number_of_times90days_late)) + geom_histogram(bins = 100) + 
  coord_cartesian(xlim = c(0, 5)) + xlab("Number of times 90 days") + ylab("Count") +
  ggtitle("90 Days Late Distribution")

##################  2. graphs with target  ################## 

# age 
ggplot(df, aes(default, age)) + geom_boxplot() + xlab("Default") + ylab("Age") + 
  ggtitle("Age by default")

# monthly income 
ggplot(df, aes(default, monthly_income)) + geom_boxplot() + coord_cartesian(ylim = c(0, 20000)) + 
  xlab("Default") + ylab("Monthly income") + ggtitle("Monthly income by default")

# debt ratio
ggplot(df, aes(default, debt_ratio)) + geom_boxplot() + coord_cartesian(ylim = c(0, 4)) + 
  xlab("Default") + ylab("Debt ratio") + ggtitle("Debt ratio by default")


##################  3. correlation  ################## 

cor(df$age, df$monthly_income)
cor(df$debt_ratio, df$revolving_utilization_of_unsecured_lines)
cor(df$monthly_income, df$debt_ratio)