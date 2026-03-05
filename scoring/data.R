library(tidyverse)
library(skimr)
library(janitor)

df <- read_csv("GermanCredit.csv")
head(df) #first head
dim(df) # (line, columns)
names(df) #name of columns
str(df) # type of variables 

summary(df)
sum(is.na(df)) #missing values

df_credit <- table(df$credit_risk)

#Graph
hist(df$amount,main = "Allocation of the amount", xlab = "Amount")
barplot(df_credit, main = "Allocation of credit risk", xlab = "Class", ylab = "Numbers")

boxplot(amount ~ credit_risk, data = df, main = "Amount depending on the risk", xlab = "credit risk", ylab = "amount")
boxplot(duration ~ credit_risk, data = df, main = "Duration depending on the risk", xlab = "credit risk", ylab = "duration")
boxplot(number_credits ~ credit_risk, data = df, main = "Number credit depending on the risk", xlab = "credit risk", ylab = "number credit")

