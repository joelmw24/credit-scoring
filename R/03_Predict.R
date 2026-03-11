library(tidyverse)

df <- read_csv("credit_scoring_clean.csv")

##################  1. split, train and test ##################  

set.seed(123)

# Create random sample for train set
train_index <- sample(1:nrow(df), 0.7 * nrow(df))

train_data <- df[train_index, ]
test_data  <- df[-train_index, ]
dim(train_data)
dim(test_data)

##################  2. build and test the logistic regression model ##################  

model <- glm(default ~ ., data = train_data, family = binomial)
summary(model)

# predict probabilities on test set
test_data$predicted_probability <- predict(model, newdata = test_data, type = "response") # list of probabilities for each client
head(test_data$predicted_probability)  
summary(test_data$predicted_probability) #stats 

##################  3. threshold  ##################  

thresholds <- seq(0.01, 0.50, by = 0.01)

results <- data.frame()

for (t in thresholds) {
  
  pred <- ifelse(test_data$predicted_probability > t, 1, 0)
  cm <- table(pred, test_data$default)
  
  TN <- cm["0","0"]
  TP <- cm["1","1"]
  FN <- cm["0","1"]
  FP <- cm["1","0"]
  
  precision_t <- TP / (TP + FP)
  recall_t <- TP / (TP + FN)
  accuracy_t <- (TP + TN) / sum(cm)
  
  results <- rbind(results,
                   data.frame(
                     threshold = t,
                     precision = precision_t,
                     recall = recall_t,
                     accuracy = accuracy_t
                   ))
}

results
head(results, 10)
head( coef(model) ,10) #variable importance 

##################  4. plot  ##################  

top5 <- head(sort(abs(coef(model))[-1], decreasing = TRUE), 5) # remove intercept
top5_df <- data.frame( variable = names(top5), abs_coefficient = as.numeric(top5))

ggplot(top5_df, aes(x = reorder(variable, abs_coefficient), y = abs_coefficient)) +
  geom_col() +
  coord_flip() +
  xlab("Variables") +
  ylab("Absolute coefficient") +
  ggtitle("Top 5 variables ")

##################  5. predict probability for all clients##################  

df$predicted_probability <- predict(model, newdata = df, type = "response")
#head(df$predicted_probability)
summary(df$predicted_probability)

# rank clients by risk
df %>% arrange(desc(predicted_probability)) %>% 
  select(default, age, monthly_income, predicted_probability) %>% 
  head(10)

##################  6. create risk groups ##################  

df$risk_group <- cut(df$predicted_probability, breaks = c(0, 0.2, 0.5, 1),
                     labels = c("Low", "Medium", "High"),
                     include.lowest = TRUE)

table(df$risk_group)
prop.table(table(df$risk_group))

#plot
ggplot(df, aes(risk_group)) + geom_bar() + xlab("Risk group") + ylab("Count") + 
  ggtitle("Distribution of risk")

######################################################    
#write.csv(test_data, "credit_scoring_test_predictions.csv", row.names = FALSE)
#write.csv(df, "credit_scoring_all_predictions.csv", row.names = FALSE)
#write.csv(importance, "variable_importance.csv", row.names = FALSE)
#write.csv(results, "threshold_results.csv", row.names = FALSE)