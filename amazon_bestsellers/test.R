books <- read.csv('/Users/DavidxChien/Desktop/SCU/MSBA/23 Winter/Marketing Analytics/Group Project/bestsellers with categories.csv')

library(caTools)
library(caret)
library(ggplot2)

# preprocess the data
books$Fiction <- ifelse(books$Genre == "Fiction", 1, 0)

# define bestseller as rating of 4.5 or higher and at least 10,000 reviews
books$Bestseller <- ifelse(books$User.Rating >= 4.5 & books$Reviews >= 10000, 1, 0)

# split the data into training and testing sets
set.seed(123)
split <- sample.split(books$Bestseller, SplitRatio = 0.7)
train <- subset(books, split == TRUE)
test <- subset(books, split == FALSE)

# train a logistic regression model
model <- glm(Bestseller ~ User.Rating + Reviews + Price + Year + Fiction, data = train, family = "binomial")

# make predictions on the test set
predictions <- predict(model, newdata = test, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# evaluate the performance of the model
confusionMatrix(table(predicted_classes, test$Bestseller))


# plot variable importance
importance_df <- data.frame(varImp(model))
ggplot(importance_df, aes(x = reorder(rownames(importance_df), -Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  coord_flip() +
  labs(title = "Variable Importance for Bestseller Prediction Model", x = "", y = "Overall Importance")



# combine bestseller and non-bestseller data
model$Bestseller <- factor(model$Bestseller)
combined_data <- rbind(train, test)

# create box plots for user ratings and reviews
p1 <- ggplot(combined_data, aes(x = Bestseller, y = User.Rating, fill = Bestseller)) + 
  geom_boxplot() + 
  labs(x = "Bestseller", y = "User Rating") +
  ggtitle("Distribution of User Ratings for Bestsellers vs Non-Bestsellers")

p2 <- ggplot(combined_data, aes(x = Bestseller, y = Reviews, fill = Bestseller)) + 
  geom_boxplot() + 
  labs(x = "Bestseller", y = "Reviews") +
  ggtitle("Distribution of Reviews for Bestsellers vs Non-Bestsellers")

# display the plots
gridExtra::grid.arrange(p1, p2, ncol = 2)


# create scatter plots for user ratings, reviews, and price
p3 <- ggplot(model, aes(x = User.Rating, y = Price, color = Bestseller)) + 
  geom_point() + 
  labs(x = "User Rating", y = "Price") +
  ggtitle("Relationship Between User Rating and Price for Bestsellers vs Non-Bestsellers")

p4 <- ggplot(model, aes(x = Reviews, y = Price, color = Bestseller)) + 
  geom_point() + 
  labs(x = "Reviews", y = "Price") +
  ggtitle("Relationship Between Reviews and Price for Bestsellers vs Non-Bestsellers")

# display the plots
gridExtra::grid.arrange(p3, p4, ncol = 2)


