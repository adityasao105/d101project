library(rpart)
library(rpart.plot)
library(rsample)
library(pheatmap)
library(ggplot2)

# set.seed(1234)
training_csv <- read.csv("phone_train.csv")
prediction_csv <- read.csv("phone_test.csv")
split <- initial_split(training_csv, prop = 0.7)
training_data <- training(split)
testing_data <- testing(split)

training_data$price_range <- factor(training_data$price_range, levels=c(0, 1, 2, 3), labels=c("Low", "Medium", "High", "Luxury"))
testing_data$price_range <- factor(testing_data$price_range, levels=c(0, 1, 2, 3), labels=c("Low", "Medium", "High", "Luxury"))

head(training_data)

# range = rep(2:100)
# accuracies <- c()
# for (ms in range) {
#   control <- rpart.control(cp=5e-4, minsplit=ms)
# # tree <- rpart(price_range ~ ., data=training_data, method="class")
#   tree <- rpart(price_range ~ ram + battery_power + px_height + px_width, data=training_data, method="class", control=control)
# # rpart.plot(tree)
#
#   prediction <- rpart.predict(tree, testing_data, type="class")
#   testing_data$prediction <- prediction
#
#   cm <- table(prediction, testing_data$price_range)
#   accuracy <- sum(cm[1], cm[6], cm[11], cm[16]) / sum(cm[1:16])
#   accuracies <- append(accuracies, accuracy)
# }
# plot(range, accuracies)

control <- rpart.control(cp=5e-4, minsplit=2)
# tree <- rpart(price_range ~ ., data=training_data, method="class")
tree <- rpart(price_range ~ ram + battery_power + px_height + px_width, data=training_data, method="class", control=control)
# rpart.plot(tree)

prediction <- rpart.predict(tree, testing_data, type="class")
testing_data$prediction <- prediction

cm <- table(prediction, testing_data$price_range)
accuracy <- sum(cm[1], cm[6], cm[11], cm[16]) / sum(cm[1:16])
accuracy
