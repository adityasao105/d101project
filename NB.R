library(rpart)
library(rpart.plot)
library(rsample)
library(pheatmap)
library(ggplot2)
library(klaR)
# setwd("/Users/adityasao/data101/project2")
set.seed(1234)
training_csv <- read.csv("phone_train.csv")
prediction_csv <- read.csv("phone_test.csv")

split <- initial_split(training_csv, prop = 0.7)
training_data <- training(split)
testing_data<-testing(split)

training_data$price_range <- factor(training_data$price_range, levels=c(0, 1, 2, 3), labels=c("l", "m", "h", "vh"))
testing_data$price_range <- factor(testing_data$price_range, levels=c(0, 1, 2, 3), labels=c("l", "m", "h", "vh"))

nb_model<-NaiveBayes(price_range ~ ram + battery_power + px_height + px_width, data=training_data)
nb_model
pred<-predict(nb_model,testing_data)
testing_data$prediction<-pred
testing_data$prediction <- factor(testing_data$price_range, levels=c(0, 1, 2, 3), labels=c("l", "m", "h", "vh"))

errormatrix(testing_data$price_range,testing_data$prediction)

