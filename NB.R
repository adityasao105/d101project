library(rsample)
library(klaR)
# setwd("/Users/adityasao/data101/project2")
set.seed(1234)
training_csv <- read.csv("phone_train.csv")
prediction_csv <- read.csv("phone_test.csv")

split <- initial_split(training_csv, prop = 0.7)
training_data <- training(split)
testing_data<-testing(split)

training_data$price_range <- factor(training_data$price_range, levels=c(0, 1, 2, 3), labels=c("Low", "Medium", "High", "Luxury"))
testing_data$price_range <- factor(testing_data$price_range, levels=c(0, 1, 2, 3), labels=c("Low", "Medium", "High", "Luxury"))

nb_model<-NaiveBayes(price_range ~ ram + battery_power + px_height + px_width, data=training_data)
pred<-predict(nb_model,testing_data)

cm <- table(pred$class, testing_data$price_range)
accuracy <- sum(cm[1], cm[6], cm[11], cm[16]) / sum(cm[1:16])
accuracy

