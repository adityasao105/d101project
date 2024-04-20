library(rpart)
library(rpart.plot)
library(rsample)
library(pheatmap)
library(ggplot2)
setwd("/Users/adityasao/data101/project2")
set.seed(1234)
training_csv <- read.csv("phone_train.csv")
prediction_csv <- read.csv("phone_test.csv")
split <- initial_split(training_csv, prop = 0.7)
training_data <- training(split)
head(training_csv)

cor_mat <- cor(training_data)
pheatmap(cor_mat,fontsize_row = 10, fontsize_col = 10, main = "correlations")
#price_range correlates with ram,px_height,px_width,battery_power,int_memory,

training_csv$price_range <- factor(training_csv$price_range, levels = c(0,1,2,3),
                                      labels = c("l", "m", "h", "vh"))

training_csv$blue <- factor(training_csv$blue, levels = c(0,1),
                               labels = c("n", "y"))
training_csv$dual_sim <- factor(training_csv$dual_sim, levels = c(0,1),
                                   labels = c("n", "y"))
training_csv$four_g <- factor(training_csv$four_g, levels = c(0,1),
                                 labels = c("n", "y"))
training_csv$three_g <- factor(training_csv$three_g, levels = c(0,1),
                                  labels = c("n", "y"))
training_csv$touch_screen <- factor(training_csv$touch_screen, levels = c(0,1),
                                       labels = c("n", "y"))
training_csv$wifi <- factor(training_csv$wifi, levels = c(0,1),
                               labels = c("n", "y"))



ggplot(training_csv, aes(x=price_range, fill = blue)) +
  geom_bar(position = "dodge") #nothing
ggplot(training_csv, aes(x=price_range, fill = dual_sim)) +
  geom_bar(position = "dodge") #nothing
ggplot(training_csv, aes(x=price_range, fill = four_g)) +
  geom_bar(position = "dodge")#nothing
ggplot(training_csv, aes(x=price_range, fill = three_g)) +
  geom_bar(position = "dodge")#more 3g sold than not 3g in each section
ggplot(training_csv, aes(x=price_range, fill = touch_screen)) +
  geom_bar(position = "dodge")#nothing
ggplot(training_csv, aes(x=price_range, fill = wifi)) +
  geom_bar(position = "dodge")#nothing
ggplot(training_csv, aes(x=ram, y=n_cores,
                                 shape=price_range, color=price_range)) +
geom_point() +
labs(title = "Cores,Ram, and Price", x="Ram", y="Cores") # the higher the ram the higer price range
ggplot(training_csv,aes(x=price_range,y=battery_power))+geom_boxplot() #higher battery power higher price
ggplot(training_csv,aes(x=price_range,y=px_width))+geom_boxplot() #higher width higher price
ggplot(training_csv,aes(x=price_range,y=px_height))+geom_boxplot() #higher height higher price


mse<-c() #how do i get mse for catagorical values?
# for(CP in seq(from=0.00001, to=0.001,0.00001)){
# tree=rpart(price_range~three_g+ram+battery_power+px_width+px_height,data=training_data,method="class",control=rpart.control(cp=CP))
# pred<-predict(tree,training_data,type="class")
# 
# }






