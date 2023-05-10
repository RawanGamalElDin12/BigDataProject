library(dplyr)
data1 <- read.csv("Clean_Dataset.csv")

#######################################################################################
# Calculate mean target variable by source city
means_by_sourceCity <- data1 %>% 
  group_by(data1$source_city) %>% 
  summarise(mean_price = mean(price))

means_by_sourceCity
# Replace category with mean target variable
data1$source_city <- sapply(data1$source_city, function(x) {
  means_by_sourceCity$mean_price[means_by_sourceCity$`data1$source_city` == x]
})

#######################################################################################
# Calculate mean target variable by destination city
means_by_DestCity <- data1 %>% 
  group_by(data1$destination_city) %>% 
  summarise(mean_price = mean(price))

means_by_DestCity
# Replace category with mean target variable
data1$destination_city <- sapply(data1$destination_city, function(x) {
  means_by_DestCity$mean_price[means_by_DestCity$`data1$destination_city` == x]
})

######################################################################################
# Calculate mean target variable by airline
means_by_airline <- data1 %>% 
  group_by(data1$airline) %>% 
  summarise(mean_price = mean(price))

means_by_airline
# Replace category with mean target variable
data1$airline <- sapply(data1$airline, function(x) {
  means_by_airline$mean_price[means_by_airline$`data1$airline` == x]
})
######################################################################################
# Calculate mean target variable by departure time
means_by_departure_time <- data1 %>% 
  group_by(data1$departure_time) %>% 
  summarise(mean_price = mean(price))

means_by_departure_time
# Replace category with mean target variable
data1$departure_time <- sapply(data1$departure_time, function(x) {
  means_by_departure_time$mean_price[means_by_departure_time$`data1$departure_time` == x]
})
########################################################################################
# Calculate mean target variable by stops
means_by_stops <- data1 %>% 
  group_by(data1$stops) %>% 
  summarise(mean_price = mean(price))

means_by_stops
# Replace category with mean target variable
data1$stops <- sapply(data1$stops, function(x) {
  means_by_stops$mean_price[means_by_stops$`data1$stops` == x]
})
########################################################################################
# Calculate mean target variable by arrival time
means_by_arrival_time <- data1 %>% 
  group_by(data1$arrival_time) %>% 
  summarise(mean_price = mean(price))

means_by_arrival_time
# Replace category with mean target variable
data1$arrival_time <- sapply(data1$arrival_time, function(x) {
  means_by_arrival_time$mean_price[means_by_arrival_time$`data1$arrival_time` == x]
})
########################################################################################
# Calculate mean target variable by class
means_by_class <- data1 %>% 
  group_by(data1$class) %>% 
  summarise(mean_price = mean(price))

#install.packages("caret")
means_by_class
# Replace category with mean target variable
data1$class <- sapply(data1$class, function(x) {
  means_by_class$mean_price[means_by_class$`data1$class` == x]
})

data1 <- data1[, -3]


data1
####################### min-max normalization function ####################### 

minMax <- function(y){
  scaled_column <- (y - min(y)) / (max(y) - min(y))
  return(scaled_column)
}

data1$source_city <- minMax(data1$source_city)
unique(data1$source_city)

data1$departure_time <- minMax(data1$departure_time)
unique(data1$departure_time)

data1$stops <- minMax(data1$stops)
unique(data1$stops)

data1$arrival_time <- minMax(data1$arrival_time)
unique(data1$arrival_time)

data1$destination_city <- minMax(data1$destination_city)
unique(data1$destination_city)

data1$class <- minMax(data1$class)
unique(data1$class)


data1$airline <- minMax(data1$airline)
unique(data1$airline)


data1$days_left <- minMax(data1$days_left)
unique(data1$days_left)


data1$duration <- minMax(data1$duration)
unique(data1$duration)

data1$price <- minMax(data1$price)
unique(data1$price)



##########################Pearson Correlation Coefficient map##############################################
data1 <- data1[, -1]
#install.packages("reshape2")
library(reshape2)

cor_df <- round(cor(data1), 2)

melted_cor <- melt(cor_df)

melted_cor


library(ggplot2)
ggplot(data = melted_cor, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4)

#################### linear regression model with all features #################

#install.packages("party")
library(party)
regressionData <- data1
ind<-sample(2,nrow(regressionData),prob=c(0.7,0.3),replace=TRUE)
train.regressionData <- regressionData[ind==1,]
test.regressionData <- regressionData[ind==2,]

#test.regressionData

lm_model <- lm(train.regressionData$price ~ ., data = train.regressionData)
predictions <- predict(lm_model, newdata = test.regressionData)
predictions

######### evaluation of regression model with accuracy=90.2% ###########

#install.packages("Metrics")
library(Metrics)
rmse <- rmse(predictions, test.regressionData$price)
rmse
mse = rmse*rmse
mse

summary(lm_model)
summary(lm_model)$r.squared

regression_accuracy = (summary(lm_model)$r.squared)*100
regression_accuracy


####################### reg model with best features with accuracy=89.26% ###################

lm_model2 <- lm(train.regressionData$price ~ train.regressionData$airline+train.regressionData$class+train.regressionData$stops+train.regressionData$duration, data = train.regressionData)
predictions <- predict(lm_model2, newdata = test.regressionData)
predictions

######### evaluation of regression model ###########

#install.packages("Metrics")
library(Metrics)
rmse <- rmse(predictions, test.regressionData$price)
rmse
mse = rmse*rmse
mse

summary(lm_model2)
summary(lm_model2)$r.squared

regression_accuracy = (summary(lm_model2)$r.squared)*100
regression_accuracy





















