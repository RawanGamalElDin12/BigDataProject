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

Var1 <- data1$airline
Var2 <- data1$price

data1

library(ggplot2)
ggplot(data = data1, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4)

















