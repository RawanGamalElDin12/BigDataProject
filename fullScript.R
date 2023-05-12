dataset<-read.csv("Clean_Dataset.csv")
head(dataset)

par(mfrow=c(3,3))
#################
numberOfOccurence<-table(dataset$source_city)
barplot(numberOfOccurence,main = "number of occurence of source city",ylim = c(0,70000),col =" red")
###############
numberOfOccurencedest<-table(dataset$destination_city)
barplot(numberOfOccurencedest,main = "number of occurence of destination city",ylim = c(0,70000),col =" blue")
#############
classtable<-table(dataset$class)
barplot(classtable/10000 ,main="frequency of economy and business flights",xlab = " flight type", ylab = "number of reservations (x10000)",ylim = c(0,30),col=rainbow((6)))
############
airlineTable<-table(dataset$airline)
barplot(airlineTable/1000 , main="number of flights of each airline",ylim = c(0,150) ,xlab = "airline name", ylab = "number of flights(x1000)", col=rainbow(6))
table(dataset$airline)


#############
departuretimeTable<-table(dataset$departure_time)
barplot(departuretimeTable , main="Frequency of Departure Time",xlab = "Departure Time",ylab="Frequency", col=rainbow(6))

#############
ArrivaltimeTable<-table(dataset$arrival_time)
barplot(ArrivaltimeTable , main="Frequency of Arrival Time",xlab = "Arrival Time",ylab="Frequency", col=rainbow(6))

#############

Stops_table<-table(dataset$stops)
barplot(Stops_table , main="Frequency of Stops",xlab = "Stops",ylab="Frequency", col=rainbow(6))

#reset
par(mfrow = c(1, 1))




class_counts <- table(dataset$class)
colors <- c("#6495ED","#DE3163")
percentages <- prop.table(class_counts) * 100
# Create a pie chart for the class column
pie(class_counts, labels = paste0(names(class_counts), ": ", round(percentages, 1), "%"),col =colors,main = "Buisness vs Economy class flights")
#add legend
legend("topright", 
       legend = names(class_counts), 
       fill = colors, 
       cex = 1.5)



airline_counts <- table(dataset$airline)
airline_counts
colors <- c("#f5c48c","#fbeae2","#ef886d","#b2ddde","#d6cdd2","#64683c")
percentages <- prop.table(airline_counts) * 100

# Create a pie chart for the airline column
pie(airline_counts, labels = paste0(names(airline_counts), ": ", round(percentages, 1), "%"),col =colors, main="Airlines Percentages")
#add legend
legend("bottomleft", 
       legend = names(airline_counts), 
       fill = colors, 
       cex = 1.5)

##############################################
#we have 1561 unique flight
table(dataset$flight)
print(length(unique(dataset$flight)))



par(mfrow=c(1,3))

#boxplot of durations column
boxplot(dataset$duration, main="Duration boxplot", col="#a2d2ff")
max(dataset$duration)
min(dataset$duration)
median(dataset$duration)

#boxplot of days left
boxplot(dataset$days_left, main="Days Left boxplot", col="#a2d2ff")
max(dataset$days_left)
min(dataset$days_left)
median(dataset$days_left)

#boxplot of price
boxplot(dataset$price, main="Price boxplot", col="#a2d2ff")
max(dataset$price)
min(dataset$price)
median(dataset$price)

par(mfrow=c(1,1))


par(mfrow=c(1,2))
#Histogram of flight prices
hist(dataset$price, main="histogram of flight prices",xlab="Price")

#density plot of flight prices
plot(density(dataset$price), main="Density plot of Price", xlab="Price")
par(mfrow=c(1,1))


#################Target Variable Analysis######################3
# calculate the average price for each airline
avg_price <- aggregate(dataset$price, by = list(dataset$airline), FUN = mean)
avg_price
# create a barplot of average price per airline
barplot(avg_price$x, names.arg = avg_price$Group.1, 
        xlab = "Airline", ylab = "Average Price", 
        main = "Average Flight Price by Airline", col= rainbow(6))

###################################################################################
# create a bar plot of mean prices by airline and class
mean_price <- aggregate(price ~ airline + class, data = dataset, FUN = mean)
mean_price
colors <- c(rep("#00b4d8", times =2), rep("#ffc8dd", times = 6))
barplot(height = mean_price$price, 
        names.arg = paste(mean_price$airline, mean_price$class), 
        xlab = "Airline and Class", ylab = "Mean Price", 
        main = "Mean Flight Price by Airline and Ticket Class",
        col = colors)

legend("topright", legend = c("Buisness", "Economy"), fill = c("#00b4d8", "#ffc8dd"))

#################################################################
# calculate the average price for each class
avg_price <- aggregate(dataset$price, by = list(dataset$class), FUN = mean)
avg_price
# create a barplot of average price per airline
barplot(avg_price$x, names.arg = avg_price$Group.1, 
        xlab = "Class", ylab = "Average Price", 
        main = "Average Flight Price by Class", col= rainbow(6))

#####################################################

avg_prices <- aggregate(price ~ days_left, data = dataset, FUN = mean)
avg_prices
# Create a scatterplot of the average prices against the number of days left
plot(avg_prices$days_left, avg_prices$price, 
     xlab = "Days Left", ylab = "Average Price", 
     main = "Average Flight Price by Days Left", col="blue", pch =16)


######################################
avg_pricess <- aggregate(price ~ duration, data = dataset, FUN = mean)
avg_pricess
# Create a scatterplot of the average prices against the duration of the flight
plot(avg_pricess$duration, avg_pricess$price, 
     xlab = "Duration", ylab = "Average Price", 
     main = "Average Flight Price by Duration", col="blue", pch =16)


#############################################################
par(mfrow=c(1,2))
# calculate the average price for each departure time
avg_price <- aggregate(dataset$price, by = list(dataset$departure_time), FUN = mean)
avg_price
# create a barplot of average price per depart time
barplot(avg_price$x, names.arg = avg_price$Group.1, 
        xlab = "Departure Time", ylab = "Average Price", 
        main = "Average Flight Price by Departure Time", col="blue")


#############################
# calculate the average price for each arrival time
avg_price <- aggregate(dataset$price, by = list(dataset$arrival_time), FUN = mean)
avg_price
# create a barplot of average price per arrival time
barplot(avg_price$x, names.arg = avg_price$Group.1, 
        xlab = "Arrival Time", ylab = "Average Price", 
        main = "Average Flight Price by Arrival Time", col= "red")



boxplot(price ~ departure_time , data = dataset, 
        xlab = "Departure Time", ylab = "Price", 
        main = "Departure Time vs. Ticket Price", 
        col = "skyblue", border = "darkblue")

boxplot(price ~ arrival_time, data = dataset, 
        xlab = "Arrival Time", ylab = "Price", 
        main = "Arrival Time vs. Ticket Price", 
        col = "skyblue", border = "darkblue")


par(mfrow= c(1,1))
########################################################
par(mfrow=c(1,2))
# calculate the average price for each source city
avg_price <- aggregate(dataset$price, by = list(dataset$source_city), FUN = mean)
avg_price
# create a barplot of average price per Source City
barplot(avg_price$x, names.arg = avg_price$Group.1, 
        xlab = "Source City", ylab = "Average Price", 
        main = "Average Flight Price by source city", col="blue")

#############################
# calculate the average price for each Destination City
avg_price <- aggregate(dataset$price, by = list(dataset$destination_city), FUN = mean)
avg_price
# create a barplot of average price per Destination City
barplot(avg_price$x, names.arg = avg_price$Group.1, 
        xlab = "Destination City", ylab = "Average Price", 
        main = "Average Flight Price by Destination City", col= "red")



################################################

boxplot(price ~ source_city, data = dataset, 
        xlab = "Source City", ylab = "Price", 
        main = "Source City vs. Ticket Price", 
        col = "skyblue", border = "darkblue")

boxplot(price ~ destination_city , data = dataset, 
        xlab = "Departure City", ylab = "Price", 
        main = "Departure City vs. Ticket Price", 
        col = "skyblue", border = "darkblue")




par(mfrow= c(1,1))

#############################number of stops column visualization######################
par(mfrow= c(1,2))

df_economy <- subset(dataset, class == "Economy")
df_economy
df_buisness <-subset(dataset, class == "Business")

# Calculate the mean price for each number of stops
mean_prices_economy <- aggregate(price ~ stops, data = df_economy, mean)

mean_prices_buisness <- aggregate(price ~ stops, data= df_buisness,mean)
mean_prices_buisness
# Create barplots of number of stops vs. average price
barplot(mean_prices_economy$price, names.arg = mean_prices_economy$stops, 
        col = "skyblue", 
        main = "Average Price by Number of Stops for Economy Class Flights", 
        xlab = "Number of Stops", ylab = "Average Price")

barplot(mean_prices_buisness$price, names.arg = mean_prices_buisness$stops, 
        col = "pink", 
        main = "Average Price by Number of Stops for Buisness Class Flights", 
        xlab = "Number of Stops", ylab = "Average Price")

legend("topright", legend = c("Buisness", "Economy"), fill = c("pink", "skyblue"))

par(mfrow= c(1,1))


#####################Model############################################
library(dplyr)
data1 <- read.csv("Clean_Dataset.csv")

####delete id and flight columns########
data1 <- data1[, -3]
data1 <- data1[, -1]

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


####################### reg model with best features with accuracy=89.27% ###################

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



####################### reg model with best features with accuracy=88.29% ###################

lm_model3 <- lm(train.regressionData$price ~ train.regressionData$airline+train.regressionData$class, data = train.regressionData)
predictions <- predict(lm_model3, newdata = test.regressionData)
predictions

######### evaluation of regression model ###########

#install.packages("Metrics")
library(Metrics)
rmse <- rmse(predictions, test.regressionData$price)
rmse
mse = rmse*rmse
mse

summary(lm_model3)
summary(lm_model3)$r.squared

regression_accuracy = (summary(lm_model3)$r.squared)*100
regression_accuracy



















