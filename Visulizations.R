dataset <- read.csv("Clean_Dataset.csv")


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
legend("topright", 
       legend = names(airline_counts), 
       fill = colors, 
       cex = 1.5)
############################# airline frequency ############################# 
barplot(table(dataset$airline),xlab = 'Airlines', ylab = 'Counts')

#the following plot is with normalization: 
barplot(prop.table(table(dataset$airline)))

##############################################
#we have 1561 unique flight
table(dataset$flight)
print(length(unique(dataset$flight)))



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
boxplot(dataset$price, main="Days Left boxplot", col="#a2d2ff")
max(dataset$price)
min(dataset$price)
median(dataset$price)

#Histogram of flight prices
hist(dataset$price, main="histogram of flight prices",xlab="Price")

#density plot of flight prices
plot(density(dataset$price), main="Density plot of Price", xlab="Price")


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

##################################################################33
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
# calculate the average price for each departue time
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

################################################3

boxplot(price ~ destination_city , data = dataset, 
        xlab = "Departure City", ylab = "Price", 
        main = "Departure City vs. Ticket Price", 
        col = "skyblue", border = "darkblue")

boxplot(price ~ source_city, data = dataset, 
        xlab = "Source City", ylab = "Price", 
        main = "Source City vs. Ticket Price", 
        col = "skyblue", border = "darkblue")


par(mfrow= c(1,1))

########################################################

avg_price <- aggregate(dataset$price, by = list(dataset$stops), FUN = mean)
avg_price
# create a barplot of average price per number of stops
barplot(avg_price$x, names.arg = avg_price$Group.1, 
        xlab = "Destination City", ylab = "Average Price", 
        main = "Average Flight Price by Destination City", col= "red")

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

