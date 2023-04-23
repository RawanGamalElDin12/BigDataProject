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

#we have 1561 unique flight
table(dataset$flight)
print(length(unique(dataset$flight)))


#Histogram of flight prices
hist(dataset$price, main="histogram of flight prices",xlab="Price")

df <- data.frame(income = c(50000, 60000, 70000, 80000),
                 education = c("High school", "College", "Graduate", "College"))

# Convert education variable to a factor
df$education <- factor(df$education)

# Create dummy variables using model.matrix()
dummy_vars <- model.matrix(~ education - 1, data=df)

# Print the dummy variable matrix
print(dummy_vars)
