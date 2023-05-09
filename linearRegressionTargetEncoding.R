library(dplyr)
data1 <- read.csv("Clean_Dataset.csv")


# Calculate mean target variable by source city
means_by_sourceCity <- data1 %>% 
  group_by(data1$source_city) %>% 
  summarise(mean_price = mean(price))

means_by_sourceCity
# Replace category with mean target variable
data1$source_city <- sapply(data1$source_city, function(x) {
  means_by_sourceCity$mean_price[means_by_sourceCity$`data1$source_city` == x]
})

data1