

# Load the dplyr package
library(dplyr)

# Load your data into a data frame
dataset <- read.csv("Clean_Dataset.csv")


#encode departure time label using label encoding
dept_time <- as.numeric(factor(dataset$departure_time))

# Check the encoded column
dept_time

# Get the unique values and their labels
unique_depts_time <- unique(dataset$departure_time)
dept_labels <- levels(factor(dataset$departure_time))

# Print the name of the label and the number assigned to it
for (i in seq_along(unique_depts_time)) {
  cat(sprintf("%s: %d\n", dept_labels[i], i))
}

dataset$departure_time <- dept_time