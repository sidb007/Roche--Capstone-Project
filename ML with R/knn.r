library(class)

# Basic import function for csv files

import.csv <- function(filename){
  
  return(read.csv(filename,sep="," ,header=TRUE))
}

# Load the csv file and examine it

data <- import.csv('wine.csv') # Replace with your file name

print(data)

# Split the data into test and training sets

train.data <- data[1:300,] # Replace with your rows

test.data <- data[301:363,] # Replace with your rows


# perform the KNN analysis with k = 3

result <- knn(train.data, test.data, as.factor(train.data$type), k = 3)

# view the classes assigned to each of the test data points

result