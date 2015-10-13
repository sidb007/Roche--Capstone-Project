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

# Load the library for Random Forests

library(randomForest)

# Documentation for Random Forest function

?randomForest

# Create a random forest with the training data

forest <- randomForest(formula = type ~ ., data = train.data)

# Predict using the test data and add it to the test data set

pred.forest <- predict(object = forest, newdata = test.data)

test.data <- cbind(test.data, pred.forest)

# Determine which predictions were correct

correct.forest <- ifelse(test.data[,"type"] == test.data[,"pred.forest"], 1, 0)

test.data <- cbind(test.data, correct.forest)

# Print the results

print(test.data)

# Find out what percentage were correct

percent.correct.forest <- sum(test.data[, "correct.forest"])/nrow(test.data)

print(percent.correct.forest)
