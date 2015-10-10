# Load the rpart library

library(rpart)

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

# Documentation for the rpart function

?rpart

# Run the rpart function

tree <- rpart(formula = type ~ ., data = train.data)

# Get printed summary of the tree, including variables used for splits and how many observations fell into each branch

summary(tree)

# Load a special library that extends the plotting functions for rpart trees

library(rpart.plot)

# Plot the rpart tree

prp(tree)

# Predict on the test data using the decision tree

pred.tree <- predict(object = tree, newdata = test.data, type = "class")

# Add the predictions to the test data

test.data <- cbind(test.data, pred.tree)

# Generate a list of correct predictions and add it to the test data

correct.tree <- ifelse(test.data[,"type"] == test.data[,"pred.tree"], 1, 0)

test.data <- cbind(test.data, correct.tree)

# Print the results

print(test.data)

# Find the percentage correct

percent.correct.tree <- sum(test.data[,"correct.tree"])/nrow(test.data)

print(percent.correct.tree)