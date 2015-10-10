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

# Load the library containing the Naive Bayes function

if (!require("e1071")){
  install.packages("e1071")
}else{
  require("e1071")
}



# Documentation for Naive Bayes function

?naiveBayes

# Run the Naive Bayes function

nb <- naiveBayes(formula = type ~ ., data = train.data)

# Predict using test data

pred.nb <- predict(object = nb, newdata = test.data, type = "class")

# Append prediction results onto the test data

test.data <- cbind(test.data, pred.nb)

# Add a new column showing if the prediction was correct

correct.nb <- ifelse(test.data[,"type"] == test.data[,"pred.nb"], 1, 0)

test.data <- cbind(test.data, correct.nb)

# Print the results

print(test.data)

# Find out what percentage were correct

percent.correct.nb <- sum(test.data[, "correct.nb"])/nrow(test.data)

print(percent.correct.nb)
