#adapted from R documentation

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

# alternatively the traditional interface:

x <- subset(train.data, select = -type)

y <- train.data$type

model <- svm(x, y)

# test with train data
test<-test.data[,-c(ncol(test.data))]
pred <- predict(model, test)

# Check accuracy

table(pred, test.data$type)
