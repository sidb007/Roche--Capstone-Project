library(nnet)

wine.data<-read.csv("wine.csv")


# normalize data for model convergence
norm.fn<-function(x){
  norm.data<-(x-min(x))/(max(x)-min(x))
  norm.data
}
norm.wine.data<-data.frame(apply(wine.data[,-ncol(wine.data)],MARGIN = 2,norm.fn))
wine.data<-cbind(norm.wine.data,type=wine.data$type)

# Cross Validation 10 fold, k=10
k<-10

# ID defines each subset among k 
wine.data$id <- sample(1:k, nrow(wine.data), replace = TRUE)
list<-1:k

# to store the result of k fold validations
total<-data.frame(matrix(0,nrow=0,ncol=2))

# Function for building Neural Network
neural.net<-function(train){
  # maxit is the number of iterations for training neural networks
  model<-nnet(as.factor(train$type)~.,data=train,size = 3,maxit=10000,trace=F)
  model
}
#################

# function plot.nnet for plotting Neural network

require(RCurl)

root.url<-'https://gist.githubusercontent.com/fawda123'
raw.fun<-paste(
  root.url,
  '5086859/raw/cc1544804d5027d82b70e74b83b3941cd2184354/nnet_plot_fun.r',
  sep='/'
)
script<-getURL(raw.fun, ssl.verifypeer = FALSE)
eval(parse(text = script))
rm('script','raw.fun')




#################

par(mar=numeric(4),mfrow=c(1,2),family='serif')

# Loop for 10 fold cross validation
for(i in 1:k){
  train <- subset(wine.data, id %in% list[-i])
  test <- subset(wine.data, id %in% c(i))
  # Removing  id columns from train, test
  train<-train[-ncol(train)]
  test<-test[-ncol(test)]
  # Building model
  nn.model<-neural.net(train)
  # Using built model for prediction
  test$pred<-predict(nn.model, newdata=test, type="class")
  # Appending actual and predicted results for each Cross validation step to data frame
  total<-rbind(total,data.frame(test$type,test$pred))

  #plotting Neural network with NID  
#   plot(nn.model,nid=F)
#   plot(nn.model)
  plot.nnet(nn.model,pos.col='darkgreen',neg.col='darkblue',alpha.val=0.7,rel.rsc=15,circle.cex=10,cex=1.4,circle.col='brown')

}

# function for calculating Accuracy, precision & Recall

prf <- function(predAct){
  preds = predAct[,1]
  trues = predAct[,2]
  xTab <- table(preds, trues)
  clss <- as.character(sort(unique(preds)))
  r <- matrix(NA, ncol = 5, nrow = 1, 
              dimnames = list(c(),c('Accuracy',paste("Precision",clss[1],sep='_'), 
                                    paste("Recall",clss[1],sep='_'),paste("Precision",clss[2],sep='_'), 
                                    paste("Recall",clss[2],sep='_'))))
  r[1,1] <- sum(xTab[1,1],xTab[2,2])/sum(xTab) # Accuracy
  r[1,2] <- xTab[1,1]/sum(xTab[,1]) # Miss Precision
  r[1,3] <- xTab[1,1]/sum(xTab[1,]) # Miss Recall
  r[1,4] <- xTab[2,2]/sum(xTab[,2]) # Hit Precision
  r[1,5] <- xTab[2,2]/sum(xTab[2,]) # Hit Recall
  r
}

prf(data.frame(total$test.type,total$test.pred))
