library("plyr")
library("car")
library(MASS)
library(ROCR)
library(e1071)
library(randomForest)

m2.data<-read.csv("merged_all.csv")
  

## Script starts here



# Function to convert categorical feature to a set of Binary features
to.binary<-function(data.col,name){
  
  col.levels<-levels(data.col)
  colname<-paste(name,col.levels,"_")
  new.df<-data.frame(matrix(0,nrow=length(data.col),ncol=length(col.levels)))
  for(i in 1:length(col.levels)){
    new.df[which(data.col==col.levels[i]),i]=1
  }
  colnames(new.df)<-colname
  new.df
}

# Function accepts col.list which is the index of columns to be converted to binary.
which.to.binary<-function(data.1,col.list){
  
  new.df<-data.1
  for(i  in 1:length(col.list)){
    x<-to.binary(data.1[,col.list[i]],names(data.1)[col.list[i]])
    new.df<-cbind(new.df,x)
  }
  
  new.df<-new.df[,-col.list]
  new.df
}


#
#           Function to aggregate all columns
#

aggregation<-function(m2.data){
  
  # Number of Invoices
  
  #m2.data$InvoiceTotal<-as.numeric(levels(m2.data$InvoiceTotal))[m2.data$InvoiceTotal]
  
  num.inv<-ddply(m2.data,.(TENo),summarize,count=length(TENo),inv.mean=mean(InvoiceTotal),inv.std.dev=sd(InvoiceTotal))
  
  
  # Participant nu
  
  participants<-ddply(m2.data,.(TENo),summarize,participants.mean=mean(ParticipantsNo))
  
  #Number of AccountCode
  
  account<-ddply(m2.data,.(TENo,AccountCode),summarize,acc.count=length(AccountCode))
  
  acc2<-ddply(account,.(TENo),summarize,distinct.cc=length(acc.count))
  
  # Department
  
  dep<-ddply(m2.data,.(TENo,Department),summarize,count=length(Department))[,c(1,2)]
  
  # Lables
  
  status<-ddply(m2.data,.(TENo,Status),summarize,Count=length(Status))[,c(1,2)]
  
  # Joining 
  
  agg.df<-merge(num.inv,participants,by="TENo")
  agg.df2<-merge(agg.df,acc2,by="TENo")
  agg.df3<-merge(agg.df2,dep,by="TENo")
  agg.df4<-merge(agg.df3,status,by="TENo")
  agg.with.dep<-which.to.binary(agg.df4,7)
  
  agg.with.dep$status.bin<-recode(agg.with.dep$Status,"c('Approval')='0';else='1'")
  #agg.df4$status.bin<-recode(agg.df4$Status,"c('Approval')='0';else='1'")
  
  write.csv(agg.with.dep,"agg_data.csv",row.names = F)
  
    
  agg.with.dep
}

agg.with.dep<-aggregation(m2.data)
write.csv(agg.with.dep,"agg_data_all.csv",row.names = F)

#############################################################################
#       Classification Models
#############################################################################

agg.with.dep<-read.csv("agg_data_all.csv")

## Logistic Regression


# removing extra columns

data.model<-agg.with.dep[,-c(1,7,8)]

#data.model$status.bin<-as.numeric(levels(data.model$status.bin))[data.model$status.bin]
  

# Cross Validation 10 fold, k=10
k<-2

# ID defines each subset among k 
data.model$id <- sample(1:k, nrow(data.model), replace = TRUE)
list<-1:k
  
# to store the result of k fold validations
total<-data.frame(matrix(0,nrow=5,ncol=3))
colnames(total)<-c("TPR","FPR","AUC")

# Loop for k fold cross validation
for(i in 1:k){
  #total$cutoff.pred<-0
  train <- subset(data.model, id %in% list[-i])
  test <- subset(data.model, id %in% c(i))
  # Removing  id columns from train, test
  train<-train[-ncol(train)]
  test<-test[-ncol(test)]
  # Building model


  step <- stepAIC(glm.model, direction="both")
  #step$anova
  my.formula<-as.formula("status.bin ~ inv.mean + inv.std.dev + participants.mean + Department.AB._ + 
    Department.AC._ + Department.B._ + Department.C._ + Department.D._ + 
                         Department.E._ + Department.F._ + Department.GA._ + Department.I._ + 
                         Department.J._ + Department.JA._ + Department.K._ + Department.L._ + 
                         Department.M._ + Department.N._ + Department.Q._ + Department.R._ + 
                         Department.T._ + Department.U._ + Department.V._ + Department.Y._ + 
                         Department.YA._ + Department.Z._")
  
  #glm.model<-glm(formula=status.bin ~.,data=train)     
  glm.model<-glm(formula=my.formula,data=train)     

  
  nb.model<-naiveBayes(formula=my.formula,data=train)
  
  train$status.bin<-as.factor(train$status.bin)
  
  svm.formula<-as.formula("status.bin ~ count + inv.mean + inv.std.dev +
                          participants.mean + distinct.cc + Department.D._ +  
                          Department.V._ + Department.H._ + Department.Y._ + 
                          Department.R._ + Department.X._")
  
  #svm.model<-svm(formula=svm.formula,data=train,probability=T,kernal="radial basis")
  
  nn.model<-nnet(formula=my.formula,data=train,size = 7,maxit=10000,trace=T)
  
  rf.model<-randomForest(my.formula,data=train,ntree=500,mtry=3,importance=TRUE,na.action=na.omit,replace=FALSE)
  
  
  # Predict
  test<- test[complete.cases(test),]
  test$pred.glm<-predict(glm.model, newdata=test, type="response")
  test$pred.nb<-predict(nb.model,newdata = test[,-ncol(test)],type="raw")[,2]
  #temp<-predict(svm.model,test,probability =T)
  #test$pred.svm<-attributes(temp)$probabilities[,2]
  test$pred.nn<-predict(nn.model,newdata=test,type = "raw")
  test$pred.rf<-predict(rf.model,newdata=test,type = "prob")[,2]
  
  # Naive Bayes
  my.pred.nb<-prediction(test$pred.nb,test$status.bin)
  perf.nb <- performance(my.pred.nb, measure = "tpr", x.measure = "fpr")
  #plot(perf.nb)
  
  #test<-test[-which(is.na(test$pred)),]
  
  # GLM
  my.pred.glm<-prediction(test$pred.glm,test$status.bin)
  perf.glm <- performance(my.pred.glm, measure = "tpr", x.measure = "fpr")

  #plot(perf.glm)

  # RF
  
  my.pred.rf<-prediction(test$pred.rf,test$status.bin)
  perf.rf <- performance(my.pred.rf, measure = "tpr", x.measure = "fpr")
  
  # nn
  
  my.pred.nn<-prediction(test$pred.nn,test$status.bin)
  perf.nn <- performance(my.pred.nn, measure = "tpr", x.measure = "fpr")
  
  nb.df<-data.frame(perf.nb@x.values,perf.nb@y.values)
  names(nb.df)<-c("FPR","TPR")
  glm.df<-data.frame(perf.glm@x.values,perf.glm@y.values)
  names(glm.df)<-c("FPR","TPR")
  nn.df<-data.frame(perf.nn@x.values,perf.nn@y.values)
  names(nn.df)<-c("FPR","TPR")
  rf.df<-data.frame(perf.rf@x.values,perf.rf@y.values)
  names(rf.df)<-c("FPR","TPR")
  
  write.csv(nb.df,"TPR_NB1.csv",row.names = F)
  write.csv(glm.df,"TPR_GLM1.csv",row.names = F)
  write.csv(nn.df,"TPR_NN1.csv",row.names = F)
  write.csv(rf.df,"TPR_RF1.csv",row.names = F)
  
  
  plot(perf.nb, type="l", col="red" )
  par(new=TRUE)
  plot(perf.glm, type="l", col="green" )
  par(new=TRUE)
  plot(perf.rf, type="l", col="blue" )
  par(new=TRUE)
  plot(perf.nn, type="l", col="black" )
  abline(a=0,b=1,lty=8)
  
#   abline(a=.2,b=0,lty=8)
#   abline(a=.4,b=0,lty=8)
#   abline(a=.6,b=0,,lty=8)
#   abline(a=.8,b=0,,lty=8)
#   
#   abline(h= .8,v=.02,lty=8)
#   abline(a=.4,b=0,lty=8)
#   abline(a=.6,b=0,,lty=8)
#   abline(a=.8,b=0,,lty=8)
  
  legend('bottomright', c("NB","GLM","RF","NN") , lty=1, col=c('red', 'green','blue',"black"), bty='n', cex=.75)  
  
  
  # AUC
  auc.perf.nb = performance(my.pred.nb, measure = "auc")
  auc.perf.glm = performance(my.pred.glm, measure = "auc")
  auc.perf.nn = performance(my.pred.nn, measure = "auc")
  auc.perf.rf = performance(my.pred.rf, measure = "auc")
  
   # Accuracy Plot NB
    acc.perf.nb = performance(my.pred.nb, measure = "acc",x.measure="cutoff")
    #plot(acc.perf.nb)
  
    # Accuracy Plot GLM
    acc.perf.glm = performance(my.pred.glm, measure = "acc",x.measure="cutoff")
    # plot(acc.perf.glm)
    
    # Accuracy Plot NN
    acc.perf.nn = performance(my.pred.nn, measure = "acc",x.measure="cutoff")

    # Accuracy Plot RF
    acc.perf.rf = performance(my.pred.rf, measure = "acc",x.measure="cutoff")
    
    
    plot(acc.perf.nb, type="l", col="red" )
    par(new=TRUE)
    plot(acc.perf.nn, type="l", col="yellow" )
    par(new=TRUE)
    plot(acc.perf.rf, type="l", col="blue" )
    par(new=TRUE)
    plot(acc.perf.glm, type="l", col="green" )
    
    legend('bottomright', c("NB","GLM","NN") , lty=1, col=c('red', 'green','yellow'), bty='n', cex=.75)  
    
    
    # TPR VS Cutoff
    acc.tpr.cut.nb = performance(my.pred.nb, measure = "tpr",x.measure="cutoff")
    acc.tpr.cut.glm = performance(my.pred.glm, measure = "tpr",x.measure="cutoff")
    acc.tpr.cut.nn = performance(my.pred.nn, measure = "tpr",x.measure="cutoff")
    acc.tpr.cut.rf = performance(my.pred.rf, measure = "tpr",x.measure="cutoff")
    
    plot(acc.tpr.cut.nb, type="l", col="red" )
    par(new=TRUE)
    
    plot(acc.tpr.cut.glm, type="l", col="green" )
    par(new=TRUE)
    
    plot(acc.tpr.cut.nn, type="l", col="yellow" )
    par(new=TRUE)
    
    plot(acc.tpr.cut.rf, type="l", col="blue" )
    
    
    legend('topright', c("NB","GLM","NN") , lty=1, col=c('red', 'green',"yellow"), bty='n', cex=.75)  
    
    
    #plot(acc.tpr.cut.glm)
    abline(h= .8,v=.02)
   
  # Cut-Off
  cutoff<-.02
  
  # Prediction after cutoff
  test$pred.new<-0
  test$pred.new[which(test$pred>cutoff)]=1
  pred.cut<-prediction(test$pred.new,test$status.bin)
  perf.cut<-performance(pred.cut,measure="tpr",x.measure="fpr")
  
  # TPR
  tpr<-(unlist(pred.cut@tp)/max(unlist(pred.cut@tp)))[2]
  fpr <- (unlist(pred.cut@fp)/max(unlist(pred.cut@fp)))[2]
  
  total[i,1]<-tpr
  total[i,2]<-fpr
  total[i,3]<-unlist(auc.perf@y.values)
  #print (paste("TPR",tpr,"FPR",fpr,"AUC",auc.perf@y.values))
}

total



