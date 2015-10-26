library("plyr")
library("car")
library(MASS)
library(ROCR)

setwd("~/Capstone/data/shared folder")

#  m.data<-read.csv("merged.csv")
#  m2.data<-m.data[which(m.data$Status %in% c("Approval","Rejected")),]
#  write.csv(m2.data,"merged_vf.csv",row.names = F)

#m2.data<-read.csv("merged_vf.csv")


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
  
  m2.data$InvoiceTotal<-as.numeric(levels(m2.data$InvoiceTotal))[m2.data$InvoiceTotal]
  
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



#############################################################################
#       Classification Models
#############################################################################

agg.with.dep<-read.csv("agg_data.csv")

## Logistic Regression


# removing extra columns

data.model<-agg.with.dep[,-c(1,7,8)]

#data.model$status.bin<-as.numeric(levels(data.model$status.bin))[data.model$status.bin]
  

# Cross Validation 10 fold, k=10
k<-5

# ID defines each subset among k 
data.model$id <- sample(1:k, nrow(data.model), replace = TRUE)
list<-1:k

# to store the result of k fold validations
total<-data.frame(matrix(0,nrow=5,ncol=3))
colnames(total)<-c("TPR","FPR","AUC")

# Loop for 10 fold cross validation
for(i in 1:k){
  #total$cutoff.pred<-0
  train <- subset(data.model, id %in% list[-i])
  test <- subset(data.model, id %in% c(i))
  # Removing  id columns from train, test
  train<-train[-ncol(train)]
  test<-test[-ncol(test)]
  # Building model
   
  #step <- stepAIC(glm.model, direction="both")
#   step$anova
  glm.model<-glm(formula=status.bin ~ count + inv.mean + inv.std.dev + participants.mean + 
                   distinct.cc + Department.D._ + Department.I._ + Department.J._ + 
                   Department.N._ + Department.S._ + Department.U._ + 
                   Department.V._ + Department.W._ + Department.X._ + 
                   Department.Y._ + Department.B._,family=binomial(logit),data=train)
  
  # Using built model for prediction
  test$pred<-predict(glm.model, newdata=test, type="response")
  
  test<-test[-which(is.na(test$pred)),]
  
  
   my.pred<-prediction(test$pred,test$status.bin)
   perf <- performance(my.pred, measure = "tpr", x.measure = "fpr")

#  plot(perf, col=rainbow(10))

  # AUC
  auc.perf = performance(my.pred, measure = "auc")
  
  
  
#   # Accuracy Plot
#    acc.perf = performance(my.pred, measure = "acc",x.measure="cutoff")
#    plot(acc.perf)
  
   # TPR VS Cutoff
#    acc.tpr.cut = performance(my.pred, measure = "tpr",x.measure="cutoff")
#    plot(acc.tpr.cut)
#    abline(h= .8,v=.02)
   
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

