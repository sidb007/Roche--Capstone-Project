import csv
import numpy as np

from sklearn.linear_model import LogisticRegression

from sklearn import cross_validation
from sklearn import metrics

#================================================================
# Reading the wine data file
#================================================================

f = open('wine.csv', 'rb')
reader = csv.reader(f)
headers = reader.next()
x=[]
y=[]
data=[]

for row in reader:
    data.append(row)
    x.append(row[0:len(row)-1])
    y.append(row[len(row)-1])
f.close()



train=np.array(x).astype(np.float)
target=np.array(y)



#================================================================
# Machine Learning using logistic regression with l1 penalty
#================================================================


#using logistic regreesion with l1 penalty
l1_lr = LogisticRegression(penalty='l1')

#K-Fold cross validation. 10 folds.
predicted = cross_validation.cross_val_predict(l1_lr, train,target, cv=10)
print "Logistic Regression (L1 penalty) Accuracy:"+str(metrics.accuracy_score(target, predicted))


#================================================================
# Machine Learning using logistic regression with l2 penalty
#================================================================

#using logistic regreesion with l2 penalty
l2_lr = LogisticRegression(penalty='l2')

#K-Fold cross validation. 10 folds.
predicted = cross_validation.cross_val_predict(l2_lr, train,target, cv=10)
print "Logistic Regression (L2 penalty) Accuracy:"+str(metrics.accuracy_score(target, predicted))