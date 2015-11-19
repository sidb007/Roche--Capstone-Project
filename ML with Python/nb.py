import csv
import numpy as np

from sklearn.naive_bayes import GaussianNB

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
# Machine Learning using naive bayes
#================================================================


#using naive bayes classifier
nb = GaussianNB()

#K-Fold cross validation. 10 folds.
predicted = cross_validation.cross_val_predict(nb, train,target, cv=10)
print "Naive Bayes Accuracy:"+str(metrics.accuracy_score(target, predicted))






