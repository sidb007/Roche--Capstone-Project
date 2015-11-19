import csv
import numpy as np

from sklearn.neighbors import KNeighborsClassifier

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
# Machine Learning using knn
#================================================================

#using knn
knn = KNeighborsClassifier()

#K-Fold cross validation. 10 folds.
predicted = cross_validation.cross_val_predict(knn, train,target, cv=10)
print "Knn Accuracy:"+str(metrics.accuracy_score(target, predicted))


