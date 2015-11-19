import csv
import numpy as np

from sklearn.tree import DecisionTreeClassifier

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
# Machine Learning using decision tree
#================================================================


#using decision tree classifier
nt = DecisionTreeClassifier(random_state=0)

#K-Fold cross validation. 10 folds.
predicted = cross_validation.cross_val_predict(nt, train,target, cv=10)
print "Decision Tree Accuracy:"+str(metrics.accuracy_score(target, predicted))