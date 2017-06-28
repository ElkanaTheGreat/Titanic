import pandas as pd
from sklearn.pipeline import FeatureUnion
from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import train_test_split
from matplotlib import pyplot as plt
from sklearn.decomposition import PCA
from mpl_toolkits.mplot3d import Axes3D
from sklearn.preprocessing import normalize
from sklearn.svm import SVC
from sklearn.feature_selection import SelectKBest
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
plt.style.use('ggplot')

#load and clean data
train = pd.read_csv('train.csv')
y = train['Survived']
df1 = train.drop(['Survived','Name','Ticket','Cabin','Fare'],1)
df = df1.drop('PassengerId',1)
#df = pd.get_dummies(df, columns = ['Sex','Embarked','Parch','Pclass','SibSp'], drop_first = True)
df = pd.get_dummies(df, columns = ['Embarked','Sex'], drop_first = True)
df = df.fillna(df.mean())

#df['Age']=normalize(df.loc[:,'Age'].values.reshape(-1,1))
#raise SystemExit

#split data for later test
df_train, df_test, y_train, y_test = train_test_split(df, y)

#color code for later plot
#color = []
#for i in range(137):
#	if y_train.iloc[i]==0:
#		color.append('r')
#	else:
#		color.append('b')


#pca for visualization (not needed for classification) 
#pca = PCA(n_components = 3, random_state = 42)
#selection = SelectKBest(k=1)
##combined_features = FeatureUnion([("pca", pca), ("univ_select", selection)])
#df_train = combined_features.fit(df_train, y_train).transform(df_train)
#df_test = combined_features.transform(df_test)

#plot a visualization(unfortunatly 3 dimensions is the max:(, blame God!)
#fig = plt.figure()
#ax = fig.add_subplot(111)
#ax = Axes3D(fig)
#ax.scatter(df_train[:,0],df_train[:,1],df_train[:,2], marker='.',c=color, alpha=0.3)
#plt.show()


#KNeighbors method for classification
knc = KNeighborsClassifier(n_neighbors=13)
knc1 = knc.fit(df_train, y_train)
print("KNC Score:")
print(knc1.score(df_test,y_test))

#Try svc classification
svc = SVC()
svc1 = svc.fit(df_train, y_train)
print("SVC Score:")
print(svc1.score(df_test,y_test))

#Try Decision tree
dt = DecisionTreeClassifier(criterion = 'entropy')
dt1 = dt.fit(df_train, y_train)
print("Decision Tree Score:")
print(dt1.score(df_test, y_test))

#Try Random Forest
rf = RandomForestClassifier(criterion = 'gini',min_samples_split=18, min_samples_leaf=18)
rf1 = rf.fit(df_train, y_train)
print("Random Forest Score:")
print(rf1.score(df_test, y_test))

#fit to whole train data set.
knc2 = knc.fit(df, y)
svc2 = svc.fit(df, y)
dt2 = dt.fit(df, y)
rf2 = rf.fit(df, y)

#clean testing dataset
dft = pd.read_csv('test.csv')
dft = dft.drop(['PassengerId','Name','Ticket','Cabin','Fare'],1)
#dft = pd.get_dummies(dft, columns = ['Sex','Embarked','Parch','Pclass','SibSp'],drop_first = True)
dft = pd.get_dummies(dft, columns = ['Embarked','Sex'], drop_first = True)
dft = dft.fillna(dft.mean())

#create csv file with results (SVC because it worked best on average).
results = pd.DataFrame(knc2.predict(dft))
results.to_csv('result.csv', header=['Survived'], index_label = 'PassengerId')
