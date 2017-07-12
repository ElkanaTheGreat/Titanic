import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
import tensorflow as tf

df = pd.read_csv('train.csv')

yi = df['label']
df = df.drop('label',1)

labels=[]
for i in range(len(yi)):
    label = [0,0,0,0,0,0,0,0,0,0]
    label[yi[i]]= 1
    labels.append(label)
    
labels = np.array(labels)
df = df.as_matrix()

df_train, df_test,y_train, y_test = train_test_split(df,labels)
#df_train = tf.constant(df_train, dtype = tf.float32)
#df_test = tf.constant(df_test, dtype = tf.float32)
#y_train = tf.constant(y_train, dtype = tf.float32)
#y_test = tf.constant(y_test, dtype = tf.float32)


x = tf.placeholder(tf.float32, [None, 784])

W = tf.Variable(tf.zeros([784, 10]))
b = tf.Variable(tf.zeros([10]))

y = tf.nn.softmax(tf.matmul(x, W) + b)

y_ = tf.placeholder(tf.float32, [None, 10])

cross_entropy = tf.reduce_mean(-tf.reduce_sum(y_ * tf.log(y), reduction_indices=[1]))
optimizer = tf.train.GradientDescentOptimizer(0.003)
train_step = optimizer.minimize(cross_entropy)

correct_prediction = tf.equal(tf.argmax(y,1), tf.argmax(y_,1))
accuracy = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))

sess = tf.Session()

init = tf.global_variables_initializer()
sess.run(init)

def next_batch(num, data, labels):
    '''
    Return a total of `num` random samples and labels. 
    '''
    idx = np.arange(0 , len(data))
    np.random.shuffle(idx)
    idx = idx[:num]
    data_shuffle = [data[ i] for i in idx]
    labels_shuffle = [labels[ i] for i in idx]

    return np.asarray(data_shuffle), np.asarray(labels_shuffle)


for i in range(10000):
    df_train0, y_train0 = next_batch(100,df_train,y_train)
    sess.run(train_step, feed_dict={ x: df_train0, y_: y_train0})
    
print(sess.run(accuracy,feed_dict={x:df_test, y_:y_test}))
