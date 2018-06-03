# -*- coding: utf-8 -*-
"""
Created on Wed May 16 01:09:45 2018

@author: abhil
"""
#Converting data to time series
from rpy2.robjects.packages import importr
import numpy as np
import rpy2.robjects as ro
import pandas as pd
utils = importr('utils')
forecast = importr('forecast')
utils.install_packages('forecast')
ro.r("data = read.csv('air_combined_creative_cusine.csv')")
ro.r("dats = ts(data$Logs_visitorsvisited, frequency = 7)")
ro.r("train = subset(dats, end = length(dats)-39, frequency=7 )")
ro.r("test = subset(dats, start= length(dats)-38, frequency=7 )")
y_train = np.asarray(ro.r["train"]).reshape((-1,1))
y_test = np.asarray(ro.r["test"]).reshape((-1,1))

#Reading the data
data = pd.read_csv("air_combined_creative_cusine.csv", encoding = "ISO-8859-1")
drop_columns =["visit_date","latitude","longitude","Avg_Temp"]
data = data.drop(drop_columns, axis=1)
X = data.drop(["Logs_visitorsvisited"], axis=1)
X = pd.get_dummies(X)
del X["day_of_week_Sunday"]
X_train, X_test, y_train, y_test = X[0:-39], X[-39:], y_train, y_test

#Normalization
from sklearn.preprocessing import StandardScaler
X_scaler = StandardScaler()
X_train = X_scaler.fit_transform(X_train)
X_test = X_scaler.transform(X_test)

#Regression
from sklearn import linear_model
model = linear_model.LinearRegression()
model.fit(X_train, y_train)
y_pred = model.predict(X_test)

#RSquared and RMSE
from sklearn.metrics import mean_squared_error
print(np.sqrt(mean_squared_error(y_test, y_pred)))
from sklearn.metrics import r2_score
print(r2_score(y_test, y_pred))


#Neural Network
from sklearn.neural_network import MLPRegressor
model = MLPRegressor((200,150,100), activation='tanh', solver='adam', batch_size=28, verbose=True, max_iter=1000)
model.fit(X_train, y_train)
y_pred = model.predict(X_test)

#RSquared and RMSE
print(np.sqrt(mean_squared_error(y_test, y_pred)))
print(r2_score(y_test, y_pred))

