#install packages 
install.packages("MASS")
install.packages("ISLR")

#install library 
library(tidymodels)
library(MASS)
library(ISLR)

set.seed(123)

#1-specify the model: type mode and engine
lm_spec<-linear_reg()%>%
  set_mode("regression")%>%
  set_engine("lm")

#view data set in MAAS
data(Boston)

#2- take model specification and apply to data (put data into the specify model): use fit()
lm_fit<-lm_spec%>%
  fit(data=Boston, medv~lstat)

lm_fit

#show how fit the model 
lm_fit%>%pluck("fit")%>% summary()

#can use this to see how fit the model
tidy(lm_fit)

#3- use fitted model from step2 to predict new y in new data
predict(lm_fit, new_data = Boston)

#examining new predicted values 
final_model<-augment(lm_fit, new_data = Boston)



rm(list=ls())

data(Boston)

#1
model_spec<-linear_reg()%>%
  set_mode("regression")%>%
  set_engine("lm")

model_spec

#2
model_fit<-model_spec%>% fit(data=Boston, medv~age+crim+rm)

#3
model_predict<-predict(model_fit, new_data = Boston)

model_predict_augment<-augment(model_fit, new_data = Boston)