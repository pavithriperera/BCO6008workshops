
library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)

#load data set
muffin_cupcake_dataset_original<-read.csv("https://raw.githubusercontent.com/adashofdata/muffin-cupcake/master/recipes_muffins_cupcakes.csv
")

#get a overview of data set 
muffin_cupcake_dataset_original%>%skim()


#clean variable names
muffin_cupcake<-muffin_cupcake_dataset_original%>%clean_names()

#count number of data points for each type
muffin_cupcake%>%count(type)

#spiting data, using clean data set. 
muffin_cupcake_split<-initial_split(muffin_cupcake)

#see training and testing proportions
muffin_cupcake_split

#save training and testing data set separately 
#save training data set
muffin_cupcake_training<-training(muffin_cupcake_split)

#save testing data set 
muffin_cupcake_testing<-testing(muffin_cupcake_split)

#create a recipe
muffin_recipe<-recipe(type~flour+milk+sugar+egg,data=muffin_cupcake_training)
muffin_recipe

#record steps in recipe
muffin_recipe_steps<-muffin_recipe%>%step_meanimpute(all_numeric())%>% 
  step_center(all_numeric())%>%
  step_scale(all_numeric())

#prep the recipe 
prepped_recipe<-prep(muffin_recipe_steps, training=muffin_cupcake_training)

#bake
#bake training set 
muffin_train_preprocessed<-bake(prepped_recipe,muffin_cupcake_training)

#bake testing set 
muffin_test_preprocessed<-bake(prepped_recipe,muffin_cupcake_testing)

