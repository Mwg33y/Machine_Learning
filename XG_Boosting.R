#XG_Boost



getwd()
setwd("C:/Users/Abdul/Desktop/DataAnalysis22/A_I/Machine_Learning_Lessons/Codes_Datasets/Part_10_Model_Selection_Boosting/Section 49 - XGBoost/R")


# Importing the dataset

#predict Which banking customers will leave the bank and which one's will stay
#using XGBoost trees classification

dataset = read.csv('Churn_Modelling.csv')
dataset = dataset[4:14]#only data that can help determine bank client loyalty

# Encoding the categorical variables as factors
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting XGBoost to the Training set
install.packages('xgboost')
library(xgboost)
#data parameter takes a matrix 
#returns Root Mean Squared Error; the lower the better the model
#target should be a value less than 0.30
classifier = xgboost(data = as.matrix(training_set[-11]), 
                     label = training_set$Exited, 
                     nrounds = 10)#max iterations



# Applying k-Fold Cross Validation
install.packages('caret')
library(caret)
folds = createFolds(dataset$Exited, k = 10)
cv = lapply(folds, function(x) {
  training_fold = dataset[-x, ]
  test_fold = dataset[x, ]
  classifier = xgboost(data = as.matrix(training_fold[-11]), label = training_fold$Exited, nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-11]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[, 11], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
#return the mean accuracy for all k modles executed
mean_accuracy = mean(as.numeric(cv))
