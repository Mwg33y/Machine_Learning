#Random Forest Regression

#Is the average of the predictions of many decision trees for a y value. This 
#gives a more accrate predicted value for y using the randmom independent 
#variables of the regression model

getwd()
setwd("C:/Users/Abdul/Desktop/DataAnalysis22/A_I/Machine_Learning_Lessons/Codes_Datasets/Part_2_Regression/Section_9_Random_Forest_Regression/R")


dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]#we will use a subset of the data


# No need to split the data set for this decision tree
#the data set is too small

# Feature Scaling not required for decision tree. Decision tree is based
# conditions of the independent variable.



# Fitting the random Forest Regression model to the dataset
install.packages('randomForest')
library(randomForest)
set.seed(1234)#use random data points from the dataset
regressor = randomForest(x = dataset[1],
                         y = dataset$Salary,
                         ntree = 500
                         )

# Predicting a new result 
y_predict = predict(regressor, data.frame(Level = 6.5))



# Visualising the Random Forest Regression Model results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.001)#0.001 increasesres solution
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor,
                                        newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Salary claims: True or False (Random Forest)') +
  xlab('Level') +
  ylab('Salary')



