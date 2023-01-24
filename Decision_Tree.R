#Decision Tree Regression (non-continuous regression model)

#Decision trees use random independent variables to make a  3-D plot with 
#the dependent variable. The data values from the dependent variables are
#split into subgroups, and an average value is take for the y value of each subgroup. 
#This helps with better predicting a future y value as opposed to taking the 
#the average of all of the y values together.


getwd()
setwd("C:/Users/Abdul/Desktop/DataAnalysis22/A_I/Machine_Learning_Lessons/Codes_Datasets/Part_2_Regression/Section_8_Decision_Tree_Regression/R")

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

# No need to split the data set for this decision tree
#the data set is too small

# Feature Scaling not required for decision tree. Decision tree is based
# conditions of the independent variable.


# Fitting the Decision Tree Regression to the dataset
install.packages('rpart')
library(rpart)
regressor = rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))

# According to the data made available from the new hire's
#previous employer, his previous salary fall between levels 6 and 7.
#We will confirm if his previous salary is close enough to justify
#his new salary request 
#Here his justified salary is predicted  
y_pred = predict(regressor, data.frame(Level = 6.5))


# Visualizing the Decision Tree Regression results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.001)#higher resolution
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor,
                                        newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Salary claims : True or False (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')