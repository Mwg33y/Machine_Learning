
# SVR
#A newly hired employee is requesting a certain salary based on his 
#previous experience and position at his previous job.
#Predict the salary of a newly hired employee using data obtained from his
#previous employer

getwd()
setwd("C:/Users/Abdul/Desktop/DataAnalysis22/A_I/Machine_Learning_Lessons/Codes_Datasets/Part_2_Regression/Section_7_Support_Vector_Regression/R")


# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]#we will use a subset of the data

# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(123)#randomize the splitting of data 
split = sample.split(dataset$Salary, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
#scale the data in columns of each set for easier comparison and
#analysis 
training_set = scale(training_set)
test_set = scale(test_set)


# Support Vector Regression is an alternative to linear regression for modeling
# and predicting data 

# Fitting the SVR to the dataset

#install package to run support vector machine functions
install.packages('e1071')
library(e1071)

#define the support vector regressor
regressor = svm(formula = Salary ~ .,
                data = dataset,
                type = 'eps-regression'
)

# According to the data made available from the new hire's
#previous employer, his previous salary fall between levels 6 and 7.
#We will confirm if his previous salary is close enough to justify
#his new salary request 
#Here his justified salary is predicted 
y_pred = predict(regressor, data.frame(Level = 6.5))

# Visualizing the  SVR results
install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Salary claims: True or False (Support Vector Regression)') +
  xlab('Level: Experience & Position in Company') +
  ylab('Salary in Dollars')