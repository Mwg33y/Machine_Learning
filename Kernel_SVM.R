# Kernel SVM
# To find the boundary line between categories of data point
# that cannot have a linear boundary line. The boundary is found
# by projecting the two dimension plot of the data points into the 
# third dimension. The points are projected onto a three dimensional curve
# in the third dimension. 

# These curves can be built using mathematical formulas
# such as the Gaussian Radial Basis function, the three dimensional 
# sigmoid function, or a a three dimensional polynomial function. Many other
# math formulas can be used. 

# A flat plane of best fit is used to approximately
# intersect all of the data points that have been projected onto the 3-d curve.
# an upper and lower plane, sandwiching the first plane,intersect the 3-D 
# curves as well. the space between the first plane and the two planes on either 
# side are representitive of the acceptable error, similar to the linear SVR.

# These 3-D intersection points of the sandwich of planes approximating the 
# data point projections in the third dimenion are projected back onto the second
# dimension. In the end we get a non-linear boundary between two dimensional data point 
# categories without expending processor power on our computer

getwd()
setwd("C:/Users/Abdul/Desktop/DataAnalysis22/A_I/Machine_Learning_Lessons/Codes_Datasets/Part_3_Classification/Section_17_Kernel_SVM/R")



# Track social network adds to users who did (blue region in the plot) or 
#didn't (salmon colour in the chart) purchase advertised SUV vehicle.


# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])#exclude the third column
test_set[-3] = scale(test_set[-3])

# Fitting Kernel SVM to the Training set
install.packages('e1071')
library(e1071)
classifier = svm(formula = Purchased~.,
                data = training_set,
                type = 'C-classification',
                kernel = 'radial'
                 )

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# use the Confusion Matrix to evaluate the test model
#an nxn matrix that compares the observed y values with the 
#predicted values
cm = table(test_set[, 3], y_pred)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Kernel SVM (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Kernel SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))