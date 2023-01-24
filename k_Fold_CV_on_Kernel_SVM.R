
#K-fold Cross Validation for non-linearly separable data applied to Kernel PCA
#using k-Fold to select the best fitting model SVM for the data, and 

getwd()
setwd("C:/Users/Abdul/Desktop/DataAnalysis22/A_I/Machine_Learning_Lessons/Codes_Datasets/Part_3_Classification/Section_17_Kernel_SVM/R")



# Track social network ads to users who did (blue region in the plot) or 
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

#applying k-Fold Cross Validation to kernel SVM model
install.packages('caret')
library(caret)
folds = createFolds(training_set$Purchased, k=10)
cv = lapply(folds, function(x){
                              training_fold = training_set[-x,]
                              test_fold = training_set[x,]
                              classifier = svm(
                                              formula = Purchased ~ .,
                                              data = training_fold,
                                              type ='C-classification',
                                              kernel ='radial'
                                          )
                              y_pred = predict(classifier, newdata= test_fold[-3])
                              cm = table(test_fold[,3], y_pred)
                              #find accuracy using the confusion matrix values
                              accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
                              return(accuracy)
                    }
)

#take the mean of the accuracy values of each of the executed test sets from above
mean_accuracy <- mean(as.numeric(cv))

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'kFold Model Boosting for Kernel SVM (Training set)',
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
     main = 'kFold Model Boosting for Kernel SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'dodgerblue', 'salmon'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3'))