#Dimensionality - Feature Extraction

#Linear Discriminant Analysis  (LDA) 

#makes it easier to visualize the data with one or two independent variables 
# in two dimension instead of many variables making it harder to visualize
#classification of categories of data



getwd()
setwd("C:/Users/Deka/Desktop/DataAnalysis22/A_I/Machine_Learning_Lessons/Codes_Datasets/Part_9_Dimensionality_Reduction/Section_44_Linear_Discriminant_Analysis/R")

#a wine retailer is trying to find which wines his customers like and plce the customers
#into categories according to their preferences. We will use the PCA to reduce the
#number of variables in the dataset to get a better visualization/idea of his customers'
#preferences. These categories will be used to predict future customer preferences


# Importing the dataset
dataset = read.csv('Wine.csv')


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])


#Applying LDA to the wine data
install.packages('e1071')#statistics functions
library(e1071)
install.packages('caret')#for classification and regression training
library(caret)
install.packages('MASS')#supports lda functions
library(MASS)

lda = lda(formula = Customer_Segment ~ ., data = training_set)

training_set_lda = as.data.frame(predict(lda, training_set))
test_set_lda = as.data.frame(predict(lda, test_set))


#only want linear discriminants and the dependent variable
training_set_lda = training_set_lda[c(5,6,1)]
test_set_lda = test_set_lda[c(5,6,1)]

# Fitting LDA model to the Training set using SVM
#dependent variable name changed to 'class' after applying lda function
classifier = svm(formula = class ~ .,
                 data = test_set_lda,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set_lda[-3])

# Making the Confusion Matrix
cm = table(test_set_lda[, 3], y_pred)

# Visualizing the Training set results
set = training_set_lda
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')

y_grid = predict(classifier,newdata = grid_set)
plot(set[, -3],
     main = 'LDA model using SVM (Training set)',
     xlab = 'x.LD1', ylab = 'x.LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)

points(grid_set,pch = '.',col=ifelse(y_grid ==2, 'green',ifelse(y_grid == 1, 'dodgerblue', 'salmon')))


points(set, pch = 21, bg = ifelse(set[,3]==2,'orange',ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3')))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set_lda
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
y_grid = predict(classifier,newdata = grid_set)
plot(set[, -3],
     main = 'LDA model using SVM (Test set)',
     xlab = 'x.LD1', ylab = 'x.LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set,pch = '.',col=ifelse(y_grid ==2, 'green',ifelse(y_grid == 1, 'dodgerblue', 'salmon')))


points(set, pch = 21, bg = ifelse(set[,3]==2,'orange',ifelse(set[, 3] == 1, 'dodgerblue3', 'salmon3')))

