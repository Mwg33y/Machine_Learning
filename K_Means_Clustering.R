#K-Means Clustering

getwd()
setwd("C:/Users/Abdul/Desktop/DataAnalysis22/A_I/Machine_Learning_Lessons/Codes_Datasets/Part_4_Clustering/Section_24_K_Means_Clustering/R")

#Importing the mall dataset
dataset <- read.csv("mall.csv")

#We are going to group  mall patrons that have a client membership card 
#based on data collected by the mall operator. The client's genders, ages, 
#mall visits, and annual income are recorded. The owner of the mall has a 
#computed a spending score for each patron using the provided data. The 
#mall owner wants to separate the mall patrons into categories based on annual 
#income and spending score. We will use K-means clustering algorithm.

#client member income and spending score data only
x<-dataset[4:5]

#elbow method to find the optimal number of clusters
set.seed(6)#create reproducible results for variables that take random values
wcss <- vector()
for (i in 1:10) wcss[i] <-sum(kmeans(x, i)$withins)
plot(1:10, wcss, type = "b", main = paste('Clusters of clients'), xlab = 'Number of clusters',ylab='WCSS')

#Applying k-means to the dataset
set.seed(29)
kmeans <- kmeans(x, 5, iter.max = 300, nstart = 10)

#Visualize clusters
library(cluster)
clusplot(x, 
         kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotcher = FALSE,
         span = TRUE,
         main = paste('Cluster of clients'),
         xlab = 'Annual income',
         ylab = 'Spending score'
        )












