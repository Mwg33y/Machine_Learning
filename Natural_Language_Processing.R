#Natural Language Processing (NLP)

getwd()
setwd("C:/Users/Deka/Desktop/DataAnalysis22/A_I/Machine_Learning_Lessons/Codes_Datasets/Part_7_Natural_Language_Processing/Section_36_Natural_Language_Processing/R")


#import a tab delimited file where one column has a customer review, and
#the other column has a yes/no response. Avoiding a comma de-limited file
#because we are counting words and symbols from the reviews to put in 
#our bag of words arrays. Customers use commas when writing sentences.

# ignore quotes used in sentences by customers. 

#Also do not treat the contents as factors as we do in classification for 
#categorical responses. We want to analyse and record words and how many times
#they occur
dataset <- read.delim('Restaurant_Reviews.tsv', quote = '', stringsAsFactors = FALSE)


#Cleaning the texts in the customer comments

#install text mining package for NLP
install.packages('tm')
library(tm)

#combine comments entries into one text document for easier cleaning
corpus = VCorpus(VectorSource(dataset$Review))

#use "as.character(corpus[[i]])" to view text content of corpus instead of the meta data

#put all words in review comments in lower case
corpus = tm_map(corpus, content_transformer(tolower)) 


#review numbers written in the reviews by customers
corpus = tm_map(corpus, removeNumbers)

#remove punctuation from the customer reviews
corpus = tm_map(corpus, removePunctuation)

#remove words that don't help predict customer reviews eg. "this"
#use the stopword() function to keep mostly only adjectives and verbs

install.packages('SnowballC')
library(SnowballC)
corpus = tm_map(corpus, removeWords, stopwords())

#find root words of remaining words. Also reduces the final number of words in
#our customer comments "corpus" word array
corpus = tm_map(corpus, stemDocument)

#remove extra spaces in customer review sentences
corpus = tm_map(corpus, stripWhitespace)




#Create the bag of words model

#create a sparse matrix out the "corpus" array.
#sparse matrix has many zeros
dtm <- DocumentTermMatrix(corpus)

#update sparse matrix and keep 99.9% of the most frequent words.
#for future reference, for documents with more than 2000 words keep more than 99.9%
dtm = removeSparseTerms(dtm, 0.999)

#use random forest classification as our NLP algorithm.
#transform dtm to a data frame first

my_nlp = as.data.frame(as.matrix(dtm))#dtm needs to be the right matrix type

#the independent variable is the comments, the dependent variable is the liked column
#from the customer review
#add the dependent variable to the my_nlp dataframe
my_nlp$Liked = dataset$Liked


# Encoding the Liked column as factor
my_nlp$Liked = factor(my_nlp$Liked, levels = c(0, 1))

# Splitting my_nlp into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(my_nlp$Liked, SplitRatio = 0.8)
training_set = subset(my_nlp, split == TRUE)
test_set = subset(my_nlp, split == FALSE)



# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)

classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 1000)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)






















