##Week 3 Assignment Project
#Bring in the Data
library(readr)
sms_spam_csv <- read_csv("Data Science School Documents/MSDS 680 Machine Learning/Week 3/sms_spam.csv.txt")
#View Data
View(sms_spam_csv)

#Change file name
sms <- sms_spam_csv

#Explore the Dataset
head(sms)
str(sms)
#Convert the type vector from a character type to a factor type
sms$type <- factor(sms$type)

# Now check out the data with the type variable now a factor type
str(sms)
table(sms$type)

# Install package tm and bring it into the analysis
install.packages("tm")
library(tm)

#Create a corpus to create a collection of the documents within the data.
sms_corpus <- VCorpus(VectorSource(sms$text))

# Then pring out the results of your newly built corpus
print(sms_corpus)
# The results state that there are 5574 documents within the corpus

#Use the inspect command to receive a summary of specific messages.
inspect(sms_corpus[1:2])

#To view the actual message text, the as.character() function will be applied to the desired messages
as.character(sms_corpus[[1]])

#To view multiple documents, the as.character() function will need to be used in conjunction with the lapply()
lapply(sms_corpus[1:2], as.character)

#To perform the analysis the messages will need to be divided out into individual words. To start I will use the tm_map() function to help change the letters to lowercase
sms_corpus_clean <- tm_map(sms_corpus,
      content_transformer(tolower))
#Check to make sure the command worked above:
as.character(sms_corpus[[1]]) #Old corpus
as.character(sms_corpus_clean[[1]]) #New Corpus

#Remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

#Next is removing stop words
sms_corpus_clean <- tm_map(sms_corpus_clean,
                           removeWords, stopwords())

#Remove punctuation
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

#Bring in the SnowballC package
install.packages("SnowballC")
library(SnowballC)

#Now were going to use the wordStem() function,
# This command returns all the words in a vector to their root form
#Since this is being done to an entire document, we are using the command stemDocument
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

#Next, clean out the white space of the text
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

#Lets take a look at the data now that we have done so much data prep
as.character(sms_corpus[1:3])
as.character(sms_corpus_clean[1:3])

#Creating a DTM Sparse Matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

#If the preprocessing beforehand wasn't done, we could have done so by providing a list of control
#parameter options to override the defaults (All in one command)
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation= TRUE,
  stemming = TRUE
))

#Let's look at sms_dtm & sms_dtm2
sms_dtm
sms_dtm2

#Creating the training and testing datasets
sms_dtm_train <- sms_dtm[1:4180, ]
sms_dtm_test <- sms_dtm[4181:5574, ]

# Now create the train and test labels. For this use the unclean versions of the data since the 
#clean ones lose their labels in the DTM.
sms_train_labels <- sms[1:4180, ]$type
sms_test_labels <- sms[4181:5574, ]$type

#To make sure the subset represents the complete set of SMS data, let's make a couple tables based on 
# the labels 
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

install.packages("wordcloud")
library(wordcloud)

#Create a wordcloud of the cleaned data.
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
# A look of it in random order
wordcloud(sms_corpus_clean, min.freq = 50, random.order = TRUE)

#Use the subset function to create a subset for spam and ham.
spam <- subset(sms, type == "spam")
ham <- subset(sms, type == "ham")

# Now compare the two subsets based on their wordclouds
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

#Find frequent terms 
findFreqTerms(sms_dtm_train, 5)

#Save the frequent words
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)

#Look into the contents of the frequent words
str(sms_freq_words)

#Using the DTM filter to include only the terms appearing in a specified vector for both sets
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

#Convert the counts to Yes/No Strings
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

#Specify between Rows and Columns
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
                   convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
                  convert_counts)

#Package e1071
install.packages("e1071")
library(e1071)

#Setting up the the model
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

#Now it is time to evaluate the performance of the model.
sms_test_pred <- predict(sms_classifier, sms_test)

install.packages("gmodels")
library(gmodels)

#Test the predicted values with a Cross Table
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

#When Bayes model has Laplace = 1
#train data
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
                              laplace = 1)
#test data
sms_test_pred2 <- predict(sms_classifier2, sms_test)

#Now using another CrossTable to evaluate the model again.
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

