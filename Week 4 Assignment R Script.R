##Week 4 Assignment - Decision Trees and Random Forests

library(readr)
winequality_red <- read_delim("Data Science School Documents/MSDS 680 Machine Learning/Week 4/winequality-red.csv", 
";", escape_double = FALSE, trim_ws = TRUE)
View(winequality_red)

#Shorten name of dataset file
wine <- winequality_red

#Data Exploration 
str(wine)

#Tables of vectors
table(wine$quality)
table(wine$alcohol)
table(wine$density)

#Summary of vectors
summary(wine$fixed_acidity)
summary(wine$citric_acid)
summary(wine$pH)

#Plot the variables and the distribution of quality
plot(wine)
hist(wine$quality)
#Set Training and Testing Sets
wine$quality <- as.factor(wine$quality)
set.seed(170)
train <- sample(1599, 1439)
str(train)

wine_train <- wine[train, ]
wine_test <- wine[-train, ]

prop.table(table(wine_train$quality))
prop.table(table(wine_test$quality))

#Bring in C50 package
install.packages("C50")
library(C50)

#Building the data model
wine_model <- C5.0(wine_train[-12], wine_train$quality) #Variable [-12] for number of variables in the dataset
wine_model
summary(wine_model)

#Test performance of the model
install.packages("gmodels")
library(gmodels)

#Predictor model for the wine data
wine_pred <- predict(wine_model, wine_test)

#Test the predicted to the actual data
CrossTable(wine_test$quality, wine_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#Boosting the model
wine_boost10 <- C5.0(wine_train[-12], wine_train$quality, trials = 10)
wine_boost10
summary(wine_boost10)

#Predict the boosted model
wine_boost_pred10 <- predict(wine_boost10, wine_test)
CrossTable(wine_test$quality, wine_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#Prep the Random Forest Model
install.packages("randomForest")
install.packages("caret")
library(randomForest)
library(caret)

#Set up RF Model
set.seed(170)
wine_rf <- randomForest(quality~., data=wine_train, mtry=4, ntree=2000, importance=TRUE, method="class", na.action=na.roughfix)
wine_rf
plot(wine_rf)

#Evaluate Random Forest Model
wine_result <- data.frame(wine$quality, predict(wine_rf, wine_test[,1:11], type="response"))
plot(wine_result)
