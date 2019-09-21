##Week 7 Assignment
library(readr)
Abalone_Data <- read_csv("Data Science School Documents/MSDS 680 Machine Learning/Week 7/Abalone Data.txt", 
                col_names = FALSE)
View(Abalone_Data)
#Data Prep and Exploration
abalone <- Abalone_Data
head(abalone)
colnames(abalone) <- c("Sex", "Length", "Diameter","Height","Whole Weight","Shucked Weight","Viscera Weight","Shell Weight","Rings")
head(abalone)
#Set sex as type factor and rings as type numeric
abalone$Sex <- as.factor(abalone$Sex)
abalone$Rings <- as.numeric(abalone$Rings)
str(abalone)

#Exploration
dim(abalone)
sum(is.na(abalone))
summary(abalone)

summary(abalone$Rings)

#Creating the age range by rings for the Abalones
abalone$age <- abalone$Rings + 1.5 #Creates the age variable for Abalones
abalone$age <- cut(abalone$age, breaks = c(0,7,12,100), labels = c("young", "adult", "old")) #Creates 3 age buckets
str(abalone)
abalone <- subset(abalone, select = -Rings) #Remove rings to not have it factor into the output
summary(abalone$age)
head(abalone)

#Create a KNN classification algorithm
abalone$Sex <- NULL
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
abalone[1:7] <- as.data.frame(lapply(abalone[1:7], normalize))
summary(abalone$`Shucked Weight`)
#Now the variables are between 0 and 1. 

#Split the data into training and testing sets
abalone_sets <- sample(2, nrow(abalone), replace=TRUE, prob=c(0.75, 0.25))
abalone_train <- abalone[abalone_sets==1,]
abalone_test <- abalone[abalone_sets==2,]

#Bring in class library
library(class)

#Predictor - Set k = 30
abalone_pred <- knn(train = abalone_train[1:7], test = abalone_test[1:7], cl = abalone_train$age, k = 30)

library("gmodels")

#Test the model at k = 30
CrossTable(x = abalone_test$age, y = abalone_pred, prop.chisq = FALSE)
18+552+213
783/1048
#Answer = 0.7471374
#This means the accuracy of our first KNN model was just under 75%, not ideal but it could be worse

#Let's try it again, this time at k = 20
abalone_pred2 <- knn(train = abalone_train[1:7], test = abalone_test[1:7], cl = abalone_train$age, k = 20)
CrossTable(x = abalone_test$age, y = abalone_pred2, prop.chisq = FALSE)
19 + 549 + 207
775/1048
#Answer = 0.7395038

#Let's go higher than our original at k = 30 and instead go at k = 40.
abalone_pred3 <- knn(train = abalone_train[1:7], test = abalone_test[1:7], cl = abalone_train$age, k = 40)
CrossTable(x = abalone_test$age, y = abalone_pred3, prop.chisq = FALSE)
18 + 551 + 210
779/1048
#Answer = 0.7433206
#Neither of the following KNN models were able to score as accurately as the first when k = 30.

#Let's show the predictive power of the first model using a confusion matrix
library(caret)

confusionMatrix(abalone_pred, abalone_test$age)

#From the result we can see the accuracy of the model is just under 75% and a p-value of under 0.05. 

##Now I'll create a naive bayes classifier on the the abalone data.
NB_abalonetrain <- abalone_train
NB_abalonetest <- abalone_test

library(e1071)

abalone_model <- naiveBayes(age ~., data = NB_abalonetrain)
abalone_model

nb_pred <- predict(abalone_model, NB_abalonetest)
print(confusionMatrix(nb_pred, NB_abalonetest$age))

#The accuracy of the NB model is just slightly below 61% w/ a misclassification rate of 39%

#This proves that the KNN alogrithm is a much more effective model in predicting the age of the abalone than the Naive Bayes model. 

#I will now try bootstrapping the model to evaluate the model even further.
#First on Naive Bayes Method
nb_train_control <- trainControl(method='boot', number = 100)
tr_abalone_model <- train(age ~., data = abalone, trControl = nb_train_control, method = "nb")
print(tr_abalone_model)

#Next on KNN method
tr_abalone_model2 <- train(age ~., data = abalone, trControl=nb_train_control, method="knn")
print(tr_abalone_model2)

#From the accuracy outputs of the two bootstrap models, we see that KNN performs better than NB. Also k = 9 would've given the best output

#Now let's try a 10-fold cross validation to evaluate the two models
abalone_control = trainControl(method = "repeatedcv", number = 10, repeats = 3)
abalone_mod <- train(age~., data = abalone_train, method = "knn", preProcess="scale", trControl=abalone_control)
abalone_mod
#The 10 fold cross validation confirms the optimal model for KNN is one w/  k=9, and for NB, fL = 0 and usekernal = TRUE, and adjust = 1

#Now I'll create the new models with the ideal parameters
knn_pred <- knn(train = abalone_train[1:7], test = abalone_test[1:7], cl = abalone_train$age, k = 9)
confusionMatrix(knn_pred, abalone_test$age)
#At k=9, the model was about 73% accurate

#new model for NB 
nb_model <- naiveBayes(age ~., data = NB_abalonetrain, fL = 0, usekernal = TRUE, adjust = 1)
nb_model

#Predict NB model
nb_pred <- predict(nb_model, NB_abalonetest)
print(confusionMatrix(nb_pred, NB_abalonetest$age))
#Using the suggested parameters, the NB algorithm is still just under 61% accurate. The 10-fold models have close to equal accuracy to the originals
control_nb = trainControl(method="repeatedcv", number = 10, repeats = 3)
model <- train(age ~., data = NB_abalonetrain, method = "rf", preProcess = "scale", trControl=control_nb)
model
#Attempting to see what a random forest model would look like against this abalone data and it appears that it would've have been decently
# accurate ata about 75%. Overall, each machine learning algorithm is having a difficult time trying to predict the age of the abalone. 
#The solution to this issue would be to get better data or a large data set. 