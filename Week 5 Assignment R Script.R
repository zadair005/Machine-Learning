##Week 5 Assignment

##SVM

#Load the data
mushroom <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), 
                     header = FALSE, sep = ",")
str(mushroom)


#Name Columns
colnames(mushroom) <- c("edibility", "cap_shape", "cap_surface", "cap_color", "bruises", "odor", "grill_attachment", "grill_spacing", "grill_size", "grill_color", 
                    "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type",
                    "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")
head(mushroom)

#Check for missing data values
sum(is.na(mushroom))

#Get rid of veil_type because it is useless for this analysis
mushroom <- subset(mushroom, select = -veil_type)

#Change the variables from factors to numeric
mushroom$cap_shape <- as.numeric(mushroom$cap_shape)
mushroom$cap_surface <- as.numeric(mushroom$cap_surface)
mushroom$cap_color <- as.numeric(mushroom$cap_color)
mushroom$bruises <- as.numeric(mushroom$bruises)
mushroom$odor <- as.numeric(mushroom$odor)
mushroom$grill_attachment <- as.numeric(mushroom$grill_attachment)
mushroom$grill_spacing <- as.numeric(mushroom$grill_spacing)
mushroom$grill_size <- as.numeric(mushroom$grill_size)
mushroom$grill_color <- as.numeric(mushroom$grill_color)
mushroom$stalk_shape <- as.numeric(mushroom$stalk_shape)
mushroom$stalk_root <- as.numeric(mushroom$stalk_root)
mushroom$stalk_surface_above_ring <- as.numeric(mushroom$cap_shape)
mushroom$stalk_surface_below_ring <- as.numeric(mushroom$cap_shape)
mushroom$stalk_color_above_ring <- as.numeric(mushroom$stalk_color_above_ring)
mushroom$stalk_color_below_ring <- as.numeric(mushroom$stalk_color_below_ring)
mushroom$veil_color <- as.numeric(mushroom$veil_color)
mushroom$ring_number <- as.numeric(mushroom$ring_number)
mushroom$ring_type <- as.numeric(mushroom$ring_type)
mushroom$spore_print_color <- as.numeric(mushroom$spore_print_color)
mushroom$population <- as.numeric(mushroom$population)
mushroom$habitat <- as.numeric(mushroom$habitat)

#Explore the data
summary(mushroom)
Histo <- as.numeric(mushroom$edibility)
hist(Histo)

table(Histo)

#Prepare to make Training and Test Datasets @ 80:20 split
mushroom_train <- mushroom[1:6093, ]
mushroom_test <- mushroom[6094:8124, ]

#Install needed R Packages
install.packages("e1071")
library(e1071)
install.packages("gamlss")
library(gamlss)

#SVM Model
svm_mushroom <- svm(edibility ~., data = mushroom_train, kernel = 'linear', cost = 1, scale = FALSE)
print(svm_mushroom)

#Test the accuracy of the model 
svm.pred <- predict(svm_mushroom, mushroom_test[, !names(mushroom_test) %in% c("edibility")])
svm.table <- table(svm.pred, mushroom_test$edibility)
svm.table

#Accuracy of SVM was?
1923/2031
#Almost 95%, pretty good!

#Trying to improve the model
svm_mushroom2 <- svm(edibility ~ ., data = mushroom_train, kernel = 'linear', cost = 100, scale = FALSE)
svm.pred2 <- predict(svm_mushroom2, mushroom_test[, !names(mushroom_test) %in% c("edibility")])
svm.table2 <- table(svm.pred2, mushroom_test$edibility)
svm.table2

#Accuracy of SVM 2?
1915/2031
#A little worse, which was not expected but it's the result 

#Next, change the kernel type and see if there is improvement on in that model
svm_mushroom3 <- svm(edibility ~., data=mushroom_train, kernel = 'radial', cost = 100, scale = FALSE)
svm.pred3 <- predict(svm_mushroom3, mushroom_test[, !names(mushroom_test) %in% c("edibilility")])
svm.table3 <- table(svm.pred3, mushroom_test$edibility)
svm.table3

#Accuracy of SVM model 3?
1725/2031
#Worse than the previous two, so changing the kernel type just drastically weakened model performance, no enhancement

##Neural Networks

#Data prep, remove stalk root field
mushroom <- subset(mushroom, select = -stalk_root)

#Import libraries
library(caret)

#Created dummy variables to split into Training and test sets
party <- createDataPartition(mushroom$edibility, p = .8, list = FALSE)

dum <- subset(mushroom, select = -edibility)

shroom_dummy <- dummyVars(~., data = dum, sep= ".")
shroom_dummy <- data.frame(predict(shroom_dummy, dum))

ncol(shroom_dummy)

shroom_dummy$edibility <- mushroom$edibility
ncol(shroom_dummy)

#Set the training and test sets for NN
NN_train <- shroom_dummy[party,]
NN_test <- shroom_dummy[-party,]
NN_testLabels <- subset(NN_test, select = edibility)
NN_testset <- subset(NN_test, select = -edibility)

#Bring in nnet package
install.packages("nnet")
library(nnet)

net <- nnet(edibility ~., data = NN_train, size = 2, rang = 0.1, maxit = 200)

summary(net)

shroom.predict <- predict(net, NN_testset, type = "class")

net_table <- table(NN_test$edibility, shroom.predict)
net_table

#Test accuracy of the Net table
1179/1606

library(caret)
confusionMatrix(net_table)

#Add gamlss.add package
install.packages("gamlss.add")
library(gamlss.add)

#Plot Neural Net
plot(net, .2)

