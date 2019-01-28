##loading packages needed for this task
install.packages("caret")
install.packages("randomForest")
install.packages("e1071")
install.packages("rpart")
library(caret)
library(randomForest)
library(e1071)


##loading the datasets
training<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
quiz<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
##Breifly summarize the data
str(training)

##Removing variables with missing values
training1<-training[, colSums(is.na(training))== 0]
quiz1<-quiz[, colSums(is.na(quiz)) ==0]
sum(complete.cases(training1))
sum(complete.cases(quiz1))

##Have a look of the dirstribution of classes
percentage <- prop.table(table(training1$classe)) * 100
cbind(freq=table(training1$classe), percentage=percentage)
#   freq percentage
# A 5580   28.43747
# B 3797   19.35073
# C 3422   17.43961
# D 3216   16.38977
# E 3607   18.38243
barplot(percentage)


##After removing variables which all the values are NAs, we still find many variables has very few
##values, we need to remove those predictors with near zero variance functions
x<-nearZeroVar(training1)
training1<-training1[,-x]

##Create training set(70%) and testing set(30%)
inTrain<-createDataPartition(training1$classe, p=0.7, list = F)
trainingSet<-training1[inTrain,]
testingSet<-training1[-inTrain,]


##Building algorithms:Random Forest, Classification and Regression Trees and Gradient Boosting Machine
set.seed(781)
Model_rf<-train(classe~.,data = trainingSet, method="rf")
Model_cart<-train(classe~., data=trainingSet, method="rpart")
Model_gbm<-train(classe~., data=trainingSet, method="gbm",verbose=F)

prediction_rf<-predict(Model_rf, newdata=testingSet)
prediction_cart<-predict(Model_cart,newdata = testingSet)
prediction_gbm<-predict(Model_gbm, newdata=testingSet)

##find the accuracy of each
rf_acc<-confusionMatrix(prediction_rf, testingSet$classe)$overall[1]
cart_acc<-confusionMatrix(prediction_cart,testingSet$classe)$overall[1]
gbm_acc<-confusionMatrix(prediction_gbm, testingSet$classe)$overall[1]

##visualize the accuracy
barplot(rf_acc,cart_acc, gbm_acc)

##use random forest model for predicting final results 
result<-predict(Model_rf, newdata=quiz1)



