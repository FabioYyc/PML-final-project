##loading packages needed for this task
install.packages("caret")
install.packages("randomForest")
install.packages("e1071")
install.packages("rpart")
library(caret)
library(randomForest)
library(e1071)
library(rpart)

##loading the datasets
training<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
quiz<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
##Breifly summarize the data
str(training)

##Removing variables with missing values
training1<-training[, colSums(is.na(training))== 0]
sum(complete.cases(training1))

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
##removing the columns are not related to making predcition
training1<-training1[,-c(1:7)]
x<-nearZeroVar(training1)
training1<-training1[,-x]


##Create training set(70%) and testing set(30%)
##Samping the training set because my pc can not handle the sample size
training2<-training1[sample(nrow(training1), 2000), ]
inTrain<-createDataPartition(training2$classe, p=0.75, list=F)
trainingSet<-training2[inTrain,]
testingSet<-training2[-inTrain,]
##Building algorithms:Random Forest, Classification and Regression Trees 
set.seed(781)
Model_rf<-train(classe~.,data = trainingSet, method="rf")
Model_cart<-rpart(classe~., data=trainingSet, method="class")

fancyRpartPlot(Model_cart)

prediction_rf<-predict(Model_rf, newdata=testingSet)
prediction_cart<-predict(Model_cart,newdata = testingSet, type="class")


##find the accuracy of each
confusionMatrix(prediction_rf, testingSet$classe)$overall[1]
##Accuracy 
##0.9317269
sum(prediction_cart==testingSet$classe)/length(testingSet$classe)
##0.6646586

##use random forest model for predicting final results
##Process the quiz set same as training set for better outcome
quiz <-quiz[,colSums(is.na(quiz)) == 0]
quiz<-quiz[,-c(1:7)]


result<-predict(Model_rf, newdata=quiz)
result
## [1] B A A A A E D D A A B C B A E E A B A B
##Levels: A B C D E



