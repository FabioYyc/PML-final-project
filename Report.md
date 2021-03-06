# Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal
activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who 
take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks.
One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. 
In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were
asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available 
from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

# Prediction
Six young health participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell 
Biceps Curl in five different fashions: exactly according to the specification (Class A), throwing the elbows to the front (Class B), 
lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E).

Class A corresponds to the specified execution of the exercise, while the other 4 classes correspond to common mistakes.

# How the model is built

## Loading the files and needed pacakges
The validation dataset were named as quiz
```
install.packages("caret")
install.packages("randomForest")
install.packages("e1071")
install.packages("rpart")
library(caret)
library(randomForest)
library(e1071)
library(rpart)
    
    training<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
    quiz<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
  
```
## cleaning the dataset and create training and testing set
the columns with no values or very few values are removed, then create training and testing set 

```

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
```
![bar plot](https://github.com/FabioYyc/PML-final-project/blob/master/Rplot.png)


## Further processing of the dataset
After removing variables which all the values are NAs, we still find many variables has very few
values, we need to remove those predictors with near zero variance functions
remove the columns are not related to making predictions
*The training set was sampled down to to 2000 rows because the computing power of my pc

```
training1<-training1[,-c(1:7)]
x<-nearZeroVar(training1)
training1<-training1[,-x]
 ##Because my computer can not handle the sample size of training set, I had to cut the size down to 3000 rows
   training2<-training1[sample(nrow(training1), 2000), ]
   
   
##Create training set(70%) and testing set(30%)
inTrain<-createDataPartition(training1$classe, p=0.75, list = F)
trainingSet<-training2[inTrain,]
testingSet<-training2[-inTrain,]

```

    
## Build models with random forest, classfication tree, and use the models to predict the testing set
   
    set.seed(781)
    Model_rf<-train(classe~.,data = trainingSet, method="rf")
    Model_cart<-rpart(classe~., data=trainingSet, method="class")
   
    
    fancyRpartPlot(Model_cart)
 ![Tree](https://github.com/FabioYyc/PML-final-project/blob/master/Tree.png)
    
    prediction_rf<-predict(Model_rf, newdata=testingSet)
    prediction_cart<-predict(Model_cart,newdata = testingSet, type="class")
   
    
## Find the accuracy of the prediction models
    confusionMatrix(prediction_rf, testingSet$classe)$overall[1]
    ## Accuracy 
     0.9317269 
     
     sum(prediction_cart==testingSet$classe)/length(testingSet$classe)
    ##0.6646586

    


random forest method here seems to have the highest accuracy, hence select this model to predict validation set
 
    ##process the validation data the same way for better results
    quiz <-quiz[,colSums(is.na(quiz)) == 0]
    quiz<-quiz[,-c(1:7)]
  
    
    result<-predict(Model_rf, newdata=quiz)
    result
    ## [1] B A A A A E D D A A B C B A E E A B A B
    ##Levels: A B C D E

## Out of sample error
Out of sample error of the model =1- accuracy =1 -0.93 =0.07=7%
However, due to small sample size for training the model, the error will be larger 
