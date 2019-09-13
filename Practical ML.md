title: "Practical Machine Learning Quiz 4"
author: "Pooi Mun"
date: "9/13/2019"


# Executive Summary
## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 

## Objective
To predict activity quality (classe) from activity monitors

## Data
The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

# Model Development
## Load library and data
```{r}
library(caret)
library(randomForest)
library(ggplot2)

#download files from the urls provided
#train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
#download.file(url=train_url, destfile="pml-training.csv")

#test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
#download.file(url=test_url, destfile="pml-testing.csv")

train <- read.csv("pml-training.csv",header = TRUE,sep=",", na.strings = c("NA",""))
test <- read.csv("pml-testing.csv")

```

## Data Pre-processing
Unnecessary columns and columns containing 75% NA have been removed. Only the useful features are used for model development
```{r}
#Drop unnecessary columns
train1 <- train[,8:length(colnames(train))]
test1 <- test[,8:length(colnames(test))]

#Drop columns that have at least 75% NA
features <- c((colSums(!is.na(train1[,-ncol(train1)])) >= 0.75*nrow(train1)))
train2 <- train1[,features]
test2 <- test1[,features]

```

## Split training set and validation set
Before building model, 80% of the training data is set for training purpose and 20% of the training data is set for validation purpose
```{r}
#Create train and validation set
validation_index <- createDataPartition(train2$classe, p=0.80, list=FALSE)
train_set <- train2[validation_index,]
validation_set <- train2[-validation_index,]
```

## Random Forest 
Random Forest model is selected to build the model as it is suitable for classification and this model tends to be more accurate due to its ensemble methods

```{r}
#Model Development (Random forest)
set.seed(2)
RF_fit <- randomForest(classe~.,data=train_set)
print(RF_fit)
```

## Cross validation on validation set
The model is evaluated by applying on the validation set

```{r}
#Cross validation on validation set
predict1 <- predict(RF_fit,validation_set,type='class')
confusionMatrix(validation_set$classe,predict1)
```

## Estimate performance (error)
The model fit using the training set and is evaluated against the validation set. The predicted values are compared to the actual values. The accuracy of this model and the overall out of sample error are shown below

```{r}
#Accuracy
accuracy <- postResample(predict1,validation_set$classe)[1]
accuracy

#Out of sample error
overall_OSE <- 1-
as.numeric(confusionMatrix(validation_set$classe,predict1)$overall[1])
overall_OSE

```

## Results
The model is then applied to the test data to produce the result
```{r}
#Apply model to the test data
results <- predict(RF_fit,test2[,-length(names(test2))])
results

```
