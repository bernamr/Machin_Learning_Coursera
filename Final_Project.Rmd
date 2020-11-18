---
title: "Machine Learning Project"
output:
  html_document:
    df_print: paged
---

```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)
```

## Background


Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement ??? a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).


## Loading Package.

First of all we are going to load all the packagees needed for the analysis. 

````{r packages}
library(kernlab); library(caret); library(ggplot2); library(rpart); 
library(randomForest); library(rattle)

```
## Loading Data. 

Now we will  load the data needed provided by the course. This data came in two types: Training Set and Test set.

```{r data}
training_data <- read.csv("pml-training.csv", na.strings =c("NA","#DIV/0!", "") )
test_data <- read.csv("pml-testing.csv", na.strings = c("NA", "#DIV/0!",""))

```
Already we set the NA values of the data for in order to simply further analysis. Also we see the deminsion of the two sort of data

```{r dim}
dim(training_data)
dim(test_data)
```

## Cleaning Data

Now we need to clean the training data in order yo build or models. 



```{r cleaning , echo=TRUE}
training_data <- training_data[,sapply(training_data[, -53],is.numeric)] # remove non-numeric variables
training_data <- training_data[, colMeans(is.na(training_data))< 0.9] # removing NA values
training_data <- training_data[, -c(1:7)] # Removing the first 7 variables. 
training_data$classe <- as.factor(training_data$classe) # trating classe variable as factor. 
```

Also we are going to eliminated the variables highly correlated. This is important because its allows as to have less explanatory variables by eliminated those who are highly correlated being redundant. 

```{r correlation}
corrtable<- cor(training_data[, -49])
highcorrvars<- findCorrelation(corrtable, cutoff = 0.8)

training_clean <-  training_data[, -highcorrvars]
dim(training_clean)
names(training_clean)
```

Alse we could se how the dimention of the training data now es less than the begining.


## Data partitions

ONce we have the clean training data we can do the partition into training set (s_train) andtesting set(set_test). We do this by maintining the 80% of the data in the training set in order to fix ours differents models. 

```{r partiton}
train_cv <- createDataPartition(training_clean$classe, p=0.8, list = F)
s_train <- training_clean[train_cv,]
s_test <- training_clean[-train_cv,]

plot(as.factor(s_train$classe),  main="classe in s_train data set", xlab="classe", ylab="Frequency", col="steelblue")
```

## Algorithms

We are going to fix 3 differents algorithms in order to choose the best one for the prediction in the Test_data. The algorithms  we will use are: Decision Tree, Random Forest and Support Vector Machine 
Using the s_train data we will train the models, and after that we will predict the values of classe variable in s_test data. We will compare the results in order to choose the most accurate model. 

### Decision Tree

```{r decisin tree}
modelTree <- rpart(classe ~ ., data=s_train, method="class")

predicTree <- predict(modelTree, s_test, type = "class")
tree_matrix <- confusionMatrix(predicTree, s_test$classe)

```

### Random Forest. 

```{r random f}
modelForest <- randomForest(classe ~., data = s_train, method ="class")
predictForest <- predict(modelForest, s_test, type = "class")
forest_matrix <- confusionMatrix(predictForest, s_test$classe)
VarImport <- varImp(modelForest)
varImpPlot(modelForest,n.var = 10)
```

### Support Vector Machine.

```{r svm}
mod_svm <- train(classe~., data=s_train, method="svmLinear")
pred_svm <- predict(mod_svm, s_test)
svm_matrix <- confusionMatrix(pred_svm, (s_test$classe))
```

### Accurancy Analisis.

```{r accurancy}
tree_accuracy <- tree_matrix$overall["Accuracy"]
forest_accuracy <- forest_matrix$overall["Accuracy"]
svm_accurancy <- svm_matrix$overall["Accuracy"]

accurancy <- matrix(c(tree_accuracy, forest_accuracy, svm_accurancy), nrow = 1, ncol = 3)
colnames(accurancy) <- c("tree_model", "forest_model", "svm_model")
accurancy
```

We can see that the best model in our case is the Random Forest. 

## Prediction.

Due the results of the previus section,  we are going to use the Random Forest model to predict the classe variable in the Test_Data.
First we prepared the test data.

```{r  test data}
test_data <- test_data[,sapply(test_data,is.numeric)] # remove non-numeric variables
test_data <- test_data[, colMeans(is.na(test_data))< 0.9]
test_clean <- test_data[, -c(1:7)]
```

Now we can use our model:

```{r predict}
predict_test <- predict(modelForest, test_clean, type="class")
predict_test
```

