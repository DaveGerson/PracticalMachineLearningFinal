library(caret)
library(AppliedPredictiveModeling)
require(randomForest)

setwd("C:/Users/gerson64/Desktop/Dropbox Sync/Dropbox/Coursera/practicalMachineLearning/project/")
train <- read.csv("pml-training.csv")
test <- read.csv("pml-testing.csv")

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

# Initial Exploration
summary(train$classe)
train<-train[,3:length(train)]
test<-test[,3:length(test)]

defactored <- data.frame(sapply(train[,sapply(train, class) == "factor"], as.numeric))
train[,sapply(train, class) == "factor" & names(train) != "classe"] <- defactored[,1:length(defactored) - 1]
test[,sapply(test, class) == "factor"] <- data.frame(sapply(test[,sapply(test, class) == "factor"], as.numeric))

naCount <- function(x) sum( is.na( x ) )
train <- train[ , sapply(test,naCount) == 0 ]
train <- train[ , sapply(train,naCount) == 0 ]

#no method is assigned since random forest is the default
model = as.formula(paste("classe ~ ", paste( names(train)[names(train) != "classe"]  , collapse = " + " )))  

inTrain = createDataPartition(train$classe, p = 3/4)[[1]]
training = train[ inTrain,]
testing = train[-inTrain,]
trainObj <- randomForest( model  , data=training, importance=TRUE, ntree= 106)
preds <- data.frame( pred = predict(trainObj, testing) , actual  =  testing$classe )

test[,sapply(test, class) == "factor"] <- data.frame(sapply(test[,sapply(test, class) == "factor"], as.numeric))
test <- test[,names(test) %in% names(train) ]
preds <- predict(trainObj, test)
as.character(preds)

pml_write_files( as.character(preds) )