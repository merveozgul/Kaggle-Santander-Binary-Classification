library(xgboost)
library(Matrix)
library(readr)
library(stringr)
library(caret)
library(car)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)
library(mlbench)
library(ranger)
library(unbalanced)

set.seed(1340)

train <- read.csv("etrain.csv")
test  <- read.csv("etest.csv")
cat('Length: ', nrow(train))


##### Removing IDs
train$ID <- NULL
test.id <- test$ID
test$ID <- NULL

##### Extracting TARGET
train.y <- train$TARGET
test.y <- test$TARGET
train$TARGET <- NULL
test$TARGET <- NULL

##### 0 count per line      -FEATURE n0
count0 <- function(x) {
  return( sum(x == 0) )
}

train$n0 <- apply(train, 1, FUN=count0)
test$n0 <- apply(test, 1, FUN=count0)

##### Removing constant features        -Drops many columns
cat("\n## Removing the constants features.\n")
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}

##### Removing identical features
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(train[[f1]] == train[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

# Define Feature Names after cleanup
feature.names <- setdiff(names(train), toRemove)

#Transforms variable with log to get smaller range
train$var38 <- log(train$var38)
test$var38 <- log(test$var38)

# Decreases feature numbers to 307
train <- train[, feature.names]
test <- test[, feature.names]
tc <- test



### Adds target to train set
train$TARGET <- train.y
# Gets a sparse matrix
#train <- sparse.model.matrix(TARGET ~ ., data = train)
#ts_label <- test$TARGET
#ts_label <- as.numeric(ts_label)-1
#test <- sparse.model.matrix(TARGET ~ ., data = test)

#dtrain <- xgb.DMatrix(data=train, label=train.y)

train$TARGET<-as.factor(train$TARGET)
summary(train$TARGET)
test.y <- as.factor(test.y)

# Manage Unbalanced Data
data(ubIonosphere)
n<-ncol(train)
output<- train$TARGET
input<- train [ ,-n]

#Balance the Dataset using ubSMOTE
data<-ubBalance(X= input, Y=output, type="ubSMOTE", percOver=100, percUnder=200, verbose=TRUE)
prop.table(table(data$Y))

#Balanced Data
train<-cbind(data$X,data$Y)
train$TARGET <- train$`data$Y`
train$`data$Y` <-NULL
head(train)

# Create model with default paramters
# Set a random seed (so you will get the same results as me)
set.seed(42)
# Train the model using a "random forest" algorithm
model <- ranger(TARGET ~ ., data = train)
#model <- ranger(TARGET ~ ., data = train, num.trees=100, mtry=25, classification = TRUE)
pred <- predict(model, data = test)
pred <- predictions(pred)
pred

#Conf. Matrix based on this model
conf.matrix <- round(prop.table(table(test.y, pred), 2), 2)
rownames(conf.matrix) <- c("Actually Satisfied", "Actually Unsatisfied")
colnames(conf.matrix) <- c("Predicted Satisfied", "Predicted Unsatisfied")
conf.matrix
confusionMatrix(test.y, pred)

#  Plot ROC
model <- ranger(TARGET ~ ., data = train, probability = TRUE)
pred <- predict(model, data = test)
pred <- predictions(pred)
library("ROCR")
fit.pred = prediction(pred[,2] , test.y)
fit.perf_Ranger = performance(fit.pred,"tpr","fpr")
plot(fit.perf_Ranger,lwd=2,col="red",
     main="ROC:  Classification Trees on Santander Dataset")
abline(a=0,b=1)

AUC<-function(actual,predicted)
{
  library(pROC)
  auc<-auc(as.numeric(actual),as.numeric(predicted))
  auc 
}
AUC(test.y,pred[,2]) ##AUC


#model <- ranger(TARGET ~ ., data = train, num.trees=100, mtry=25, classification = TRUE)
preds <- predict(model, data = test)
preds <- predictions(preds)
preds <- preds[,2]
preds

submission <- data.frame(ID=test.id, TARGET=preds)
cat("saving the submission file\n")
write.csv(submission, "e_sRangersubmit.csv", row.names = F)

