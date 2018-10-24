library(xgboost)
library(Matrix)
library(xgboost)
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
train <- sparse.model.matrix(TARGET ~ ., data = train)

test$TARGET <- -1
#ts_label <- test$TARGET
#ts_label <- as.numeric(ts_label)-1
test <- sparse.model.matrix(TARGET ~ ., data = test)

dtrain <- xgb.DMatrix(data=train, label=train.y)

# get the number of negative & positive cases in our data
negative_cases <- sum(train.y == FALSE)
postive_cases <- sum(train.y == TRUE)

# 4 Fold Cross Validation for 35 Rounds
nround.cv = 35
system.time( bst.cv <- xgb.cv(data = dtrain, # the data           
                              max.depth = 5, # the maximum depth of each decision tree
                              nround = nround.cv, # number of boosting rounds
                              nfold=4, 
                              objective = "binary:logistic", # the objective function
                              eval_metric = "auc",
                              prediction = TRUE,
                              scale_pos_weight = negative_cases/postive_cases # control for imbalanced classes
) )

#tail(bst.cv$evaluation_log)
max.auc.idx = which.max(bst.cv$evaluation_log[, test_auc_mean])
bst.cv$evaluation_log[max.auc.idx,]

# train a model using our training data
model_tuned <- xgboost(data = dtrain, # the data           
                       max.depth = 5, # the maximum depth of each decision tree
                       nround = max.auc.idx, # number of boosting rounds
                       #early_stopping_rounds = 3, # if we dont see an improvement in this many rounds, stop
                       objective = "binary:logistic", # the objective function
                       scale_pos_weight = negative_cases/postive_cases # control for imbalanced classes
) 

pred <- predict(model_tuned, test)



# Confusion Matrix for Train Data
pred.cv = ifelse (pred > 0.5,1,0)
levels(pred.cv) <- c(0, 1)
conf.matrix <- round(prop.table(table(test.y, factor(pred.cv)), 2), 2)
rownames(conf.matrix) <- c("Actually Satisfied", "Actually Unsatisfied")
colnames(conf.matrix) <- c("Predicted Satisfied", "Predicted Unsatisfied")
conf.matrix
confusionMatrix(factor(test.y), factor(pred.cv))


#  Plot ROC
library("ROCR")
fit.pred = prediction(pred , test.y)
fit.perf_XGB = performance(fit.pred,"tpr","fpr")
plot(fit.perf_XGB,lwd=2,col="blue",
     main="ROC:  Classification Trees on Santander Dataset")
abline(a=0,b=1)

AUC<-function(actual,predicted)
{
  library(pROC)
  auc<-auc(as.numeric(actual),as.numeric(predicted))
  auc 
}
AUC(test.y,pred) ##AUC

preds <- predict(model_tuned, test)

submission <- data.frame(ID=test.id, TARGET=preds)
cat("saving the submission file\n")
write.csv(submission, "e_Overfitsubmit.csv", row.names = F)


# PLOT ROC CURVES FOR rparts, Ranger and xgb
plot(fit.perf_XGB,lwd=2,col="blue",
     main="ROC:  Classification Trees on Santander Dataset")
plot(fit.perf_Ranger,lwd=2,col="red", lty=4,
     add=TRUE)
plot(fit.perf_rp,lwd=2,col="black", lty=3,
     add=TRUE)
abline(a=0,b=1)


