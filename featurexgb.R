library(data.table)
library(rpart)
library(rpart.plot)
library(caret)
library(xgboost)
library(pROC)
library('bit64')
library(methods)
library(margittr)
library(DiagrammeR)

#Lets see what the model looks like.

#{r modelDump} Not Easy To Understand
model <- xgb.dump(model_tuned, with_stats = T)
model[1:10]

# Get the feature real names
names <- dimnames(dtrain)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = model_tuned)

# Nice graph
xgb.plot.importance(importance_matrix[1:10,])

#Get Tree
xgb.plot.tree(feature_names = names, model = model_tuned, trees = 2)


importanceRaw <- xgb.importance(feature_names = colnames(dtrain), model = model_tuned)
# Cleaning for better display
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]
head(importanceClean, 10)

# Show relation between var15 and output
# In labeled train set
plot(factor(train.y), dftrain$var15)
# In predictions with XGB Model
preds.cv = ifelse (preds > 0.5,1,0)
levels(preds.cv) <- c(0, 1)
plot(factor(preds.cv),tc$var15)

