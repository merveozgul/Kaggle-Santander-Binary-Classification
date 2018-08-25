library(caret)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

#understanding the class of our dataset
class(train)

#looking for structure of my data
str(train) #result is every feature is either num or integer. Num are "numeric values" approximation to real nums.Integers are Z whole numbers

#looking at the types of each variable, another method
sapply(train, class)

#looking if we have any missing values in the cols, features. Missing values are represented
#with NA.
colSums(is.na(train)) #we dont have any missing values in our dataset 

#counting the missing values again, in total 
sum(colSums(is.na(train))) #we have 0 missing values

#looking for min and max values for columns
apply(train,2,min)
apply(train,2,max)

#getting min, max mean values with "psych" package
library(psych) #if this code does not run probably you havent installed this package! 
describe(train)

#looking for correlation between our attributes, first we need to download the caret package.
library(caret)
set.seed(123) #we would like to do this randomly 
correlationMatrix <- cor(train[,2:370]) #creating a correlation matrix with all of the attributes
View(correlationMatrix)

#viewing the head of the correlation matrix
head(correlationMatrix)

#finding highly correlated features
library(caret)
cor(na.omit(correlationMatrix))
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.9, verbose=FALSE)

#creating a heatmap for correlation
library(ggcorrplot)
ggcorrplot(correlationMatrix) #if we run this correlation plot without any limitations we will get a bad
#and noisy plot and we get a lot of NA values, missing values

#looking at histograms, these are examples
hist(train$var3)
hist(train$var38)
hist(train$TARGET)


#exploring the constant features in the dataset
zero.var = nearZeroVar(train, saveMetrics=TRUE)
zero.var


####################Data Cleaning Part


##### Removing constant features
cat("n## Removing the constants features.n")
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.n")
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}

##### Removing identical features, this process can be found online
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(train[[f1]] == train[[f2]])) {
      cat(f1, "and", f2, "are equals.n")
      toRemove <- c(toRemove, f2)
    }
  }
}

##### Removing IDs
train$ID <- NULL
test.id <- test$ID
test$ID <- NULL

##### Extracting TARGET
train.y <- train$TARGET
train$TARGET <- NULL

feature.names <- setdiff(names(train), toRemove)

train <- train[, feature.names]
test <- test[, feature.names]





