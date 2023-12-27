library(stats)
library(dplyr)
library(randomForest)
library(caret)
library(e1071)
library(ggplot2)
str(waterQuality1)
set.seed(1)
df =waterQuality1
df <- na.omit(df)
df$ammonia <- ifelse(is.na(df$ammonia), 0, df$ammonia)
df$is_safe=as.numeric(df$is_safe)
df <- na.omit(df)
df[df$is_safe== 0,]$is_safe="NO"
df[df$is_safe== 1,]$is_safe="YES"
df$is_safe=as.factor(df$is_safe)
train_split_idx <- caret::createDataPartition(df$is_safe, p = 0.70, list = FALSE)
train <- df[train_split_idx, ]
test <- df[-train_split_idx,]
rf.df=randomForest(is_safe~.,data=train,mtry=20,importance=TRUE,)
rf_class <- predict(rf.df, newdata = test, type = "response")
predictions <- cbind(data.frame(test_preds=rf_class,test$is_safe))
cm <- caret::confusionMatrix(predictions$test_preds, predictions$test.is_safe,mode="prec_recall")
print(cm)
##Albero decisionale##
library(rpart)
library(rpart.plot)
library(partykit)
set.seed(2)
train_split_idx <- caret::createDataPartition(df$is_safe, p = 0.70, list = FALSE)
train <- df[train_split_idx, ]
test <- df[-train_split_idx,]
tree_model = rpart(is_safe ~ ., data = train, method = "class")
dc_class=predict(tree_model, newdata = test, type = "class")
predictions <- cbind(data.frame(test_preds=dc_class,test$is_safe))
cm <- caret::confusionMatrix(predictions$test_preds, predictions$test.is_safe,mode="prec_recall")
print(cm)
plot(as.party(tree_model))