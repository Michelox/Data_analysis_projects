library(stats)
library(dplyr)
library(randomForest)
library(caret)
library(e1071)
library(ggplot2)
library(rpart)
library(partykit)
str(train)
df=train
ggplot(df, aes(x = price_range)) + 
  geom_density(color = "purple")
ggplot(df, aes(x = "", y = price_range)) + 
  geom_boxplot(color = "blue") +
  xlab("") +
  ylab("price_range")
df$blue=as.factor(df$blue)
df$dual_sim=as.factor(df$dual_sim)
df$four_g=as.factor(df$four_g)
df$three_g=as.factor(df$three_g)
df$touch_screen=as.factor(df$touch_screen)
df$price_range=as.factor(df$price_range)
set.seed(12)
train_split_idx <- caret::createDataPartition(df$price_range, p = 0.75, list = FALSE)
train <- df[train_split_idx, ]
test <- df[-train_split_idx,]
rf.df=randomForest(price_range~.,data=train,mtry=20,ntree= 500, importance=TRUE)
oob.values=vector(length=20)
for(i in 1:20)
{rf.l=randomForest(price_range~.,data=train,mtry=i,importance=TRUE,ntree=500)
oob.values[i]=rf.l$err.rate[nrow(rf.l$err.rate),1]}
print(oob.values)
##valore ottimale mtry=10
##troviamo il valore ottimale di alberi
oob.error.data=data.frame(Trees=rep(1:nrow(rf.df$err.rate),times=1),Type=rep(c("OOB"),each=nrow(rf.df$err.rate)),Error=c(rf.df$err.rate[,"OOB"]))
ggplot(data=oob.error.data,aes(x=Trees,y=Error))+geom_line(aes(color=Type))
##proviamo 800 alberi##
rf.df=randomForest(price_range~.,data=train,mtry=20,ntree= 800, importance=TRUE)
oob.error.data=data.frame(Trees=rep(1:nrow(rf.df$err.rate),times=1),Type=rep(c("OOB"),each=nrow(rf.df$err.rate)),Error=c(rf.df$err.rate[,"OOB"]))
ggplot(data=oob.error.data,aes(x=Trees,y=Error))+geom_line(aes(color=Type))
##si vede l'oob che è più basso a 200,proviamo 200 alberi##
rf.df=randomForest(price_range~.,data=train,mtry=20,ntree= 200, importance=TRUE)
##modello ottimizzato##
rf.df=randomForest(price_range~.,data=train,mtry=10,ntree= 250, importance=TRUE)
rf_class <- predict(rf.df, newdata = test, type = "response")
predictions <- cbind(data.frame(test_preds=rf_class,test$price_range))
cm <- caret::confusionMatrix(predictions$test_preds, predictions$test.price_range,mode="prec_recall")
print(cm)
##proviamo con regressione logistica multinomiale##
library(nnet)
model <- multinom(price_range ~ ., data = train)
predictions <- predict(model, newdata = test)
pl_class=cbind(data.frame(test_preds=predictions,test$price_range))
cm <- caret::confusionMatrix(pl_class$test_preds, pl_class$test.price_range,mode="prec_recall")
print(cm)