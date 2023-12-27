library(stats)
library(dplyr)
library(randomForest)
library(caret)
library(e1071)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(partykit)
str(glass)
set.seed(1)
glass$Type=factor(glass$Type)
df =glass
train_split_idx <- caret::createDataPartition(df$Type, p = 0.75, list = FALSE)
train <- df[train_split_idx, ]
test <- df[-train_split_idx,]
rf.df=randomForest(Type~.,data=train,mtry=9,ntree= 500, importance=TRUE)##1##
oob.error.data=data.frame(Trees=rep(1:nrow(rf.df$err.rate),times=1),Type=rep(c("OOB"),each=nrow(rf.df$err.rate)),Error=c(rf.df$err.rate[,"OOB"]))
ggplot(data=oob.error.data,aes(x=Trees,y=Error))+geom_line(aes(color=Type))
oob.values=vector(length=9)
for(i in 1:9)##2##
{rf.l=randomForest(Type~.,data=train,mtry=i,importance=TRUE,ntree=500)
oob.values[i]=rf.l$err.rate[nrow(rf.l$err.rate),1]}
print(oob.values)
## dopo aver eseguito l'operazione 1 e l'operazione 2 vediamo che il modello Ra
##ndom forest da il meglio con 800 alberi e 4 variabili##
rf.df=randomForest(Type~.,data=train,mtry=4,ntree= 800, importance=TRUE)
oob.error.data=data.frame(Trees=rep(1:nrow(rf.df$err.rate),times=1),Type=rep(c("OOB"),each=nrow(rf.df$err.rate)),Error=c(rf.df$err.rate[,"OOB"]))
ggplot(data=oob.error.data,aes(x=Trees,y=Error))+geom_line(aes(color=Type))
rf_class <- predict(rf.df, newdata = test, type = "response")
predictions <- cbind(data.frame(test_preds=rf_class,test$Type))
cm <- caret::confusionMatrix(predictions$test_preds, predictions$test.Type,mode="prec_recall")
importance(rf.df)
print(cm)
importance(rf.df)
#proviamo a costruire un modello con le variabili più rilevanti#
glass2=glass[,-c(2,5,9)]
glass2$Type=factor(glass2$Type)
df =glass2
train_split_idx <- caret::createDataPartition(df$Type, p = 0.75, list = FALSE)
train <- df[train_split_idx, ]
test <- df[-train_split_idx,]
rf.df=randomForest(Type~.,data=train,mtry=6,ntree= 500, importance=TRUE)
oob.error.data=data.frame(Trees=rep(1:nrow(rf.df$err.rate),times=7),Type=rep(c("OOB","1","2","3","5","6","7"),each=nrow(rf.df$err.rate)),Error=c(rf.df$err.rate[,"OOB"],rf.df$err.rate[,"1"],rf.df$err.rate[,"2"],rf.df$err.rate[,"3"],rf.df$err.rate[,"5"],rf.df$err.rate[,"6"],rf.df$err.rate[,"7"]))
ggplot(data=oob.error.data,aes(x=Trees,y=Error))+geom_line(aes(color=Type))
oob.values=vector(length=6)
for(i in 1:6)##2##
{rf.l=randomForest(Type~.,data=train,mtry=i,importance=TRUE,ntree=500)
oob.values[i]=rf.l$err.rate[nrow(rf.l$err.rate),1]}
print(oob.values)
##vediamo che il nemero di alberi ottimale è 800 e il numero ottimale di variabili è 3##
rf.df=randomForest(Type~.,data=train,mtry=3,ntree= 800, importance=TRUE)
rf_class <- predict(rf.df, newdata = test, type = "response")
predictions <- cbind(data.frame(test_preds=rf_class,test$Type))
cm <- caret::confusionMatrix(predictions$test_preds, predictions$test.Type,mode="prec_recall")
importance(rf.df)
print(cm)
##Notiamo che le performances del modello sono migliorate##
##usiamo KNN##
library(stats)
library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
str(glass)
set.seed(1)
glass$Type=factor(glass$Type)
df =glass
train_split_idx <- caret::createDataPartition(df$Type, p = 0.75, list = FALSE)
train <- df[train_split_idx, ]
test <- df[-train_split_idx,]
model <- train(Type~., data = train, method = "knn", preProc = c("center", "scale"), tuneLength = 10)
knn_class <- predict(model, newdata = test, type = "raw")
predictions <- cbind(data.frame(test_preds=knn_class,test$Type))
cm <- caret::confusionMatrix(predictions$test_preds, predictions$test.Type,mode="prec_recall")
print(cm)
##proviamo a costruire un modello knn con le variabili più rilevanti##
library(stats)
library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
glass2=glass[,-c(2,5,9)]
set.seed(2)
glass2$Type=factor(glass2$Type)
df =glass2
glass2$Type=factor(glass2$Type)
train_split_idx <- caret::createDataPartition(df$Type, p = 0.75, list = FALSE)
train <- df[train_split_idx, ]
test <- df[-train_split_idx,]
model <- train(Type~., data = train, method = "knn", preProc = c("center", "scale"), tuneLength = 10)
knn_class <- predict(model, newdata = test, type = "raw")
predictions <- cbind(data.frame(test_preds=knn_class,test$Type))
cm <- caret::confusionMatrix(predictions$test_preds, predictions$test.Type,mode="prec_recall")
print(cm)
##Decision tree##
tree_model = rpart(Type ~ ., data = train, method = "class")
print(tree_model)
dc_class=predict(tree_model, newdata = test, type = "class")
predictions <- cbind(data.frame(test_preds=dc_class,test$Type))
cm <- caret::confusionMatrix(predictions$test_preds, predictions$test.Type,mode="prec_recall")
print(cm)
plot(as.party(tree_model))


