library(stats)
library(dplyr)
library(randomForest)
library(caret)
library(e1071)
library(ggplot2)
str(breast.cancer)
set.seed(1)
breast.cancer$diagnosis=as.factor(breast.cancer$diagnosis)
df =breast.cancer
train_split_idx <- caret::createDataPartition(df$diagnosis, p = 0.70, list = FALSE)
train <- df[train_split_idx, ]
test <- df[-train_split_idx,]
rf.df=randomForest(diagnosis~.,data=train,mtry=31,importance=TRUE,)
#verifica numero di alberi#
oob.error.data=data.frame(Trees=rep(1:nrow(rf.df$err.rate),times=3),Type=rep(c("OOB","M","B"),each=nrow(rf.df$err.rate)),Error=c(rf.df$err.rate[,"OOB"],rf.df$err.rate[,"M"],rf.df$err.rate[,"B"]))
ggplot(data=oob.error.data,aes(x=Trees,y=Error))+geom_line(aes(color=Type))
## L'errore OOB diminuisce sensibilmente con 50 alberi##
#verifica del numero di variabili#
oob.values=vector(length=31)
for(i in 1:31)
{rf.l=randomForest(diagnosis~.,data=train,mtry=i,importance=TRUE,ntree=50)
oob.values[i]=rf.l$err.rate[nrow(rf.l$err.rate),1]}
print(oob.values)
## Il numero di variabili più opportuno è 21##
rf.df=randomForest(diagnosis~.,data=train,mtry=21,importance=TRUE,ntree=50)
rf_class <- predict(rf.df, newdata = test, type = "response")
predictions <- cbind(data.frame(test_preds=rf_class,test$diagnosis))
cm <- caret::confusionMatrix(predictions$test_preds, predictions$test.diagnosis,mode="prec_recall")
importance(rf.df)
print(cm)
##i Risultati sono sbalorditivi, per scrupolo proviamo con la regressione logistica##
library(stats)
library(dplyr)
library(randomForest)
library(caret)
library(e1071)
library(ggplot2)
str(breast.cancer)
set.seed(1)
breast.cancer$diagnosis=as.factor(breast.cancer$diagnosis)
df =breast.cancer
train_split_idx <- caret::createDataPartition(df$diagnosis, p = 0.70, list = FALSE)
train <- df[train_split_idx, ]
test <- df[-train_split_idx,]
model <- glm(diagnosis~., data =train, family = "binomial")
## il modello di regressione restituisce i seguenti errori:Warning messages:1: glm.fit: algorithm did not converge ,2: glm.fit: fitted probabilities numerically 0 or 1 occurred ##
## Pobabilmente ci sarà un'alta correlazione tra le variabili##
##Nella random forest, l'algoritmo di costruzione del modello utilizza un processo di bootstrapping per selezionare casualmente un sottoinsieme di osservazioni dal dataset di input e costruisce un albero di decisione per ogni sottoinsieme.## 
##Ciò significa che ogni albero della foresta viene costruito su una versione leggermente diversa del dataset originale, il che aiuta a ridurre l'overfitting e aumentare la generalizzazione del modello.##
##Poiché ogni albero della foresta viene costruito su un sottoinsieme di osservazioni casuale, la correlazione tra i predittori di solito non influenza significativamente la random forest. Tuttavia, se una determinata caratteristica ha un'alta correlazione con la variabile dipendente, potrebbe essere selezionata come predittore principale in molti alberi della foresta, il che può portare a un sovraaddestramento.##
## Qundi con l'approccio RF nessun problema##
cor_matrix <- df %>%
  select_if(is.numeric) %>%  # seleziona solo le colonne numeriche
  cor()  # calcola le correlazioni
print(cor_matrix)
# seleziona le colonne con correlazione maggiore di 0.8
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.8)
# visualizza le colonne altamente correlate
print(highly_correlated)
df2=df[,-c(8,9,7,29,28,24,22,4,25,27,14,15,17,6,3)]
df3=df2[,-c(3,12,13,7,8)]
df4=df3[,-c(9,5)]
train_split_idx <- caret::createDataPartition(df4$diagnosis, p = 0.70, list = FALSE)
train <- df4[train_split_idx, ]
test <- df4[-train_split_idx,]
model <- glm(diagnosis~., data =train, family = "binomial")
Lr_class <- predict(model, newdata = test, type = "response")
LR_class=ifelse(Lr_class > 0.5, "M", "B")
LR_class=as.factor(LR_class)
predictions <- cbind(data.frame(test_preds=LR_class,test$diagnosis))
predictions$test_preds=as.factor(predictions$test_preds)
cm <- caret::confusionMatrix(predictions$test_preds, predictions$test.diagnosis,mode="prec_recall")
print(cm)
##abbiamo ottenuto dei buoni risultati ma Rf resta superiore
