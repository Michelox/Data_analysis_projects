library(stats)
library(dplyr)
library(randomForest)
library(caret)
library(e1071)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(partykit)
library(gridExtra)
library(reshape2)
df=KAG_energydata_complete
str(df)
library(lubridate)
df <- na.omit(df)
df$date <- ymd_hms(df$date)
# Distribution plot di 'Appliances' 
ggplot(df, aes(x = Appliances)) + 
geom_density(color = "green")
# Filtriamo i dati per includere solamente valori tra 0 e 99th percentile
df <- df[df$Appliances > quantile(df$Appliances, 0) & df$Appliances < quantile(df$Appliances, 0.99),]
# Boxplot di 'Appliances' 
ggplot(df, aes(x = "", y = Appliances)) + 
geom_boxplot(color = "green") +
xlab("") +
ylab("Appliances")
# Aggiungiamo nuove colonne chiamate: exact_date, hours, seconds and weekday
df$exact_date <- as.Date(df$date, "%Y-%m-%d")
df$exact_date=as.factor(df$exact_date)
df$hours <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S")
df$hours <- as.integer(format(df$hours, "%H"))
df$week <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S")
df$weekday <- as.integer(format(df$week, "%u"))
df$week <- weekdays(df$week)
# Aggiungiamo log_appliances, hour*lights and hour_avg columns
df$log_appliances <- log(df$Appliances)
df$hour_lights <- df$hours * df$lights
df$hour_avg <- ave(df$Appliances, df$hours, FUN = mean)
df$low_consum <- as.integer((df$Appliances + 25) < df$hour_avg)
df$high_consum <- as.integer((df$Appliances + 100) > df$hour_avg)
dates <- unique(df$exact_date) 
arranged_day <- factor(df$exact_date, levels=dates, ordered=TRUE)
table <- table(arranged_day, df$Appliances)
barplot(table, col=rainbow(nrow(table)),main="Appliances by Date", xlab="Date", ylab="Sum of Appliances")
df$log_appliances <- log(df$Appliances)
p1 <- ggplot(df, aes(x = Appliances)) +geom_histogram(fill = "blue", color = "black") +labs(title = "Appliance's consumption", x = "Appliances wH")
p2 <- ggplot(df, aes(x = log_appliances)) +geom_histogram(fill = "blue", color = "black") +labs(title = "Log Appliance's consumption", x = "Appliances log(wH)")
grid.arrange(p1, p2, ncol = 2)
col = c('log_appliances', 'lights', 'T1', 'RH_1', 'T2', 'RH_2', 'T3', 'RH_3', 'T4','RH_4', 'T5', 'RH_5', 'T6', 'RH_6', 'T7', 'RH_7', 'T8', 'RH_8', 'T9', 'RH_9', 'T_out', 'Press_mm_hg', 'RH_out', 'Windspeed', 'Visibility','Tdewpoint','hours')
df_subset <- df[col]
corr <- cor(df_subset)
corr_melted <- melt(corr)
ggplot(corr_melted, aes(Var1, Var2, fill = value)) + 
geom_tile(colour = "white") +
scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
midpoint = 0, limit = c(-1,1), name = "Correlation") + 
theme_minimal() + 
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
axis.text.y = element_text(angle = 45, vjust = 1, hjust = 1)) +
labs(x = "", y = "")
df2=df[,c(37,38,31,14,15,35,3,27,26,25,23,34)]
set.seed(15)
train_split_idx <- caret::createDataPartition(df2$log_appliances, p = 0.70, list = FALSE)
train <- df2[train_split_idx, ]
test <- df2[-train_split_idx,]
decision_tree_model <- rpart(log_appliances ~ ., data = train)
decision_tree_predictions <- predict(decision_tree_model, newdata = test)
mse <- mean((decision_tree_predictions - test$log_appliances)^2)
rsq <- 1 - (mse/var(test$log_appliances))
print(paste("MSE: ", mse))
print(paste("R-squared: ", rsq))
prp(decision_tree_model)
#codice usato per trovare l'mtry ottimale###
###mtry_values <- seq(1, ncol(train) - 1, by = 2) 
###results <- data.frame(mtry = numeric(), mse = numeric())
###for (mtry in mtry_values) {
##model <- train(log_appliances ~ ., data = train, method = "rf",
###trControl = trainControl(method = "cv", number = 5),
###tuneGrid = data.frame(mtry = mtry))
###results[results$mtry == mtry, "mse"] <- model$results[["Resample"]][["MSE"]]}
###best_mtry <- results[which.min(results$mse), "mtry"]
rf.df=randomForest(log_appliances~.,data=train,mtry=9,importance=TRUE,ntree=50)
rf_predictions=predict(rf.df,newdata=test)
pred_df <- data.frame(true = test$log_appliances, pred = rf_predictions)
mse <- mean(( rf_predictions- test$log_appliances)^2)
rsq <- 1 - (mse/var(test$log_appliances))
print(paste("MSE: ", mse))
print(paste("R-squared: ", rsq))
