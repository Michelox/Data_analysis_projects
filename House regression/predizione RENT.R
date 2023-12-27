library(tidyverse)  
library(tidyr) 
library(tidyselect)
library(plotly)
library(dplyr)
library(corrplot)
library(caTools)
library(htmlwidgets)
library(lubridate)
house_rent_data=House_Rent_Dataset
head(house_rent_data)
house_data<-drop_na(house_rent_data)
head(house_data)
dim(house_data)
summary(house_data)
str(house_data)
names(house_data)
sum(is.na(house_data))
#vediamo se c'è correlazione tra i predittori
cor_matrix<-cor(house_data[,c(2,3,4,11)])

cor_matrix

corrplot(cor_matrix, addCoef.col = TRUE)
#modellazione variabili
house_data$Posted.On <- ymd(house_data$Posted.On)

# Converti la variabile Posted.On da data a numerica usando as.numeric
house_data$Posted.On <- as.numeric(house_data$Posted.On)


# Plot dei dati
#Numero di BHK
BHK_count <- house_data %>% 
  group_by(BHK) %>% 
  summarise(Count = n())

# Plot del conteggio di BHK
ggplot(BHK_count, aes(x = BHK, y = Count)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Conteggio di BHK", x = "BHK", y = "Count")
#conteggio di Size
Size_count <- house_data %>% 
  group_by(Size) %>% 
  summarise(Count = n())

# Plot del conteggio di Size
ggplot(Size_count, aes(x = Size, y = Count)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.7) +
  labs(title = "Conteggio di Size", x = "Size", y = "Count")
#dimensioni medie per ogni categoria BHK
avg_size_BHK <- house_data %>% 
group_by(BHK) %>% 
  summarise(Avg_size = mean(Size))

# Plot delle dimensioni medie per ogni categoria BHK
ggplot(avg_size_BHK, aes(x = BHK, y = Avg_size)) +
  geom_bar(stat = "identity", fill = "orange", alpha = 0.7) +
  labs(title = "Dimensioni medie per categoria BHK", x = "BHK", y = "Avg Size")
#medie degli affitti per ogni categoria BHK
avg_rent_BHK <- house_data %>% 
  group_by(BHK) %>% 
  summarise(Avg_rent = mean(Rent))

# Plot delle medie degli affitti per ogni categoria BHK
ggplot(avg_rent_BHK, aes(x = BHK, y = Avg_rent)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.7) +
  labs(title = "Medie degli affitti per categoria BHK", x = "BHK", y = "Avg Rent")
#totale degli affitti per ogni categoria di area
Total_rent_area <- house_data %>% 
  group_by(Area.Type) %>% 
  summarise(Total_rent = sum(Rent))

# Plot del totale degli affitti per ogni categoria di area
ggplot(Total_rent_area, aes(x = Area.Type, y = Total_rent)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Totale degli affitti per categoria di area", x = "Area Type", y = "Total Rent")
#medie degli affitti per ogni categoria di area (ordinato)
Avg_rent_area <- house_data %>% 
  group_by(Area.Type) %>% 
  summarise(Avg_rent = mean(Rent)) %>%
  arrange(desc(Avg_rent))

# Plot delle medie degli affitti per ogni categoria di area (ordinato)
ggplot(Avg_rent_area, aes(x = reorder(Area.Type, -Avg_rent), y = Avg_rent)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.7) +
  labs(title = "Medie degli affitti per categoria di area (ordinato)", x = "Area Type", y = "Avg Rent") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#conteggio delle case per ogni categoria di area
count_house_area <- house_data %>% 
  group_by(Area.Type) %>% 
  summarise(Count = n())

# Plot del conteggio delle case per ogni categoria di area
ggplot(count_house_area, aes(x = reorder(Area.Type, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.7) +
  labs(title = "Conteggio delle case per categoria di area", x = "Area Type", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#conteggio delle case per le prime 5 località di area
count_house_area_lty <- house_data %>% 
  group_by(Area.Locality) %>% 
  summarise(Count = n()) %>%
  top_n(5)

# Plot del conteggio delle case per le prime 5 località di area
ggplot(count_house_area_lty, aes(x = reorder(Area.Locality, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "purple", alpha = 0.7) +
  labs(title = "Conteggio delle case per le prime 5 località di area", x = "Area Locality", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Preparazione dei dati per l'addestramento
library(dplyr)
house_data= house_data[,-c(5,7,8)]
aggregate_categories <- function(column, threshold = 2) {
  counts <- table(column)
  categories_to_aggregate <- names(counts[counts < threshold])
  column_aggregated <- fct_collapse(column, "Other" = categories_to_aggregate)
  return(column_aggregated)
}

# Applica l'aggregazione alle colonne specificate
columns_to_encode <- c("Area.Type", "Furnishing.Status", "Tenant.Preferred", "Point.of.Contact")

for (col in columns_to_encode) {
  house_data[[col]] <- aggregate_categories(house_data[[col]])
}

# Visualizza il dataframe con categorie aggregate
print(house_data)

# Crea le variabili dummy per le nuove colonne aggregate
encoded_data <- model.matrix(~ . - 1, data = house_data)

# Unisco il nuovo dataframe codificato con il dataframe originale
final_data <- cbind(house_data, encoded_data)
final_data$Area.Type=as.numeric(final_data$Area.Type)
final_data$Point.of.Contact=as.numeric(final_data$Point.of.Contact)
final_data$Furnishing.Status=as.numeric(final_data$Furnishing.Status)
final_data$Tenant.Preferred=as.numeric(final_data$Tenant.Preferred)
final_data <- final_data %>%
  select(-which(duplicated(t(.))))

# Stampa le prime righe del nuovo dataset
head(final_data)
final_data$log_Rent <- log(final_data$Rent)
final_data=final_data[,-c(3)]#levo la colonna Rent
final_data <- na.omit(final_data)
final_data=final_data[,-c(10,11,13,14,15,16)]#levo le variabili prima della codifica dummy
final_data=final_data[,-c(11)]
#preparazione train set e test set
library(caret)

set.seed(15)
train_idx <- createDataPartition(final_data$log_Rent, p = 0.8, list = F)
train <- final_data[train_idx, ]
test <- final_data[-train_idx, ]


head(train)

head(test)
model<-lm(log_Rent~ ., data=train)

summary(model)
#residui
res<-residuals(model)

res<-as.data.frame(res)

head(res)
#predizioni
test$predicted_rent<-predict(model,test)

head(test)
v17 <- ggplot(test, aes(log_Rent, predicted_rent)) +
  geom_point(color = "red") +
  geom_smooth(color = 'black') +
  theme_bw() +
  labs(y = 'Predicted Rent', x = 'Actual Rent', title = 'Actual Rent by Predicted Rent')

# Trasformo il grafico ggplot in un grafico plotly
v17_plotly <- ggplotly(v17)

# Visualizzo il grafico plotly direttamente in R(cliccare su Viewer qui nella console in basso a destra)
v17_plotly
# Calcolo del Mean Squared Error (MSE)
mse <- mean((test$log_Rent - test$predicted_rent)^2)
print(paste("Mean Squared Error (MSE):", mse))

# Calcolo del coefficiente di determinazione (R-squared)
r_squared <- cor(test$log_Rent, test$predicted_rent)^2
print(paste("R-squared:", r_squared))