library(readxl)
library(randomForest)
library(pROC)
library(caret)
library(dplyr)
library(ggplot2)


df <- read_excel("C:/Users/Szymon/Desktop/mortality/mortality.xlsx")


#Czyszczenie danych 

for (column_name in names(df)) {
  column_values <- df[[column_name]]
  lack <- sum(is.na(column_values)) 
  print(paste(column_name, ':',lack))
  #print(column_values)
}

df <- df[complete.cases(df), ]

names(df) <- setNames(make.names(names(df)), names(df))

# Podział danych na zestaw treningowy i testowy



set.seed(123)  # Ustawienie ziarna dla powtarzalności
partition <- createDataPartition(df$Death, p = 0.7, list = FALSE) 

train_data <- df[partition, ]
test_data <- df[-partition, ]

#Random forest model

train_data$Death <- as.factor(train_data$Death)

# Dopasuj model Random Forest do zadania klasyfikacji


rf_model <- randomForest(formula = Death ~ ., data = train_data, importance = TRUE, proximity = TRUE)

# Wyświetl model
print(rf_model)

#Predykcje przy uzyciu random forest

predictions <- predict(rf_model, newdata = test_data)
predictions

accuracy <- sum(predictions == test_data$Death) / length(predictions)
accuracy




##############
#Ocena modelu#
##############

# Obliczenie dokładności
accuracy <- sum(predictions == test_data$Death) / length(predictions)
accuracy

#Macierz błędów
predictions <- as.factor(predictions)
testf_data <- test_data
testf_data$Death <- as.factor(test_data$Death)
conf_matrix <- confusionMatrix(predictions, testf_data$Death)
conf_matrix


#Krzywa ROC
roc_obj <- roc(testf_data$Death, as.numeric(predictions))
auc <- auc(roc_obj)

# Wyświetlenie krzywej ROC i AUC-ROC
plot(roc_obj, main = "Krzywa ROC")
print(paste("AUC-ROC:", auc))

####################
####Griid search#### 
####################

#Własna próba dostosowania parametrów 

nodesize <- c(5,10,20, 200, 500, 700, 1000, 1500,2000)
ntree = c(100, 500, 1000)
for (i in nodesize){
  for(i2 in ntree){
    
    rf_model <- randomForest(formula = Death ~ ., ntree = i2,  nodesize = i, data = train_data, importance = TRUE, proximity = TRUE)
    predictions <- predict(rf_model, newdata = test_data)
    accuracy <- sum(predictions == test_data$Death) / length(predictions)
    print(paste('nodeize: ', i, 'ntree: ', i2,'accuracy: ', accuracy))
    
  }
}

#Cross validation w celu znalezienia optymalnego mtry 
hyperparameters <- expand.grid(
  mtry = c(10, 20, 30, 40, 50,60,70,80)
)

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

model <- train(Death ~.,
               data = train_data, method = "rf", 
               trControl = train_control, 
               tuneGrid = hyperparameters)
model


#################################################################

#Dostosowany model

rf_model <- randomForest(formula = Death ~ ., xtest=xtest, ytest =ytest,ntree = 100, nodesize = 20,  data = train_data, importance = TRUE, proximity = TRUE)
rf_model <- randomForest(formula = Death ~ .,ntree = 100, nodesize = 20,  data = train_data, importance = TRUE, proximity = TRUE)
rf_model

predictions <- predict(rf_model, newdata = test_data)

accuracy <- sum(predictions == test_data$Death) / length(predictions)
accuracy





#Istotność parametrów 

feat_imp_df <- importance(rf_model) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

# plot dataframe
ggplot(feat_imp_df, aes(x = reorder(feature, MeanDecreaseGini), 
                        y = MeanDecreaseGini)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: <Model>"
  )







