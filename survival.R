#wykresy kaplana meiera i modele coxa do analizy przeżycia pacjentów 
#z danych lung z pakietu surival
#przeprowadzenie eksploracyjnej analizy danych w celu ich zrozumienia 
#dodatkowo grupowanie danych za pomocą kmeans oraz hierarchical cluster 
#w celu optymalnego pograupowania obserwacji 
#potrzebnym do modelu coxa oraz wykresów kaplana meiera

library(survival)
library(survminer)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(corrplot)
library(cluster)

df <- survival::lung

###################
###Cleaning data###
###################

#print('Number of missing values:')
for (column_name in names(df)) {
  column_values <- df[[column_name]]
  lack <- sum(is.na(column_values)) 
  print(paste(column_name, ':',lack))
  #print(column_values)
}

df <- data[complete.cases(df), ]

for (column_name in names(df)) {
  column_values <- df[[column_name]]
  lack <- sum(is.na(column_values)) 
  print(paste(column_name, ':',lack))
  #print(column_values)
}


df$grupa <- ifelse(df$ph.karno > 60, "Grupa 1", "Grupa 2")


#########################
####Basic theoretical####
##Wykres Kaplana Meiera##
#########################


fit <- survfit(Surv(time, status) ~ grupa, data = df)
ggsurvplot(fit, data = df)

survdiff(Surv(time, status) ~ grupa, data = df)

# Model Coxa
cox_model <- coxph(Surv(time, status) ~ grupa, data = df)

# Podsumowanie modelu
summary(cox_model)

fit <- survfit(cox_model)
plot(fit, xlab = "Czas", ylab = "Przeżycie", lwd= 2, col = c('black', 'red', 'blue'))
legend("topright", legend = c("Grupa 1", "Grupa 2"), col = c("blue", "red"), lty = 1)

df['grupa'] <- NULL


#####################################################################################

#EDA (Eksploracyjna analiza danych)

czy_kategoryczna <- function(kolumna, u) {
  if (is.factor(kolumna)) {
    return(FALSE)
  }
  
  if (length(unique(kolumna)) <= u) {
    return(FALSE)
  }
  
  return(TRUE)
}


summary(df)

#Histogramy

par(mfrow = c(1, 1))
histogram_list <- list()

for (column_name in names(df)[1:(length(df)-1)]) {
  p <- ggplot(df, aes(x = !!sym(column_name))) +
    geom_histogram(binwidth = 1) +
    ggtitle(column_name)
  histogram_list[[column_name]] <- p
}

# Wyświetlenie wykresów obok siebie w jednym oknie
grid.arrange(grobs = histogram_list, ncol = 2)


#Boxploty

boxplot_list <- list()

for (column_name in names(df)[1:(length(df)-1)]) {
  if(czy_kategoryczna(df[[column_name]], 10)){
    p <- ggplot(df, aes(y = !!sym(column_name))) +
      geom_boxplot() +
      ggtitle(column_name)
    boxplot_list[[column_name]] <- p
  }

}

# Wyświetlenie wykresów obok siebie w jednym oknie
grid.arrange(grobs = boxplot_list, ncol = 2)


#wykresy punktowe + linie trendu 

dir.create("wykresy", showWarnings = FALSE)

# Pusta lista na wykresy
plot_list <- list()

for (column_name1 in names(df)[1:(length(df)-1)]) {
  if (czy_kategoryczna(df[[column_name1]], 2)) {
    for (column_name2 in names(df)[1:(length(df)-1)]) {
      if (czy_kategoryczna(df[[column_name2]], 2)) {
        p <- ggplot(df, aes(x = !!sym(column_name1), y = !!sym(column_name2))) +
          geom_point()
        
        # Zapisz wykres do pliku PNG
        file_name <- paste("wykresy", column_name1, "_vs_", column_name2, ".png", sep = "_")
        ggsave(filename = file_name, plot = p, width = 6, height = 4, units = "in")
      }
    }
  }
}



############
#dodawanie lini do danych (nie ma zależności liniowych)
############
ggplot(df, aes(x = zmienna1, y = zmienna2)) +
  geom_point() +
  geom_smooth(method = "lm")
############



#Wykresy kategorialne 
katplot_list <- list()

for (column_name in names(df)[1:(length(df)-1)]) {
  if (!czy_kategoryczna(df[[column_name]], 10)){
  p <- ggplot(df, aes(x = !!sym(column_name))) +
    geom_bar()
  katplot_list[[column_name]] <- p
  }
}

grid.arrange(grobs = katplot_list, ncol = 2)



#korelacje + heatmapy

numeric_columns <- df %>%
  select_if(is.numeric)

# Oblicz macierz korelacji na podstawie wybranych kolumn
cor_matrix <- cor(numeric_columns)

corrplot(cor_matrix, method = "color")


#####################################################################################



############
#Clustering#
############

df_matrix = as.matrix(df)

hierarchical_clusters <- hclust(dist(df_matrix))  # data_matrix to macierz danych
clusters <- cutree(hierarchical_clusters, k = 3)  # Podziel na 3 grupy


df$cluster_hierchical <- clusters  # Dodaj kolumnę z przypisanymi grupami

plot(hierarchical_clusters)

dfh = df 
dfh_matrix = as.matrix(dfh)

for (i in 2:10){
  dfh_matrix = as.matrix(dfh)
  hierarchical_clusters <- hclust(dist(dfh_matrix))
  clusters <- cutree(hierarchical_clusters, k = i)
  dfh[paste('cluster_',i, sep ='')] <- clusters
  
  
  formula <- as.formula(paste("Surv(time, status) ~",'cluster_',i,sep =''  ))
  cox_model <- coxph(formula, data = dfh)
  print(summary(cox_model))
 
  dfh[[paste('cluster_',i, sep ='')]] <- NULL
}

#Odpowiednia ilośc grup z pval < 0.05 to 10 dla grupowania hierarchicznego 

dfk = df 

for (i in 2:10) {
  dfk_matrix <- as.matrix(dfk)
  kmeans_clusters <- kmeans(dfk_matrix, centers = i)
  dfk[paste('cluster_', i, sep ='')] <- kmeans_clusters$cluster
  
  formula <- as.formula(paste("Surv(time, status) ~ cluster_", i,sep =''))
  cox_model <- coxph(formula, data = dfk)
  print(summary(cox_model))
  
  # Usuń kolumnę "cluster_i" z dataframe'u dfk
  dfk[, paste('cluster_', i, sep ='')] <- NULL
}

#Najmnieszja ilość grup dla kmeans to 3 z wartością pval <0.05 


###########################
###Wykres Kaplana Meiera###
#Hierarchiczne grupowanie##
###########################



df_matrix = as.matrix(df)

hierarchical_clusters <- hclust(dist(df_matrix)) 
clusters <- cutree(hierarchical_clusters, k = 10)  

df$cluster_hierchical <- clusters 

plot(hierarchical_clusters)



fit <- survfit(Surv(time, status) ~ cluster_hierchical, data = df)
ggsurvplot(fit, data = df)

survdiff(Surv(time, status) ~ cluster_hierchical, data = df)

# Model Coxa
cox_model <- coxph(Surv(time, status) ~ cluster_hierchical, data = df)

# Podsumowanie modelu
summary(cox_model)



###########################
###Wykres Kaplana Meiera###
##########Kmeans###########
###########################

df_matrix = as.matrix(df)

kmeans_clusters <- kmeans(dfk_matrix, centers = 3)

df$cluster_kmeans <- kmeans_clusters$cluster


fit <- survfit(Surv(time, status) ~ cluster_kmeans, data = df)
ggsurvplot(fit, data = df)

survdiff(Surv(time, status) ~ cluster_kmeans, data = df)

# Model Coxa
cox_model <- coxph(Surv(time, status) ~ cluster_kmeans, data = df)

# Podsumowanie modelu
summary(cox_model)






