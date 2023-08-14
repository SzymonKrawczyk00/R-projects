setwd("C:/Users/Szymon/Desktop/universal data analysis")
data = read.csv2('Heart.csv', sep = ',')
data <- data.frame(data)

############
#Clear data#
############

print('Number of missing values:')
for (column_name in names(data)) {
  column_values <- data[[column_name]]
  lack <- sum(is.na(column_values)) 
  print(paste(column_name, ':',lack))
  #print(column_values)
}

clean<- data[complete.cases(data), ]
clean

for (column_name in names(clean)) {
  column_values <- clean[[column_name]]
  lack <- sum(is.na(column_values)) 
  print(paste(column_name, ':',lack))
  #print(column_values)
}

cleany <-  clean$HeartDisease
clean <- subset(data, select = -HeartDisease)
############
#Conversion#
############



for (column_name in names(clean)) {
  if (sum(is.na(as.numeric(clean[[column_name]]))) > 0) {
    cat("Converting column", column_name, "to numeric.\n")
    
    faktor <- factor(clean[[column_name]], levels = unique(clean[[column_name]]), labels = 1:length(unique(clean[[column_name]])))
    clean[[column_name]] <- as.integer(faktor)
  }
}


############
#Elbow Plot#
############

library(cluster) 
library(ggplot2) 

wcss <- numeric(length = 10)

for (i in 1:10) {
  kmeans_model <- kmeans(clean, centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}


ggplot(data.frame(K = 1:10, WCSS = wcss), aes(x = K, y = WCSS)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Clusters (K)", y = "Within-Cluster Sum of Squares (WCSS)") +
  ggtitle("Elbow Method for Optimal K") +
  theme_minimal()


########
#Kmeans#
########


num_clusters <- 2

kmeans_model <- kmeans(clean, centers = num_clusters)

library(dplyr)
clean$ID <- rownames(clean)



####################
#Contingency matrix#
####################


contingency_matrix <- table(kmeans_model$cluster, cleany)

print(contingency_matrix)

ggplot(clean, aes(x = rownames(clean), y =MaxHR, color = factor(kmeans_model$cluster))) +
  geom_point() +
  labs( title = "K-Means Clustering") +
  scale_color_discrete(name = "Cluster")

library(rgl)

plot3d(clean$country, clean$income, clean$life_expec,
       col = rainbow(4)[kmeans_model$cluster],
       size = 5,
       xlab = rownames(clean),
       ylab = "Age",
       zlab = "MaxHR",
       main = "3D Scatter Plot for K-Means Clustering")

