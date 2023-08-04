##########
#Read data
##########

setwd("C:/Users/Szymon/Desktop/R gutowska")
data = read.csv2('przykladoweDane-Projekt.csv', sep = ';')
data = data.frame(data)
typeof(data)
df <- as.data.frame(data)
df
typeof(df)



###########
#Clear data
###########

#print('Number of missing values:')
for (column_name in names(df)) {
  column_values <- df[[column_name]]
  lack <- sum(is.na(column_values)) 
  print(paste(column_name, ':',lack))
  #print(column_values)
}

cleardata <- data[complete.cases(df), ]
cleardata

for (column_name in names(cleardata)) {
  column_values <- cleardata[[column_name]]
  lack <- sum(is.na(column_values)) 
  print(paste(column_name, ':',lack))
  #print(column_values)
}

#########
#Outliers
#########

library(ggplot2)
library(gridExtra)

par(mfrow = c(2, 4))  #Size of table with boxplots

for (column_name in names(cleardata)[4:length(cleardata)]) {
  boxplot(cleardata[[column_name]], main = paste("Boxplot for", column_name),
          ylab = column_name, xaxt = "n", outline = TRUE)
}

####################################
#characteristics for selected groups 
####################################

library(stats)

for (column_name in names(cleardata)[4:length(cleardata)]) {
  column_values <- cleardata[[column_name]]
  print(column_name)
  print(summary(column_values))
}

#Quota table
table(cleardata$grupa,cleardata$plec)


library(dplyr)


round_and_format <- function(x) {
  format(round(x, 2), nsmall = 2)
}


for (column_name in names(cleardata)[3:length(cleardata)]) {
  group_data <- cleardata %>%
    group_by(grupa) %>%
    summarise(
      count = n(),
      across(
        .cols = all_of(column_name),
        .fns = list(mean = ~round_and_format(mean(., na.rm = FALSE)),
                    sd = ~round_and_format(sd(., na.rm = TRUE)),
                    median = ~round_and_format(median(., na.rm=TRUE)))
      )
    )
  print(column_name)
  print(group_data)
}


      ##################
      #  BENCHAMRKING  #
      ##################


##################################
#conformity to normal distribution
##################################

KONTROLA <- with(cleardata, cleardata[grupa == "KONTROLA", ])
CHOR1 <- with(cleardata, cleardata[grupa == "CHOR1", ])
CHOR2 <- with(cleardata, cleardata[grupa == "CHOR2", ])

#list with columns without normal distribution
non_normal = c()

for (column_name in names(cleardata)[4:length(cleardata)]) {
  pval_shapiro <- cleardata %>%
    group_by(grupa) %>%
    mutate(
      shapiro_statistic = shapiro.test(!!as.symbol(column_name))$statistic,
      p_value = shapiro.test(!!as.symbol(column_name))$p.value
    ) %>%
    select(grupa, shapiro_statistic, p_value) %>%
    distinct()
  
  print(pval_shapiro)
  
  for( i in pval_shapiro$p_value){
    if(i<0.05){
      non_normal = append(non_normal, column_name)
    }
  }
  
}

non_normal <- unique(non_normal)
non_normal


###############
#Visualization#
###############

ggdensity( cleardata, x = "LEU" ,
                   color = "grupa" ,
                   fill = "grupa" ,
                   palette= c ( "#99cc00" , "#660099" , "#0047b3" ) ,
                   ylab = "gestosc" ,
                   xlab = "hsCRP [mg/ l ] "
                   )




x_columns <- names(cleardata)[3:length(cleardata)]


plot_list <- list()


for (x_column in x_columns) {
  ggdensity_plot <- ggplot(cleardata, aes_string(x = x_column, color = "grupa", fill = "grupa")) +
    geom_density(alpha = 0.5) +
    scale_color_manual(values = c("#99cc00", "#660099", "#0047b3")) +
    scale_fill_manual(values = c("#99cc00", "#660099", "#0047b3")) +
    ylab("density") +
    xlab("hsCRP [mg/ l]") +
    labs(title = paste("Density plot for", x_column)) +
    theme_minimal()
  
  plot_list <- c(plot_list, list(ggdensity_plot))
}


grid.arrange(grobs = plot_list, ncol = 2)


##################
#homoscedasticity#
##################

library(car)

leveneTest(hsCRP~grupa, data = cleardata)

leveneTest(hsCRP~grupa, data = cleardata)$"Pr(>F)"[1] 

#list of columns without homoscedasticity
non_homoscedasticity = c()

for (column_name in names(cleardata)[4:length(cleardata)]) {
  result <- leveneTest(as.formula(paste(column_name, "~ grupa")), data = cleardata)
  p_value <- result$"Pr(>F)"[1]
  print(paste("P-value for", column_name, ":", p_value))
  if(p_value<0.05){
    non_homoscedasticity.append(column_name)
  }
}

non_homoscedasticity

result_anova = c()
result_kruskal = c()

for (column_name in names(cleardata)[4:length(cleardata)]) {
  if (!(column_name %in% non_normal) && !(column_name %in% non_homoscedasticity)) {
    print(column_name)
    anova_result <- aov(as.formula(paste(column_name, "~ grupa")), data = cleardata)
    summary_result <- summary(anova_result)
    print(summary_result)
    pvalanova <- summary_result[[1]][["Pr(>F)"]][[1]]
    if (pvalanova < 0.05) {
      result_anova <- append(result_anova, column_name)
    }
  }
  if ((column_name %in% non_normal) && !(column_name %in% non_homoscedasticity)) {
    print(column_name)
    kruskal_result <- kruskal.test(as.formula(paste(column_name, "~ grupa")), data = cleardata)
    summary_result <- summary(kruskal_result)
    print(summary_result)
    pvalkruskal <- kruskal.test(as.formula(paste(column_name, "~ grupa")), data = cleardata)$p.value
    if (pvalkruskal < 0.05) {
      result_kruskal <- append(result_kruskal, column_name)
    }
  }
}

result_anova
result_kruskal


for(i in result_anova){
  print(TukeyHSD(aov(as.formula(paste(i, "~ grupa")), data = cleardata)))
}

library(dunn.test)

for(i in result_kruskal){
  print(dunn.test(cleardata[[i]],cleardata$grupa ))
}




####################
#corelation analysis
####################

CHOR1 <- cleardata %>%filter( grupa == "CHOR1" )
CHOR2 <- cleardata %>%filter( grupa == "CHOR2" )
KONTROLA <- cleardata %>%filter( grupa == "KONTROLA" )


resultpearson<- cor.test(CHOR1$HGB, CHOR1$ERY, method = "pearson" )
resultpearson$p.value
resultpearson
library(ggpubr)

ggscatter(KONTROLA, x = "HGB" , y = "ERY" ,
           add = "reg.line", conf.int = TRUE,
           cor.coef = TRUE, cor.method = "pearson" ,
           color = "grupa" , fill = "grupa" ,
           ylab = "HGB [gl/dl] " ,
           xlab = "ERY [t/l] "
)


corelation_chor1 <- character()

for (i in 4:length(cleardata)) {
  for (j in 4:length(cleardata)) {
    if (i != j) {
      method = ''
      if(shapiro.test(CHOR1[[i]])$p.value>0.05 && shapiro.test(CHOR1[[j]])$p.value>0.05){
      result_cor <- cor.test(CHOR1[[i]], CHOR1[[j]], method = "pearson")  
      method = 'pearson'
      }
      if(shapiro.test(CHOR1[[i]])$p.value<0.05 || shapiro.test(CHOR1[[j]])$p.value<0.05){
      result_cor <- cor.test(CHOR1[[i]], CHOR1[[j]], method = "spearman")  
      method = 'spearman'
      }
      corr_text <- paste(names(CHOR1)[i], ":", names(CHOR1)[j], method, '\n', "p-value:", result_cor$p.value, '\n', "correlation:", result_cor$estimate,'\n')
      #print(corr_text)
      
      if (result_cor$p.value < 0.05) {
        corelation_chor1 <- c(corelation_chor1, corr_text)  
      }
    }
  }
}


cat(corelation_chor1)

corelation_chor2 <- character()

for (i in 4:length(cleardata)) {
  for (j in 4:length(cleardata)) {
    if (i != j) {
      method = ''
      if(shapiro.test(CHOR2[[i]])$p.value>0.05 && shapiro.test(CHOR2[[j]])$p.value>0.05){
        result_cor <- cor.test(CHOR2[[i]], CHOR2[[j]], method = "pearson")  
        method = 'pearson'
      }
      if(shapiro.test(CHOR2[[i]])$p.value<0.05 || shapiro.test(CHOR2[[j]])$p.value<0.05){
        result_cor <- cor.test(CHOR2[[i]], CHOR2[[j]], method = "spearman")  
        method = 'spearman'
      }
      corr_text <- paste(names(CHOR2)[i], ":", names(CHOR2)[j], method, '\n', "p-value:", result_cor$p.value, '\n', "correlation:", result_cor$estimate,'\n')
      #print(corr_text)
      
      if (result_cor$p.value < 0.05) {
        corelation_chor2 <- c(corelation_chor2, corr_text)  
      }
    }
  }
}


cat(corelation_chor2)

corelation_kontrola <- character()

for (i in 4:length(cleardata)) {
  for (j in 4:length(cleardata)) {
    if (i != j) {
      method = ''
      if(shapiro.test(KONTROLA[[i]])$p.value>0.05 && shapiro.test(KONTROLA[[j]])$p.value>0.05){
        result_cor <- cor.test(KONTROLA[[i]], KONTROLA[[j]], method = "pearson")  
        method = 'pearson'
      }
      if(shapiro.test(KONTROLA[[i]])$p.value<0.05 || shapiro.test(KONTROLA[[j]])$p.value<0.05){
        result_cor <- cor.test(KONTROLA[[i]], KONTROLA[[j]], method = "spearman")  
        method = 'spearman'
      }
      corr_text <- paste(names(KONTROLA)[i], ":", names(KONTROLA)[j], method, '\n', "p-value:", result_cor$p.value, '\n', "correlation:", result_cor$estimate,'\n')
      #print(corr_text)
      
      if (result_cor$p.value < 0.05) {
        corelation_kontrola <- c(corelation_kontrola, corr_text)  
      }
    }
  }
}


cat(corelation_kontrola)


##########
#HEATMAPS#
##########



calculate_correlation_matrix <- function(data) {
  n <- ncol(data)
  cor_matrix <- matrix(NA, n, n)
  for (i in 4:n) {
    for (j in 4:n) {
      if (i != j) {
        methoda <- ifelse(shapiro.test(data[[i]])$p.value > 0.05 && shapiro.test(data[[j]])$p.value > 0.05, "pearson", "spearman")
        cor_result <- cor.test(data[[i]], data[[j]], method = methoda)
        cor_matrix[i, j] <- cor_result$estimate
      }
    }
  }
  rownames(cor_matrix) <- colnames(cor_matrix) <- names(data)
  return(cor_matrix)
}

cor_matrix_chor1 <- calculate_correlation_matrix(CHOR1)
cor_matrix_chor2 <- calculate_correlation_matrix(CHOR2)
cor_matrix_kontrola <- calculate_correlation_matrix(KONTROLA)

library(ggplot2)
library(reshape2)

cor_matrix_chor1_melted <- subset(melt(cor_matrix_chor1), as.numeric(Var1) > 4 & as.numeric(Var2) > 4)
cor_matrix_chor2_melted <- subset(melt(cor_matrix_chor2), as.numeric(Var1) > 4 & as.numeric(Var2) > 4)
cor_matrix_kontrola_melted <- subset(melt(cor_matrix_kontrola), as.numeric(Var1) > 4 & as.numeric(Var2) > 4)


ggplot(cor_matrix_chor1_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Heatmap corelation for CHOR1", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(cor_matrix_chor2_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Heatmap corelation for CHOR2", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(cor_matrix_kontrola_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Heatmap corelation for KONTROLA", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


