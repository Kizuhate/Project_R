# ------------------------------------------------MAIN SECTION-------------------------------------------

library(readxl)
library(cluster)
library(ggplot2)
library(factoextra)
library(psych)
library(rgl)
library(reshape2)

# Load data from Excel file
dane <- read.csv("dane1.csv", header = TRUE, sep = ";", row.names = 1, dec = ",")

# Display data
print(dane)


# Basic descriptive statistics
summary_stats <- summary(dane)
print("Basic descriptive statistics:")
print(summary_stats)

# Mode calculation
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
moda_values <- sapply(dane, function(x) {
  if(is.numeric(x)) {
    moda(x)
  } else {
    NA
  }
})
print("Mode for variables:")
print(moda_values)

# Range (min-max)
range_values <- sapply(dane, function(x) {
  if(is.numeric(x)) {
    range(x, na.rm = TRUE)
  } else {
    NA
  }
})
print("Range (min-max):")
print(range_values)

# Interquartile range (IQR)
iqr_values <- sapply(dane, function(x) {
  if(is.numeric(x)) {
    IQR(x, na.rm = TRUE)
  } else {
    NA
  }
})
print("Interquartile range (IQR):")
print(iqr_values)

# Skewness and kurtosis
library(moments)
skewness_values <- sapply(dane, function(x) {
  if(is.numeric(x)) {
    skewness(x, na.rm = TRUE)
  } else {
    NA
  }
})
print("Skewness:")
print(skewness_values)

kurtosis_values <- sapply(dane, function(x) {
  if(is.numeric(x)) {
    kurtosis(x, na.rm = TRUE)
  } else {
    NA
  }
})
print("Skewness:")
print(kurtosis_values)


# Coefficients of variation (CV)
print("Coefficients of variation (CV):")
cv <- sapply(dane, function(x) {
  if(is.numeric(x)) {
    sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
  } else {
    NA  
  }
})
print(cv)

# Correlation matrix
print("Correlation matrix:")
cor_matrix <- cor(dane, use="complete.obs")
print(cor_matrix)

# Variable correlation plot
print("Variable correlation plot (heatmap):")

# Outliers in the data
print("Outliers in the data:")
outliers <- lapply(dane, function(x) {
  if(is.numeric(x)) {
    boxplot.stats(x)$out  
  } else {
    NA
  }
})
print(outliers)

# -------------------------------------DATA CHARTS AND BASIC STATISTICS---------------------------------


# Histograms for each variable
print("Histograms for each variable:")
par(mfrow = c(2, 3))  
for (col in colnames(dane)) {
  if (is.numeric(dane[[col]])) {
    hist(dane[[col]], main = paste("Histogram -", col), xlab = col, col = "lightblue", border = "black")
  }
}
par(mfrow = c(1, 1))  

# Boxplots
print("Boxplots:")
par(mfrow = c(2, 3))
for (col in colnames(dane)) {
  if (is.numeric(dane[[col]])) {
    boxplot(dane[[col]], main = paste("Boxplot -", col), col = "lightgreen", border = "black")
  }
}
par(mfrow = c(1, 1))

# Scatterplot matrix
print("Scatterplot matrix:")
pairs(dane, main = "Scatterplot matrix", pch = 19, col = rgb(0.2, 0.4, 0.6, 0.5))

# Correlation heatmap

melted_cor <- melt(cor_matrix)
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  ggtitle("Correlation Heatmap")






# ------------------------------------PREPARING DATA FOR ANALYSIS----------------------------------------

# Converting nominant to stimulant
WarOpt = median(dane$fertility)
for(i in 1:nrow(dane)){
  if(dane[i, "fertility"] == WarOpt){
    dane[i, "fertility"] = 1
  }else if(dane[i, "fertility"] < WarOpt){
    dane[i, "fertility"] = -1/(dane[i, "fertility"]-WarOpt-1)
  }else{
    dane[i, "fertility"] = 1/(dane[i, "fertility"]-WarOpt+1)
  }
}
# Converting destimulant to stimulant

dane$infantMortality <- -1*dane$infantMortality

# Displaying transformed data
dane

# Standardizing data
dane_st <- as.data.frame(scale(dane))
dane_st

# -----------------------------------LINEAR ORDERING BY HELLWIG'S METHOD---------------------------------------

# Hellwig's method
wzorzec <- c(max(dane_st[,"fertility"]),
             max(dane_st[,"ppgdp"]),
             max(dane_st[,"lifeExpF"]),
             max(dane_st[,"pctUrban"]),
             max(dane_st[,"infantMortality"]))
wzorzec

# Calculating object distances from the pattern
odl <- dane_st
for(i in 1:nrow(dane_st)){
  odl[i,] <- (dane_st[i,]-wzorzec)^2
}

odl$odlSuma <- rowSums(odl)
odl$odlSuma <- sqrt(odl$odlSuma)
odl

# Determining the 'possibly distant' distance
d0 <- mean(odl$odlSuma) + 2 * sd(odl$odlSuma)
d0

# Hellwig's measure
HELLWIG <- data.frame(Nation = rownames(dane_st),
                      Miara = 1-odl$odlSuma/d0)
HELLWIG

HELLWIG[order(HELLWIG$Miara, decreasing = TRUE), ]

# --------------------------LINEAR ORDERING - STANDARDIZED SUM METHOD-----------------------------


# Calculation of the standardized value sum
ranking_ss <- rowSums(dane_st, na.rm = TRUE)

# Creating the ranking (from highest value)
ranking_ss <- data.frame(Nation = rownames(dane_st),
                         Miara_SS = ranking_ss)

# Standardization of Measure_SS
ranking_ss$Miara_SS <- scale(ranking_ss$Miara_SS)

# Sorting the ranking from best results
ranking_ss <- ranking_ss[order(ranking_ss$Miara_SS, decreasing = TRUE), ]

# Displaying the ranking
print("Ranking by the standardized sum method:")
print(ranking_ss)


# Charts

# Bar chart of the ranking by Hellwig's method
print("Hellwig ranking chart:")
ggplot(HELLWIG, aes(x = reorder(Nation, Miara), y = Miara)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Hellwig Ranking") +
  xlab("Countries") +
  ylab("Hellwig Measure")

# Bar chart of the ranking by the standardized sum method
print("Ranking chart by the standardized sum method:")
ggplot(ranking_ss, aes(x = reorder(Nation, Miara_SS), y = Miara_SS)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Ranking by the standardized sum method") +
  xlab("Countries") +
  ylab("Measure SS")

# Scatter plot comparing Hellwig and SS rankings
print("Comparison of Hellwig and SS rankings:")
ggplot() +
  geom_point(aes(x = HELLWIG$Miara, y = ranking_ss$Miara_SS), color = "purple") +
  theme_minimal() +
  ggtitle("Comparison of Hellwig and SS rankings") +
  xlab("Hellwig Measure") +
  ylab("Measure SS")




# -----------------------------------------CLUSTER ANALYSIS-------------------------------------------------

# K-medoids method


# --- K-MEDOID FOR 2 GROUPS ---


pam_result_2 <- pam(dane_st, k = 2, metric = "euclidean")
pam_result_2$clustering

dane$pam_result_2 <- as.factor(pam_result_2$clustering)
describeBy(dane[,1:5], group = dane$pam_result_2)

fviz_cluster(pam_result_2, dane[,1:5], geom = "text")

# --- K-MEDOID FOR 3 GROUPS ---

pam_result_3 <- pam(dane_st, k = 3, metric = "euclidean")
pam_result_3$clustering

dane$pam_result_3 <- as.factor(pam_result_3$clustering)
describeBy(dane[,1:5], group = dane$pam_result_3)

fviz_cluster(pam_result_3, dane[,1:5], geom = "text")

# Checking the optimal number of groups
# Elbow method
fviz_nbclust(dane_st, pam, method = "wss")

# Silhouette method
fviz_nbclust(dane_st, pam, method = "silhouette")

# Calculation of average feature values for each group
aggregate(dane[,1:5], by = list(Cluster = dane$pam_result_2), FUN = mean)
aggregate(dane[,1:5], by = list(Cluster = dane$pam_result_3), FUN = mean)


# ----------------------------------------- MULTIDIMENSIONAL SCALING (MDS) -----------------------------------------

# Classical multidimensional scaling (MDS)
mds_result <- cmdscale(dist(dane_st), k = 2)  

# Creating a data frame with MDS results
mds_df <- data.frame(X = mds_result[,1], Y = mds_result[,2], Cluster = as.factor(pam_result_2$clustering))

# 2D Plot
plot(mds_df$X, mds_df$Y, col = as.numeric(mds_df$Cluster), pch = 19, 
     xlab = "MDS Dimension 1", ylab = "MDS Dimension 2", 
     main = "Visualization of cluster analysis results (MDS)")
text(mds_df$X, mds_df$Y, labels = rownames(dane), pos = 3, cex = 0.7)
legend("topright", legend = levels(mds_df$Cluster), col = 1:length(levels(mds_df$Cluster)), pch = 19)

# Classical multidimensional scaling (MDS) to 3 dimensions
mds_result_3d <- cmdscale(dist(dane_st), k = 3)  

# Creating a data frame with MDS results
mds_df_3d <- data.frame(
  X = mds_result_3d[,1], 
  Y = mds_result_3d[,2], 
  Z = mds_result_3d[,3], 
  Cluster = as.factor(pam_result_2$clustering)  
)

# Creating a 3D plot
plot3d(mds_df_3d$X, mds_df_3d$Y, mds_df_3d$Z, 
       col = as.numeric(mds_df_3d$Cluster), 
       size = 1, type = "s", xlab = "Dimension 1", 
       ylab = "Dimension 2", zlab = "Dimension 3")

# Adding country labels
text3d(mds_df_3d$X, mds_df_3d$Y, mds_df_3d$Z, 
       texts = rownames(dane), col = "black", cex = 0.7)

