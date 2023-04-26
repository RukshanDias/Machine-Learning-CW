# -----------------------
# Rukshan Dias - w1912792
# -----------------------

# install libraries
# install.packages("readxl")

# importing libraries
library(readxl)
library(factoextra)
library(ggfortify)
library(cluster)
library(NbClust)

# read excel file
data <- read_excel("G:/Other computers/My Computer/IIT/Level_02/2.ML/1. ML CW/vehicles.xlsx", sheet = 1) # opening the excel file with sheet no
colnames(data) # getting columns names
df <- subset(data, select = -c(Samples, Class)) # new data var without sample and class columns
dataColumns <- colnames(df) # getting new columns names

data
View(data)
str(data)
summary(data)

#--- missing values check----
sum(is.na(df))
summary(is.na(df))

dim(data)
boxplot(df, main = "initial dataset")

#-------outlier detection------------
boxplot(df, main = "Detect outliers / Before Outlier Removal", outcol="red")
legend("topright", c("outliers"), border="black", fill = c("red"))

tempData <- df
for (col in names(tempData)) {
  if (is.numeric(tempData[[col]])) {
    value = tempData[[col]][tempData[[col]] %in% boxplot.stats(tempData[[col]])$out]
    tempData[tempData[[col]] %in% value, col] = NA
  }
  outlier_count <- length(value)
  print(paste("No.of outliers detected in column",col, ":", outlier_count))
}

boxplot(tempData, main = "After Outlier Removal", outcol="red")

#####   remove missing values  ######
tempData <- na.omit(tempData)
nrow(tempData)
###### scaling #########
dataScaled <- scale(tempData)

boxplot(dataScaled, main = "Scaled dataset")

#------------- Determine no of cluster centres-----------------
set.seed(1234)
## NBclust function
viewNbCluster <- function(data){
  NBcluster<-NbClust(data,
                     min.nc = 2,
                     max.nc = 10,
                     distance = "euclidean",
                     method = "kmeans")
  
  barplot(table(NBcluster$Best.n[1,]), # provide bar chart
          xlab="Numer of Clusters",
          ylab="Number of Criteria",
          main="Number of Clusters Chosen")
}

## NBclust value
viewNbCluster(dataScaled)

## Elbow method
fviz_nbclust(dataScaled,kmeans,method = "wss", k.max = 10)

## Gap statistics
fviz_nbclust(dataScaled,kmeans,method = "gap_stat")

## Silhouette method
fviz_nbclust(dataScaled,kmeans,method = "silhouette")


#-------------- K-means clustering investigation -----------------
# calculate internal evaluation metrics
calculateWssBss <- function(kmeans){
  # Calculate WSS
  wss <- sum(kmeans$withinss)
  
  # Calculate BSS
  centers <- kmeans$centers
  dist_centers <- dist(centers)^2
  bss <- sum(kmeans$size * dist_centers)
  
  # Calculate TSS
  tss <- sum(dist(data)^2)
  
  # BSS/TSS ratio
  bssOverTss <- bss / tss
  
  # Print WSS, BSS, and TSS to console
  #cat("No.of clusters:", centers, "\n")
  cat("WSS:", wss, "\n")
  cat("BSS:", bss, "\n")
  cat("TSS:", tss, "\n")
  cat("BSS / TSS ratio:", bssOverTss, "\n")
}

# for 2 clusters
k2 <-kmeans(dataScaled, 2)
autoplot(k2,dataScaled,frame=TRUE)
calculateWssBss(k2)

# for 3 clusters
k3 <-kmeans(dataScaled, 3)
autoplot(k3,dataScaled,frame=TRUE)
calculateWssBss(k3)

#--------------silhouette plot------------------

#Silhouette plot function
viewSilhouettePlot <- function(kmeans, data){
  sil <- silhouette(kmeans$cluster, dist(data))
  windows()
  plot(sil)
}

# Silhouette plot for k2
viewSilhouettePlot(k2, dataScaled)

# Silhouette plot for k3
viewSilhouettePlot(k3, dataScaled)


#--------------PCA---------------
# pca <- prcomp(dataScaled, center = TRUE)
# summary(pca)
# 
# # View the eigenvalues and eigenvectors
# # pca$rotation
# # pca$sdev^2
# 
# # View the cumulative score per principal components (PC)
# cumsum(pca$sdev^2 / sum(pca$sdev^2))
# 
# # Extract the principal components with a cumulative score > 92%
# cum_score <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
# plot(cum_score, type = "b")
# 
# #pc_scores <- as.data.frame(pca$x[, cum_score > 0.92]) # transform & filtering
# pc_scores <- which(cum_score > 0.92)
# data_transformed <- predict(pca, newdata = dataScaled)[, pc_scores]
# data_pca_transformed <- as.data.frame(data_transformed)
# 
# View(pc_scores)
# View(data_transformed)
# boxplot(pc_scores, main="after PCA")
# boxplot(data_transformed, main="after PCA2")
# 
# pc_scores <- data_pca_transformed

#---------new PCA test-----
# Perform PCA analysis
pca <- prcomp(dataScaled)
View(pca)
# Print eigenvalues and eigenvectors
print(summary(pca))

# Calculate cumulative score per principal components (PC)
pca_var <- pca$sdev^2
pca_var
pca_var_prop <- pca_var / sum(pca_var)
pca_var_prop
pca_var_cumprop <- cumsum(pca_var_prop)
pca_var_cumprop

# Plot cumulative score per PC
plot(pca_var_cumprop, xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

View(dataScaled)
# Create a new transformed dataset with principal components as attributes
pca_trans <- predict(pca, newdata = dataScaled)

# Choose PCs that provide at least cumulative score > 92%
selected_pcs <- which(pca_var_cumprop > 0.92)
transformed_data <- pca_trans[, selected_pcs]
boxplot(transformed_data)
View(transformed_data)

pc_scores <- transformed_data
#------------optimal clusters after PCA------------
## NBclust value
viewNbCluster(pc_scores)

## Elbow method
fviz_nbclust(pc_scores,kmeans,method = "wss", k.max = 10)

## Gap statistics
fviz_nbclust(pc_scores,kmeans,method = "gap_stat")

## Silhouette method
fviz_nbclust(pc_scores,kmeans,method = "silhouette")

#-----------Kmeans clustering after PCA--------
# for 2 clusters
k2 <-kmeans(pc_scores, 2)
autoplot(k2,pc_scores,frame=TRUE)
calculateWssBss(k2)

# for 3 clusters
k3 <-kmeans(pc_scores, 3)
autoplot(k3,pc_scores,frame=TRUE)
calculateWssBss(k3)

# for 4 clusters
k4 <-kmeans(pc_scores, 4)
autoplot(k4,pc_scores,frame=TRUE)
calculateWssBss(k4)

#--------------silhouette plot after PCA------------------

# Silhouette plot for k2
viewSilhouettePlot(k2, pc_scores)

# Silhouette plot for k3
viewSilhouettePlot(k3, pc_scores)