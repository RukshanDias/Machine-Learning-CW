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
  bss <- kmeans$betweenss
  
  # Calculate TSS
  tss <- kmeans$totss
  
  # BSS/TSS ratio
  bssOverTss <- bss / tss
  
  # Print WSS, BSS, and TSS to console
  cat("No.of clusters:", length(unique(kmeans$cluster)), "\n")
  cat("WSS:", wss, "\n")
  cat("BSS:", bss, "\n")
  cat("TSS:", tss, "\n")
  cat("BSS TSS ratio:", bssOverTss, "\n")
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
  fviz_silhouette(sil)
}

# Silhouette plot for k2
viewSilhouettePlot(k2, dataScaled)

# Silhouette plot for k3
viewSilhouettePlot(k3, dataScaled)


#--------------PCA---------------
pca <- prcomp(dataScaled, center = TRUE)
summary(pca)

# View the eigenvalues and eigenvectors
pca$rotation
pca$sdev^2

# Extract the principal components with a cumulative score > 92%
cum_score <- cumsum(pca$sdev^2 / sum(pca$sdev^2))

# View the cumulative score per principal components (PC)
cum_score
plot(cum_score, type = "b", main = "PCA cumulative score")

#pc_scores <- as.data.frame(pca$x[, cum_score > 0.92]) # transform & filtering
pc_scores <- which(cum_score > 0.92)


########new####
allPCs <- c(1:pc_scores[1])
data_transformed <- predict(pca, newdata = dataScaled)[, allPCs:pc_scores[1]]

#data_transformed <- predict(pca, newdata = dataScaled)[, pc_scores]
dataTransformed <- as.data.frame(data_transformed)

# View(pc_scores)
View(dataTransformed)

boxplot(dataTransformed, main="after PCA2")


#------------optimal clusters after PCA------------
## NBclust value
viewNbCluster(dataTransformed)

## Elbow method
fviz_nbclust(dataTransformed,kmeans,method = "wss", k.max = 10)

## Gap statistics
fviz_nbclust(dataTransformed,kmeans,method = "gap_stat")

## Silhouette method
fviz_nbclust(dataTransformed,kmeans,method = "silhouette")

#-----------Kmeans clustering after PCA--------
set.seed(1234)
# for 2 clusters
pca_k2 <-kmeans(dataTransformed, 2)
autoplot(pca_k2,dataTransformed,frame=TRUE)
calculateWssBss(pca_k2)

# for 3 clusters
pca_k3 <-kmeans(dataTransformed, 3)
autoplot(pca_k3,dataTransformed,frame=TRUE)
calculateWssBss(pca_k3)

# for 4 clusters
pca_k4 <-kmeans(dataTransformed, 4)
autoplot(pca_k4,dataTransformed,frame=TRUE)
calculateWssBss(pca_k4)

#--------------silhouette plot after PCA------------------

# Silhouette plot for k2
viewSilhouettePlot(pca_k2, dataTransformed)

# Silhouette plot for k3
viewSilhouettePlot(pca_k3, dataTransformed)

# Silhouette plot for k3
viewSilhouettePlot(pca_k4, dataTransformed)

#-----------Calinski Harabasz Index----------
calculate_Calinski_Harabasz_Index <- function(kmeans, data){
  clusterCount <- length(unique(kmeans$cluster))
  rowCount <- nrow(data)
  BGSS <- kmeans$betweenss
  WGSS <- kmeans$tot.withinss
  
  CH_index <- (BGSS/WGSS) * ((rowCount-clusterCount)/ (clusterCount-1))
  cat("CH_index for",clusterCount, "clusters -> ", CH_index, "\n")
  return(CH_index)
}
ch_index_k2 <- calculate_Calinski_Harabasz_Index(k2, dataScaled)
ch_index_k3 <- calculate_Calinski_Harabasz_Index(k3, dataScaled)
ch_index_pca_k2 <- calculate_Calinski_Harabasz_Index(pca_k2, dataTransformed)
ch_index_pca_k3 <- calculate_Calinski_Harabasz_Index(pca_k3, dataTransformed)

