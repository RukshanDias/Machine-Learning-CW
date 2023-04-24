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

###### scaling #########
dataScaled <- as.data.frame(scale(tempData))

boxplot(dataScaled, main = "Scaled dataset")

#------------- Determine no of cluster centres-----------------
## NBclust
NBcluster<-NbClust(dataScaled,
                   min.nc = 2,
                   max.nc = 10,
                   distance = "euclidean",
                   method = "kmeans")
#fviz_nbclust(NBcluster)
barplot(table(NBcluster$Best.n[1,]), # provide bar charts####
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen")


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
# Silhouette plot for k2
sil_k2 <- silhouette(k2$cluster, dist(dataScaled))
windows()
plot(sil_k2)

# Silhouette plot for k3
sil_k3 <- silhouette(k3$cluster, dist(dataScaled))
windows()
plot(sil_k3)

#--------------PCA---------------
