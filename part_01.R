# -----------------------
# Rukshan Dias - w1912792
# -----------------------

# install libraries
# install.packages("readxl")

# importing libraries
library(readxl)

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
fviz_nbclust(NBcluster)

## Elbow method

## Gap statistics

## Silhouette method

#--------------silhouette plot------------------

#--------------PCA---------------
