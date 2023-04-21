# -----------------------
# Rukshan Dias - w1912792
# -----------------------

# install libraries
#install.packages("readxl")

# importing libraries
library(readxl)

# read excel file
data <- read_excel("G:/Other computers/My Computer/IIT/Level_02/2.ML/1. ML CW/vehicles.xlsx", sheet = 1) #opening the excel file with sheet no
colnames(data)
df <- subset(data, select = -c(Samples,Class))

data
View(data)
str(data)
summary(data)

dim(data)
boxplot(df, main="initial dataset")

#-------outlier detection w/ Z score------------
boxplot(data, main="Before Outlier Removal")

# outlier removal


###### scaling #########
dataNormZ <- as.data.frame( scale(df))

boxplot(dataNormZ, main="Z score")
#heatmap(as.matrix(dataNormZ), main="Z score")

