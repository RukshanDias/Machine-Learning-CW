#------------------------
# Rukshan Dias - w1912792
#------------------------

#install packages


#import packages
library(ggpubr)
library(neuralnet)
library(readxl)

#import dataset
data <- read_excel("G:/Other computers/My Computer/IIT/Level_02/2.ML/1. ML CW/uow_consumption.xlsx", sheet = 1) # opening the excel file with sheet no

####-----Preprocessing----####
# change col name
names(data)[2] <- 'six'
names(data)[3] <- 'seven'
names(data)[4] <- 'eight'

# change date to numeric
date <-factor(data$date)
date <-as.numeric(date)
date

View(data)

# create data frame
uow_dataFrame <- data.frame(date,data$'six',data$'seven',data$'eight')
View(uow_dataFrame)

boxplot(uow_dataFrame[,-1], main="Before normalizing data") #plot before normalizing data. without 1st col

# min-max normalizing function
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

# apply Normalization
uow_dataNorm <- as.data.frame(lapply(uow_dataFrame[,-1], normalize))# normalized data without date col
summary(uow_dataNorm)
boxplot(uow_dataNorm, main="After normalizing data") #plot after normalization


####-----Split dataset----####
traindata <- uow_dataNorm[1:380,] #data used for training model 
testdata <- uow_dataNorm[381:470,] #data used for testing model

####-----Training using AR----####
