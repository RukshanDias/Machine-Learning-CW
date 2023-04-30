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
# un-normalizing function
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min )
}

# apply Normalization
uow_dataNorm <- as.data.frame(lapply(uow_dataFrame[,-1], normalize))# normalized data without date col
summary(uow_dataNorm)
boxplot(uow_dataNorm, main="After normalizing data") #plot after normalization


####-----Split dataset----####
traindata <- uow_dataNorm[1:380,] #data used for training model 
testdata <- uow_dataNorm[381:470,] #data used for testing model

####-----Training using AR----####
trainModel <- function(count, formula, hiddenVal){
  traindata_input <- matrix( ,nrow = 0, ncol = count) #matrix to extract input values
  traindata_output <- c() #vector to store output value
  traindata_input
  
  
  for (i in 1:length(traindata$"data.eight")) {
    #print(i)
    last_val <- i + (count - 1) #find last record number for each input set
    
    #break loop if output value is out of bounds
    if (last_val+1 > length(traindata$"data.eight")) {
      break
    }
    length(traindata$"data.eight")
    #collect new records set to the existing records
    new_input <- traindata$"data.eight"[i:last_val] #store 1st 4 records of iteration as new input set
    new_output <- traindata$"data.eight"[last_val+1] #store 5th record as output value
    new_input
    
    traindata_input <- rbind(traindata_input, new_input) #add the new input vector to inputs matrix as a row
    traindata_output <- append(traindata_output, new_output) #add new output value to output vector
    traindata_input
  }
  
  #create a data frame with all inputs and outputs
  final_train_df <- cbind(as.data.frame(traindata_input), traindata_output)
  
  set.seed(123)
  
  #neural network training model with 2 hidden layers containing 10 neurons and 7 neurons 
  nn <- neuralnet(formula, 
                         data = final_train_df, hidden=hiddenVal, act.fct = "logistic", linear.output=TRUE)
  plot(nn)
  
  return(nn)
}

###------Testing--------####
testModel <- function(count, train_nn){
  testdata_input <- matrix( ,nrow = 0, ncol = count) #matrix to extract input values
  testdata_output <- c() #vector to store output value
  for (i in 1:length(testdata$"data.eight")) {
    end_value <- i + (count - 1) #find last record number for each input set
    #break loop if output value is out of bounds
    if (end_value+1 > length(testdata$"data.eight")) {
      break
    }
    
    #collect new records set to the existing records
    new_input <- testdata$"data.eight"[i:end_value] #store 1st 4 records of iteration as new input set
    new_output <- testdata$"data.eight"[end_value+1] #store 5th record as output value
    
    testdata_input <- rbind(testdata_input, new_input) #add the new input vector to inputs matrix  as a row
    testdata_output <- append(testdata_output, new_output) #add new output value to output vector
  }
  
  #print(testdata_input)
  
  #make test data a data frame
  testdata_df <- as.data.frame(testdata_input)
  #test the neural network with test data
  order4_nn_results <- compute(train_nn, testdata_df)
  results <- data.frame(actual = testdata_output, prediction = order4_nn_results$net.result)# results of NN
  results_min <- min(uow_dataFrame$"data.eight")
  results_max <- max(uow_dataFrame$"data.eight")
  
  
  #calculating accuracy 
  predicted = results$prediction * abs(diff(range(testdata_output))) + min(testdata_output)
  actual = results$actual * abs(diff(range(testdata_output))) + min(testdata_output)
  
  #----------function for un-normalizing data
  unnormalize <- function(x, min, max) {
    return( (max - min)*x + min )
  }
  
  comparison = data.frame(predicted,actual)
  unnormalizeed_results <- unnormalize(comparison, results_min, results_max)
  deviation = ((unnormalizeed_results$actual-unnormalizeed_results$predicted)/unnormalizeed_results$actual)
  comparison=data.frame(unnormalizeed_results$predicted,unnormalizeed_results$actual,deviation)
  accuracy = 1 - abs(mean(deviation))
  accuracy
  
  
  #get RMSE value
  pred_RMSE <- rmse(unnormalizeed_results$actual, unnormalizeed_results$predicted)
  pred_mae <- mae(unnormalizeed_results$actual, unnormalizeed_results$predicted)
  pred_mape <- mape(unnormalizeed_results$actual, unnormalizeed_results$predicted)
  pred_smape <- smape(unnormalizeed_results$actual, unnormalizeed_results$predicted)
  
  cat("Acuraccy -> ",round(accuracy*100, 2), "%", "\n")
  cat("RMSE -> ", pred_RMSE, "\n")
  cat("MAE -> ", pred_mae, "\n")
  cat("MAPE -> ", pred_mape, "\n")
  cat("sMAPE -> ", pred_smape, "\n")
}

#----view NN---#
train_1n <- trainModel(count=1, formula=(traindata_output ~ V1) , hiddenVal=c(5) )
testModel(1, train_1n)

train_2n <- trainModel(count=2, formula=(traindata_output ~ V1 + V2) , hiddenVal=c(5) )
testModel(2, train_2n)

train_3n <- trainModel(count=3, formula=(traindata_output ~ V1 + V2 + V3) , hiddenVal=c(5) )
testModel(3, train_3n)

train_4n <- trainModel(count=4, formula=(traindata_output ~ V1 + V2 + V3 + V4) , hiddenVal=c(5) )
testModel(4, train_4n)