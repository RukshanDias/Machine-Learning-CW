#-------------------------
# Rukshan Dias - w1912792
#-------------------------

# import libraries
library(readxl)
library(dplyr) 
library(neuralnet)
library(Metrics)

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

# create data frame
uow_dataFrame <- data.frame(date,data$'six',data$'seven',data$'eight')

# create dataframe for 8
eight_column <- c(uow_dataFrame$data.eight)
plot(eight_column, type = "l")

# create I/O matrix
time_delayed_matrix <- bind_cols(t7 = lag(eight_column,8),
                                 t4 = lag(eight_column,5),
                                 t3 = lag(eight_column,4),
                                 t2 = lag(eight_column,3),
                                 t1 = lag(eight_column,2),
                                 eightHour = eight_column)  

# combine six and seven columns
time_delayed_matrix <- cbind(uow_dataFrame[,2:3], time_delayed_matrix)

# remove NA values
time_delayed_matrix <- na.omit(time_delayed_matrix)

# splitting data to train & test
train_data <- time_delayed_matrix[1:380,]
test_data <- time_delayed_matrix[381:nrow(time_delayed_matrix),] #used nrow() since NA data have removed

#-- max min values
origin_min_val <- min(train_data)
origin_max_val <- max(train_data)

original_data_output <- test_data$eightHour


#-------normalize-------#
# min-max normalizing function
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}
# un-normalizing function
unnormalize <- function(x, min, max) {
  return( (max - min)*x + min )
}

# apply Normalization
time_delayedNorm <- as.data.frame(lapply(time_delayed_matrix[1:ncol(time_delayed_matrix)], normalize))

#splitting data after normalize test data
train_dataNorm <- time_delayedNorm[1:380,]
test_dataNorm <- time_delayedNorm[381:nrow(time_delayed_matrix),] #from line 381 to end

#view before & after normalization
boxplot(time_delayed_matrix, main="before normalizing data") #plot before normalizing data
boxplot(time_delayedNorm, main="After normalizing data") #plot after normalization

# Creating testing data for every timeDelay
t1_testData <- test_dataNorm[, c("data.six","data.seven", "t1")]
t2_testData <- test_dataNorm[, c("data.six","data.seven", "t1", "t2")]
t3_testData <- test_dataNorm[, c("data.six","data.seven", "t1", "t2", "t3")]
t4_testData <- test_dataNorm[, c("data.six","data.seven", "t1", "t2", "t3", "t4")]
t7_testData <- test_dataNorm[, c("data.six","data.seven", "t1", "t2", "t3", "t4", "t7")]

# function to train model
trainModel <- function(formula, hiddenVal, isLinear=TRUE, actFunc="logistic"){
  set.seed(1234)
  nn <- neuralnet(formula, 
                  data = train_dataNorm, hidden=hiddenVal, act.fct = actFunc, linear.output=isLinear)
  plot(nn)
  return(nn)
}

# function to test model
testModel <- function(nnModel, testing_df){
  nnresults <- compute(nnModel, testing_df)
  predicted <- nnresults$net.result
  unnormalised_predicted <- unnormalize(predicted, origin_min_val, origin_max_val)
  devia = ((original_data_output - unnormalised_predicted)/original_data_output) # calculate deviation
  modelAccuracy = 1 - abs(mean(devia))
  accuracy = round(modelAccuracy * 100 , digits = 2)
  
  rmse = rmse(original_data_output, unnormalised_predicted)
  mae = mae(original_data_output, unnormalised_predicted)
  mape = mape(original_data_output, unnormalised_predicted)
  smape = smape(original_data_output, unnormalised_predicted)
  
  cat("Model Accuracy:", accuracy, "%\n")
  cat("RMSE:", rmse, "\n")
  cat("MAE:", mae, "\n")
  cat("MAPE:", mape, "\n")
  cat("sMAPE:", smape, "\n")
  
  return(unnormalised_predicted)
}

# function to view weighted parameter count
view_weighted_para_count <- function(inputCount, hiddenVals){
  weight_para_count = (inputCount + 1) * hiddenVals[1]
  for (i in 1:length(hiddenVals)) {
    if(i==length(hiddenVals)) {
      weight_para_count = weight_para_count + (hiddenVals[i] + 1) * 1
    } else{
      weight_para_count = weight_para_count + ((hiddenVals[i] + 1) * hiddenVals[i+1])
    }
  }
  cat("Weighted parameter count for ",inputCount,"with",length(hiddenVals),"hidden layers -> ",weight_para_count,"\n")
  return(weight_para_count)
}


# t1
train_t1 <- trainModel(eightHour ~ data.six + data.seven + t1, c(5))
test_t1_predict <- testModel(train_t1, t1_testData)
view_weighted_para_count(inputCount=3 ,hiddenVals=c(5))

#t2
train_t2 <- trainModel(eightHour ~ data.six + data.seven + t1 + t2, c(8))
test_t2_predict <- testModel(train_t2, t2_testData)
view_weighted_para_count(inputCount=4 ,hiddenVals=c(8))

#t3
train_t3 <- trainModel(eightHour ~ data.six + data.seven + t1 + t2 + t3, c(10))
test_t3_predict <- testModel(train_t3, t3_testData)
view_weighted_para_count(inputCount=5 ,hiddenVals=c(10))

#t4
train_t4 <- trainModel(eightHour ~ data.six + data.seven + t1 + t2 + t3 + t4, c(8,5))
test_t4_predict <- testModel(train_t4, t4_testData)
view_weighted_para_count(inputCount=6 ,hiddenVals=c(8,5))

#t7
train_t7 <- trainModel(eightHour ~ data.six + data.seven + t1 + t2 + t3 + t4 + t7, c(10,5))
test_t7_predict <- testModel(train_t7, t7_testData)
view_weighted_para_count(inputCount=7 ,hiddenVals=c(10,5))

train_t7_01 <- trainModel(eightHour ~ data.six + data.seven + t7, c(5,3))
test_t7_predict_01 <- testModel(train_t7_01, t7_testData)
view_weighted_para_count(inputCount=7 ,hiddenVals=c(5,3))

#-----draw graph linear-----
set.seed(234)
actual <- original_data_output
predicted <- test_t7_predict + actual

actualPredictedDf <- data.frame(actual, predicted)

# fit data to a linear model
linear_model <- lm(predicted ~ actual, actualPredictedDf )

# plot predicted vs actual 
plot(predict(linear_model), actualPredictedDf$predicted,
     main = "Predicted vs Desired outputs (NARX)",
     xlab = "Predicted Values",
     ylab = "Desired Values")
abline(a = 0, b = 1, lwd=2,
       col = "red")

#-----draw graph non-linear-----
x = 1:length(actual)
plot(x, actual, col = "red", type = "l", lwd=1,
     main = "Energy consumption prediction")
  lines(x, test_t1_predict, col = "blue", lwd=1)
  legend("topright", legend = c("Desired outputs", "predicted outputs"), 
       fill = c("red", "blue"), col = 2:3, adj = c(0, 0.6))
grid()