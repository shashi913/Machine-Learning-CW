library(readxl) #for read xlsx file
library(neuralnet)
library(Metrics)  # to use RMSE function

# Load the dataset
exchange_data <- read_excel("Financial Forecasting/ExchangeUSD.xlsx")

colnames(exchange_data) <- c("YYYY/MM/DD", "Wdy", "USD_EUR")

# Extract exchange rates column
exchange_rates <- as.data.frame(exchange_data$"USD_EUR")


#Checking missing values
sum(is.na(exchange_rates))

# Convert data frame to time series object
#ts_data <- ts(exchange_rates)

# Remove missing values
exchange_rates <- na.omit(exchange_rates)

# Calculate lagged values
t_1 <- lag(exchange_rates, 1)
t_2 <- lag(exchange_rates, 2)
t_3 <- lag(exchange_rates, 3)
t_4 <- lag(exchange_rates, 4)

# Creating I/O Matrixes
M1 <- cbind(t_1,exchange_rates)
M2 <- cbind(t_1,t_2,exchange_rates)
M3 <- cbind(t_1,t_2,t_3,exchange_rates)
M4 <- cbind(t_1,t_2,t_3,t_4,exchange_rates)

# Rename column names in I/O Matrixes
colnames(M1)<-c("t-1","output")
colnames(M2)<-c("t-1","t-2","output")
colnames(M3)<-c("t-1","t-2","t-3","output")
colnames(M4)<-c("t-1","t-2","t-3","t-4","output")

# Remove missing values
M1 <- M1[complete.cases(M1),]
M2 <- M2[complete.cases(M2),]
M3 <- M3[complete.cases(M3),]
M4 <- M4[complete.cases(M4),]


# min max normalization 
min_max_normalization <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}



# Apply normalization
M1_norm <- as.data.frame(lapply(M1, min_max_normalization))
M2_norm <- as.data.frame(lapply(M2, min_max_normalization))
M3_norm <- as.data.frame(lapply(M3, min_max_normalization))
M4_norm <- as.data.frame(lapply(M4, min_max_normalization))

# Checking the range of normalized data
summary(M1_norm)
summary(M2_norm)
summary(M3_norm)
summary(M4_norm)


#Creating training and testing data
M1_train_norm <- M1_norm[1:400, ]
M2_train_norm <- M2_norm[1:400, ]
M3_train_norm <- M3_norm[1:400, ]
M4_train_norm <- M4_norm[1:400, ]

M1_test_norm <- M1_norm[401:499, ]
M2_test_norm <- M2_norm[401:498, ]
M3_test_norm <- M3_norm[401:497, ]
M4_test_norm <- M4_norm[401:496, ]


# Evaluation function
evaluate <- function(actual,prediction){
  rmse <- rmse(actual = actual,predicted = prediction)
  mae <- mae(actual = actual, predicted = prediction)
  mape <- mape(actual = actual, predicted = prediction)
  smape <- smape(actual = actual, predicted = prediction)
  
  df <- data.frame(stat.indices = c("RMSE","MAE","MAPE","sMAPE"),values = c(rmse,mae,mape,smape))
  return(df)
  
}

# De-Normalization
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}


set.seed(20)


#M1
original_train_outputs_M1 <- M1[1:400,"output"]
original_test_outputs_M1 <- M1[401:499,"output"]

min_output_M1 <- min(original_train_outputs_M1)
max_output_M1 <- max(original_train_outputs_M1)

# neural network code/training
model1 <- neuralnet(output ~ t.1, data = M1_train_norm, hidden = 8, act.fct = 'logistic', linear.output = T)
plot(model1)
model1_results <- neuralnet::compute(model1,M1_test_norm[1])
predicted_output_model1 <- unnormalize(model1_results$net.result,min_output_M1,max_output_M1) #obtain predicted output
evaluate(original_test_outputs_M1,predicted_output_model1)  # evaluation

# neural network code/training
model2 <- neuralnet(output ~ t.1, data = M1_train_norm, hidden = c(8,5), act.fct = 'tanh', linear.output = T)
plot(model2)
model2_results <- neuralnet::compute(model2,M1_test_norm[1])
predicted_output_model2 <- unnormalize(model2_results$net.result,min_output_M1,max_output_M1) #obtain predicted output
evaluate(original_test_outputs_M1,predicted_output_model2)  # evaluation

# neural network code/training
model3 <- neuralnet(output ~ t.1, data = M1_train_norm, hidden = c(10,6), act.fct = 'logistic', linear.output = F)
plot(model3)
model3_results <- neuralnet::compute(model3,M1_test_norm[1])
predicted_output_model3 <- unnormalize(model3_results$net.result,min_output_M1,max_output_M1) #obtain predicted output
evaluate(original_test_outputs_M1,predicted_output_model3)  # evaluation




#M2
original_train_outputs_M2 <- M2[1:400,"output"]
original_test_outputs_M2 <- M2[401:498,"output"]

min_output_M2 <- min(original_train_outputs_M2)
max_output_M2 <- max(original_train_outputs_M2)

# neural network code/training
model4 <- neuralnet(output ~ t.1 + t.2, data = M2_train_norm, hidden = 12, act.fct = 'logistic', linear.output = T)
plot(model4)
model4_results <- neuralnet::compute(model4,M2_test_norm[1:2])
predicted_output_model4 <- unnormalize(model4_results$net.result,min_output_M2,max_output_M2) #obtain predicted output
evaluate(original_test_outputs_M2,predicted_output_model4)  # evaluation

# neural network code/training
model5 <- neuralnet(output ~ t.1 + t.2, data = M2_train_norm, hidden = c(7,5), act.fct = 'tanh', linear.output = T)
plot(model5)
model5_results <- neuralnet::compute(model5,M2_test_norm[1:2])
predicted_output_model5 <- unnormalize(model5_results$net.result,min_output_M2,max_output_M2) #obtain predicted output
evaluate(original_test_outputs_M2,predicted_output_model5)  # evaluation


# neural network code/training
model6 <- neuralnet(output ~ t.1 + t.2, data = M2_train_norm, hidden = c(12,7), act.fct = 'logistic', linear.output = F)
plot(model6)
model6_results <- neuralnet::compute(model6,M2_test_norm[1:2])
predicted_output_model6 <- unnormalize(model6_results$net.result,min_output_M2,max_output_M2) #obtain predicted output
evaluate(original_test_outputs_M2,predicted_output_model6)  # evaluation



#M3
original_train_outputs_M3 <- M3[1:400,"output"]
original_test_outputs_M3 <- M3[401:497,"output"]

min_output_M3 <- min(original_train_outputs_M3)
max_output_M3 <- max(original_train_outputs_M3)

# neural network code/training
model7 <- neuralnet(output ~ t.1 + t.2 + t.3, data = M3_train_norm, hidden = 10, act.fct = 'logistic', linear.output = T)
plot(model7)
model7_results <- neuralnet::compute(model7,M3_test_norm[1:3])
predicted_output_model7 <- unnormalize(model7_results$net.result,min_output_M3,max_output_M3) #obtain predicted output
evaluate(original_test_outputs_M3,predicted_output_model7)  # evaluation

# neural network code/training
model8 <- neuralnet(output ~ t.1 + t.2 + t.3, data = M3_train_norm, hidden = c(10,5), act.fct = 'tanh', linear.output = T)
plot(model8)
model8_results <- neuralnet::compute(model8,M3_test_norm[1:3])
predicted_output_model8 <- unnormalize(model8_results$net.result,min_output_M3,max_output_M3) #obtain predicted output
evaluate(original_test_outputs_M3,predicted_output_model8)  # evaluation

# neural network code/training
model9 <- neuralnet(output ~ t.1 + t.2 + t.3, data = M3_train_norm, hidden = c(10,8), act.fct = 'logistic', linear.output = F)
plot(model9)
model9_results <- neuralnet::compute(model9,M3_test_norm[1:3])
predicted_output_model9 <- unnormalize(model9_results$net.result,min_output_M3,max_output_M3) #obtain predicted output
evaluate(original_test_outputs_M3,predicted_output_model9)  # evaluation




#M4
original_train_outputs_M4 <- M4[1:400,"output"]
original_test_outputs_M4 <- M4[401:496,"output"]

min_output_M4 <- min(original_train_outputs_M4)
max_output_M4 <- max(original_train_outputs_M4)

# neural network code/training
model10 <- neuralnet(output ~ t.1 + t.2 + t.3 + t.4, data = M4_train_norm, hidden = 12, act.fct = 'logistic', linear.output = T)
plot(model10)
model10_results <- neuralnet::compute(model10,M4_test_norm[1:4])
predicted_output_model10 <- unnormalize(model10_results$net.result,min_output_M4,max_output_M4) #obtain predicted output
evaluate(original_test_outputs_M4,predicted_output_model10)  # evaluation

# neural network code/training
model11 <- neuralnet(output ~ t.1 + t.2 + t.3 + t.4, data = M4_train_norm, hidden = c(10,9), act.fct = 'tanh', linear.output = T)
plot(model11)
model11_results <- neuralnet::compute(model11,M4_test_norm[1:4])
predicted_output_model11 <- unnormalize(model11_results$net.result,min_output_M4,max_output_M4) #obtain predicted output
evaluate(original_test_outputs_M4,predicted_output_model11)  # evaluation

# neural network code/training
model12 <- neuralnet(output ~ t.1 + t.2 + t.3 + t.4, data = M4_train_norm, hidden = c(12,8), act.fct = 'logistic', linear.output = F)
plot(model12)
model12_results <- neuralnet::compute(model12,M4_test_norm[1:4])
predicted_output_model12 <- unnormalize(model12_results$net.result,min_output_M4,max_output_M4) #obtain predicted output
evaluate(original_test_outputs_M4,predicted_output_model12)  # evaluation

# Evaluate model 1
evaluate(original_test_outputs_M1, predicted_output_model1)

# Evaluate model 2
evaluate(original_test_outputs_M1, predicted_output_model2)

# Evaluate model 3
evaluate(original_test_outputs_M1, predicted_output_model3)

# Evaluate model 4
evaluate(original_test_outputs_M2, predicted_output_model4)

# Evaluate model 5
evaluate(original_test_outputs_M2, predicted_output_model5)

# Evaluate model 6
evaluate(original_test_outputs_M2, predicted_output_model6)

# Evaluate model 7
evaluate(original_test_outputs_M3, predicted_output_model7)

# Evaluate model 8
evaluate(original_test_outputs_M3, predicted_output_model8)

# Evaluate model 9
evaluate(original_test_outputs_M3, predicted_output_model9)

# Evaluate model 10
evaluate(original_test_outputs_M4, predicted_output_model10)

# Evaluate model 11
evaluate(original_test_outputs_M4, predicted_output_model11)

# Evaluate model 12
evaluate(original_test_outputs_M4, predicted_output_model12)


#graphical representation of models
par(mfrow=c(1,1))
plot(original_test_outputs_M4, predicted_output_model12 ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(a=0, b=1, h=90, v=90)
