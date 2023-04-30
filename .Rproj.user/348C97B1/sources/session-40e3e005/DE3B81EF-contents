#Install libraries
library(xlsx)
library(neuralnet)
library(Metrics)
library(stats)
library(dplyr)

#Read dataset
loadfile <- read.xlsx(file = 'uow_consumption.xlsx', sheetIndex = 1, header = TRUE)[4:7]
colnames(loadfile)<-c("Date","18H","19H","20H")

#Get the 20h column
dataset <- as.data.frame(loadfile$`20H`)
str(dataset)

#Creating lags
t1 <- lag(dataset,1)
t2 <- lag(dataset,2)
t3 <- lag(dataset,3)
t4 <- lag(dataset,4)
t7 <- lag(dataset,7)

#I/O matrix
v1 <- cbind(dataset,t1,t2)
v2 <- cbind(dataset,t1,t2,t3)
v3 <- cbind(dataset,t1,t2,t3,t4)
v4 <- cbind(dataset,t1,t2,t3,t4,t7)

#Remove NA 
v1 <- v1[complete.cases(v1),]
v2 <- v2[complete.cases(v2),]
v3 <- v3[complete.cases(v3),]
v4 <- v4[complete.cases(v4),]

colnames(v1) <- c("consumption","t1","t2")
colnames(v2) <- c("consumption","t1","t2","t3")
colnames(v3) <- c("consumption","t1","t2","t3","t4")
colnames(v4) <- c("consumption","t1","t2","t3","t4","t7")

#normalize function
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#normalize matrices
v1_norm <- normalize(v1)
v2_norm <- normalize(v2)
v3_norm <- normalize(v3)
v4_norm <- normalize(v4)

#split train data and test data
v1_train <- v1_norm[1:380,]
v2_train <- v2_norm[1:380,]
v3_train <- v3_norm[1:380,]
v4_train <- v4_norm[1:380,]

v1_test <- v1_norm[381:468,]
v2_test <- v2_norm[381:467,]
v3_test <- v3_norm[381:466,]
v4_test <- v4_norm[381:463,]

#de-normalize function
unnomarlize <- function(x,min,max){
  return((max-min)*x+min)
}

#Actual result and predicted result table function
actual_predict_table <- function(actual,predicted){
  act_pred_table <- cbind(actual,predicted)
  colnames(act_pred_table) <- c("Expected","Predicted")
  return(act_pred_table)
}

#Evaluation function
evaluateResult <- function(actual,pred){
  rmse <- rmse(actual = actual,predicted = pred)
  mae <- mae(actual = actual, predicted = pred)
  mape <- mape(actual = actual, predicted = pred)
  smape <- smape(actual = actual, predicted = pred)
  
 
  return( c(rmse,mae,mape,smape))
}

#Train Neural Network
v1_original_cons_train <- v1[1:380,"consumption"]
v1_original_cons_test <- v1[381:468,"consumption"]

v2_original_cons_train <- v2[1:380,"consumption"]
v2_original_cons_test <- v2[381:467,"consumption"]

v3_original_cons_train <- v3[1:380,"consumption"]
v3_original_cons_test <- v3[381:466,"consumption"]

v4_original_cons_train <- v4[1:380,"consumption"]
v4_original_cons_test <- v4[381:463,"consumption"]

consumption_max <- max(v1_original_cons_train)
consumption_min <- min(v1_original_cons_train)

#(v1, hidden layer =1 , nodes = 10, linear = true)
model1 <- neuralnet(consumption ~ t1+t2, hidden = 10, data = v1_train, linear.output = TRUE)

#display network diagram
plot(model1)

#Evaluate Model
model1_result <- neuralnet::compute(model1,v1_test[2:3])

predicted_result1 <- model1_result$net.result

predicted_result1 <- unnomarlize(predicted_result1,consumption_min,consumption_max)

perform_indices1 <- evaluateResult(v1_original_cons_test,predicted_result1)
perform_indices1

actualVsPred1 <- actual_predict_table(v1_original_cons_test,predicted_result1)
actualVsPred1


#========Model 2===================== 
#(v1, hidden layer =2 , nodes = (8,10), linear = true)
model2 <- neuralnet(consumption ~ t1+t2, hidden = c(6,8), data = v1_train, linear.output = TRUE)

#display network diagram
plot(model2)

#Evaluate Model
model2_result <- neuralnet::compute(model2,v1_test[2:3])

predicted_result2 <- model2_result$net.result

predicted_result2 <- unnomarlize(predicted_result2,consumption_min,consumption_max)

perform_indices2 <- evaluateResult(v1_original_cons_test,predicted_result2)
perform_indices2

actualVsPred2 <- actual_predict_table(v1_original_cons_test,predicted_result2)
actualVsPred2

#========Model 3===================== 
#(v1, hidden layer =1 , nodes = 10, linear = fales)
model3 <- neuralnet(consumption ~ t1+t2, hidden = 10, data = v1_train, linear.output = FALSE, act.fct = 'tanh')

#display network diagram
plot(model3)

#Evaluate Model
model3_result <- neuralnet::compute(model3,v1_test[2:3])

predicted_result3 <- model3_result$net.result

predicted_result3 <- unnomarlize(predicted_result3,consumption_min,consumption_max)

perform_indices3 <- evaluateResult(v1_original_cons_test,predicted_result3)
perform_indices3

actualVsPred3 <- actual_predict_table(v1_original_cons_test,predicted_result3)
actualVsPred3

#========Model 4===================== 
#(v1, hidden layer =2 , nodes = (8,10), linear = false, act.fact = tanh)
model4 <- neuralnet(consumption ~ t1+t2, hidden = c(8,10), data = v1_train, linear.output = FALSE, act.fct = 'tanh')

#display network diagram
plot(model4)

#Evaluate Model
model4_result <- neuralnet::compute(model4,v1_test[2:3])

predicted_result4 <- model4_result$net.result

predicted_result4 <- unnomarlize(predicted_result4,consumption_min,consumption_max)

perform_indices4 <- evaluateResult(v1_original_cons_test,predicted_result4)
perform_indices4

actualVsPred4 <- actual_predict_table(v1_original_cons_test,predicted_result4)
actualVsPred4

#========Model 5===================== 
#(v2, hidden layer =1 , nodes = 10, linear = true)
model5 <- neuralnet(consumption ~ t1+t2+t3, hidden = 10, data = v2_train, linear.output = TRUE)

#display network diagram
plot(model5)

#Evaluate Model
model5_result <- neuralnet::compute(model5,v2_test[2:4])

predicted_result5 <- model5_result$net.result



predicted_result5 <- unnomarlize(predicted_result5,consumption_min,consumption_max)

perform_indices5 <- evaluateResult(v2_original_cons_test,predicted_result5)
perform_indices5

actualVsPred5 <- actual_predict_table(v2_original_cons_test,predicted_result5)
actualVsPred5

#========Model 6===================== 
#(v2, hidden layer =1 , nodes = 10, linear = false, act.fact = tanh)
model6 <- neuralnet(consumption ~ t1+t2+t3, hidden = 10, data = v2_train, linear.output = FALSE, act.fct = 'tanh')

#display network diagram
plot(model6)

#Evaluate Model
model6_result <- neuralnet::compute(model6,v2_test[2:4])

predicted_result6 <- model6_result$net.result

predicted_result6 <- unnomarlize(predicted_result6,consumption_min,consumption_max)

perform_indices6 <- evaluateResult(v2_original_cons_test,predicted_result6)
perform_indices6

actualVsPred6 <- actual_predict_table(v2_original_cons_test,predicted_result6)
actualVsPred6

#========Model 7===================== 
#(v1, hidden layer =2 , nodes = (8,10), linear = true)
model7 <- neuralnet(consumption ~ t1+t2+t3, hidden = c(8,10), data = v2_train, linear.output = TRUE)

#display network diagram
plot(model7)

#Evaluate Model
model7_result <- neuralnet::compute(model7,v2_test[2:4])

predicted_result7 <- model7_result$net.result

predicted_result7 <- unnomarlize(predicted_result7,consumption_min,consumption_max)

perform_indices7 <- evaluateResult(v2_original_cons_test,predicted_result7)
perform_indices7

actualVsPred7 <- actual_predict_table(v2_original_cons_test,predicted_result7)
actualVsPred7

#========Model 8===================== error
#(v2, hidden layer =2 , nodes = (8,10), linear = false, act.fact = tanh)
model8 <- neuralnet(consumption ~ t1+t2+t3, hidden = c(8,10), data = v2_train, linear.output = FALSE, act.fct = 'tanh')

#display network diagram
plot(model8)

#Evaluate Model
model8_result <- neuralnet::compute(model8,v2_test[2:4])

predicted_result8 <- model8_result$net.result

predicted_result8 <- unnomarlize(predicted_result8,consumption_min,consumption_max)

perform_indices8 <- evaluateResult(v2_original_cons_test,predicted_result8)
perform_indices8

actualVsPred8 <- actual_predict_table(v2_original_cons_test,predicted_result8)
actualVsPred8


#========Model 9===================== 
#(v3, hidden layer =1 , nodes = 10, linear =true)
model9 <- neuralnet(consumption ~ t1+t2+t3+t4, hidden = 10, data = v3_train, linear.output =TRUE)

#display network diagram
plot(model9)

#Evaluate Model
model9_result <- neuralnet::compute(model9,v3_test[2:5])

predicted_result9 <- model9_result$net.result

predicted_result9 <- unnomarlize(predicted_result9,consumption_min,consumption_max)

perform_indices9 <- evaluateResult(v3_original_cons_test,predicted_result9)
perform_indices9

actualVsPred9 <- actual_predict_table(v3_original_cons_test,predicted_result9)
actualVsPred9

#========Model 10===================== 
#(v3, hidden layer =1 , nodes = 10, linear = false, act.fact = logistic)
model10 <- neuralnet(consumption ~ t1+t2+t3+t4, hidden = 10, data = v3_train, linear.output = FALSE, act.fct = 'logistic')

#display network diagram
plot(model10)

#Evaluate Model
model10_result <- neuralnet::compute(model10,v3_test[2:5])

predicted_result10 <- model10_result$net.result

predicted_result10 <- unnomarlize(predicted_result10,consumption_min,consumption_max)

perform_indices10 <- evaluateResult(v3_original_cons_test,predicted_result10)
perform_indices10

actualVsPred10 <- actual_predict_table(v3_original_cons_test,predicted_result10)
actualVsPred10


#========Model 11===================== 
#(v3, hidden layer =2 , nodes = (8,10), linear = true)
model11 <- neuralnet(consumption ~ t1+t2+t3+t4, hidden = c(8,10), data = v3_train, linear.output =TRUE)

#display network diagram
plot(model11)

#Evaluate Model
model11_result <- neuralnet::compute(model11,v3_test[2:5])

predicted_result11 <- model11_result$net.result

predicted_result11 <- unnomarlize(predicted_result11,consumption_min,consumption_max)

perform_indices11 <- evaluateResult(v3_original_cons_test,predicted_result11)
perform_indices11

actualVsPred11 <- actual_predict_table(v3_original_cons_test,predicted_result11)
actualVsPred11


#========Model 12===================== 
#(v3, hidden layer =2 , nodes = (8,10), linear = false, act.fact = logistic)
model12 <- neuralnet(consumption ~ t1+t2, hidden = c(8,10), data = v3_train, linear.output = FALSE, act.fct = 'logistic')

#display network diagram
plot(model12)

#Evaluate Model
model12_result <- neuralnet::compute(model12,v3_test[2:5])

predicted_result12 <- model12_result$net.result

predicted_result12 <- unnomarlize(predicted_result12,consumption_min,consumption_max)

perform_indices12 <- evaluateResult(v3_original_cons_test,predicted_result12)
perform_indices12

actualVsPred12 <- actual_predict_table(v3_original_cons_test,predicted_result12)
actualVsPred12


#========Model 13===================== 
#(v3, hidden layer =2 , nodes = (6,8), linear = false, act.fact = tanh)
model13 <- neuralnet(consumption ~ t1+t2+t3+t4, hidden = c(6,8), data = v3_train, linear.output = FALSE, act.fct = 'tanh')

#display network diagram
plot(model13)

#Evaluate Model
model13_result <- neuralnet::compute(model13,v3_test[2:5])

predicted_result13 <- model13_result$net.result

predicted_result13 <- unnomarlize(predicted_result13,consumption_min,consumption_max)

perform_indices13 <- evaluateResult(v3_original_cons_test,predicted_result13)
perform_indices13

actualVsPred13 <- actual_predict_table(v3_original_cons_test,predicted_result13)
actualVsPred13


#========Model 14===================== 
#(v4, hidden layer =1 , nodes = 10, linear = false, act.fact = logistic)
model14 <- neuralnet(consumption ~ t1+t2+t3+t4+t7, hidden = 10, data = v4_train, linear.output = FALSE, act.fct = 'logistic')

#display network diagram
plot(model14)

#Evaluate Model
model14_result <- neuralnet::compute(model14,v4_test[2:6])

predicted_result14 <- model14_result$net.result

predicted_result14 <- unnomarlize(predicted_result14,consumption_min,consumption_max)

perform_indices14 <- evaluateResult(v4_original_cons_test,predicted_result14)
perform_indices14

actualVsPred14 <- actual_predict_table(v4_original_cons_test,predicted_result14)
actualVsPred14


#========Model 15===================== 
#(v4, hidden layer =2 , nodes = (6,8), linear = false, act.fact = tanh)
model15 <- neuralnet(consumption ~ t1+t2+t3+t4+t7, hidden = c(6,8), data = v4_train, linear.output = FALSE, act.fct = 'tanh')

#display network diagram
plot(model15)

#Evaluate Model
model15_result <- neuralnet::compute(model15,v4_test[2:6])

predicted_result15 <- model15_result$net.result

predicted_result15 <- unnomarlize(predicted_result15,consumption_min,consumption_max)

perform_indices15 <- evaluateResult(v4_original_cons_test,predicted_result15)
perform_indices15

actualVsPred15 <- actual_predict_table(v4_original_cons_test,predicted_result15)
actualVsPred15
