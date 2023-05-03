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
  rmse <- round(rmse(actual = actual,predicted = pred),3)
  mae <- round(mae(actual = actual, predicted = pred),3)
  mape <- round(mape(actual = actual, predicted = pred),3)
  smape <- round(smape(actual = actual, predicted = pred),3)
  
 
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


#===================Model 1========================
#(v1, hidden layer =1 , nodes = 6, linear = true)
inp_1 <- c("t1+t2","1","6","true","none")
model1 <- neuralnet(consumption ~ t1+t2, hidden = 6, data = v1_train, linear.output = TRUE)

#display network diagram
plot(model1)

#Evaluate Model
model1_result <- neuralnet::compute(model1,v1_test[2:3])

predicted_result1 <- model1_result$net.result

predicted_result1 <- unnomarlize(predicted_result1,min(v1_original_cons_train),max(v1_original_cons_train))

perform_indices1 <- evaluateResult(v1_original_cons_test,predicted_result1)
perform_indices1

actualVsPred1 <- actual_predict_table(v1_original_cons_test,predicted_result1)
actualVsPred1


#========Model 2===================== 
#(v1, hidden layer =2 , nodes = (6,8), linear = true)
inp_2 <- c("t1+t2","2","(6,8)","true","none")
model2 <- neuralnet(consumption ~ t1+t2, hidden = c(6,8), data = v1_train, linear.output = TRUE)

#display network diagram
plot(model2)

#Evaluate Model
model2_result <- neuralnet::compute(model2,v1_test[2:3])

predicted_result2 <- model2_result$net.result

predicted_result2 <- unnomarlize(predicted_result2,min(v1_original_cons_train),max(v1_original_cons_train))

perform_indices2 <- evaluateResult(v1_original_cons_test,predicted_result2)
perform_indices2

actualVsPred2 <- actual_predict_table(v1_original_cons_test,predicted_result2)
actualVsPred2

#========Model 3===================== 
#(v1, hidden layer =1 , nodes = 6, linear = fales,act.fct = tanh)
inp_3 <- c("t1+t2","1","6","false","tanh")
model3 <- neuralnet(consumption ~ t1+t2, hidden = 6, data = v1_train, linear.output = FALSE, act.fct = 'tanh')

#display network diagram
plot(model3)

#Evaluate Model
model3_result <- neuralnet::compute(model3,v1_test[2:3])

predicted_result3 <- model3_result$net.result

predicted_result3 <- unnomarlize(predicted_result3,min(v1_original_cons_train),max(v1_original_cons_train))

perform_indices3 <- evaluateResult(v1_original_cons_test,predicted_result3)
perform_indices3

actualVsPred3 <- actual_predict_table(v1_original_cons_test,predicted_result3)
actualVsPred3

#========Model 4===================== 
#(v1, hidden layer =2 , nodes = (6,8), linear = false, act.fact = tanh)
inp_4 <- c("t1+t2","2","(6,8)","false","tanh")
model4 <- neuralnet(consumption ~ t1+t2, hidden = c(6,8), data = v1_train, linear.output = FALSE, act.fct = 'tanh')

#display network diagram
plot(model4)

#Evaluate Model
model4_result <- neuralnet::compute(model4,v1_test[2:3])

predicted_result4 <- model4_result$net.result

predicted_result4 <- unnomarlize(predicted_result4,min(v1_original_cons_train),max(v1_original_cons_train))

perform_indices4 <- evaluateResult(v1_original_cons_test,predicted_result4)
perform_indices4

actualVsPred4 <- actual_predict_table(v1_original_cons_test,predicted_result4)
actualVsPred4

#========Model 5===================== 
#(v2, hidden layer =1 , nodes = 6, linear = true)
inp_5 <- c("t1+t2+t3","1","6","true","none")
model5 <- neuralnet(consumption ~ t1+t2+t3, hidden = 6, data = v2_train, linear.output = TRUE)

#display network diagram
plot(model5)

#Evaluate Model
model5_result <- neuralnet::compute(model5,v2_test[2:4])

predicted_result5 <- model5_result$net.result

predicted_result5 <- unnomarlize(predicted_result5,min(v2_original_cons_train),max(v2_original_cons_train))

perform_indices5 <- evaluateResult(v2_original_cons_test,predicted_result5)
perform_indices5

actualVsPred5 <- actual_predict_table(v2_original_cons_test,predicted_result5)
actualVsPred5

#========Model 6===================== 
#(v2, hidden layer =1 , nodes = 6, linear = false, act.fact = tanh)
inp_6 <- c("t1+t2+t3","1","6","false","tanh")
model6 <- neuralnet(consumption ~ t1+t2+t3, hidden = 6, data = v2_train, linear.output = FALSE, act.fct = 'tanh')

#display network diagram
plot(model6)

#Evaluate Model
model6_result <- neuralnet::compute(model6,v2_test[2:4])

predicted_result6 <- model6_result$net.result

predicted_result6 <- unnomarlize(predicted_result6,min(v2_original_cons_train),max(v2_original_cons_train))

perform_indices6 <- evaluateResult(v2_original_cons_test,predicted_result6)
perform_indices6

actualVsPred6 <- actual_predict_table(v2_original_cons_test,predicted_result6)
actualVsPred6

#========Model 7===================== 
#(v1, hidden layer =2 , nodes = (6,8), linear = true)
inp_7 <- c("t1+t2+t3","2","(6,8)","true","none")
model7 <- neuralnet(consumption ~ t1+t2+t3, hidden = c(6,8), data = v2_train, linear.output = TRUE)

#display network diagram
plot(model7)

#Evaluate Model
model7_result <- neuralnet::compute(model7,v2_test[2:4])

predicted_result7 <- model7_result$net.result

predicted_result7 <- unnomarlize(predicted_result7,min(v2_original_cons_train),max(v2_original_cons_train))

perform_indices7 <- evaluateResult(v2_original_cons_test,predicted_result7)
perform_indices7

actualVsPred7 <- actual_predict_table(v2_original_cons_test,predicted_result7)
actualVsPred7

#========Model 8=====================
#(v2, hidden layer =2 , nodes = (6,8), linear = false, act.fact = tanh)
inp_8 <- c("t1+t2+t3","2","(6,8)","false","tanh")
model8 <- neuralnet(consumption ~ t1+t2+t3, hidden = c(6,8), data = v2_train, linear.output = FALSE, act.fct = 'tanh')

#display network diagram
plot(model8)

#Evaluate Model
model8_result <- neuralnet::compute(model8,v2_test[2:4])

predicted_result8 <- model8_result$net.result

predicted_result8 <- unnomarlize(predicted_result8,min(v2_original_cons_train),max(v2_original_cons_train))

perform_indices8 <- evaluateResult(v2_original_cons_test,predicted_result8)
perform_indices8

actualVsPred8 <- actual_predict_table(v2_original_cons_test,predicted_result8)
actualVsPred8


#========Model 9===================== 
#(v3, hidden layer =1 , nodes = 6, linear =true)
inp_9 <- c("t1+t2+t3+4","1","6","true","none")
model9 <- neuralnet(consumption ~ t1+t2+t3+t4, hidden = 6, data = v3_train, linear.output =TRUE)

#display network diagram
plot(model9)

#Evaluate Model
model9_result <- neuralnet::compute(model9,v3_test[2:5])

predicted_result9 <- model9_result$net.result

predicted_result9 <- unnomarlize(predicted_result9,min(v3_original_cons_train),max(v3_original_cons_train))

perform_indices9 <- evaluateResult(v3_original_cons_test,predicted_result9)
perform_indices9

actualVsPred9 <- actual_predict_table(v3_original_cons_test,predicted_result9)
actualVsPred9

#========Model 10===================== 
#(v3, hidden layer =1 , nodes = 6, linear = false, act.fact = logistic)
inp_10 <- c("t1+t2+t3+4","1","6","false","logistic")
model10 <- neuralnet(consumption ~ t1+t2+t3+t4, hidden = 6, data = v3_train, linear.output = FALSE, act.fct = 'logistic')

#display network diagram
plot(model10)

#Evaluate Model
model10_result <- neuralnet::compute(model10,v3_test[2:5])

predicted_result10 <- model10_result$net.result

predicted_result10 <- unnomarlize(predicted_result10,min(v3_original_cons_train),max(v3_original_cons_train))

perform_indices10 <- evaluateResult(v3_original_cons_test,predicted_result10)
perform_indices10

actualVsPred10 <- actual_predict_table(v3_original_cons_test,predicted_result10)
actualVsPred10


#========Model 11===================== 
#(v3, hidden layer =2 , nodes = (6,8), linear = true)
inp_11 <- c("t1+t2+t3+4","2","(6,8)","true","none")
model11 <- neuralnet(consumption ~ t1+t2+t3+t4, hidden = c(6,8), data = v3_train, linear.output =TRUE)

#display network diagram
plot(model11)

#Evaluate Model
model11_result <- neuralnet::compute(model11,v3_test[2:5])

predicted_result11 <- model11_result$net.result

predicted_result11 <- unnomarlize(predicted_result11,min(v3_original_cons_train),max(v3_original_cons_train))

perform_indices11 <- evaluateResult(v3_original_cons_test,predicted_result11)
perform_indices11

actualVsPred11 <- actual_predict_table(v3_original_cons_test,predicted_result11)
actualVsPred11


#========Model 12===================== 
#(v3, hidden layer =2 , nodes = (6,8), linear = false, act.fact = logistic)
inp_12 <- c("t1+t2+t3+4","2","(6,8)","false","logistic")
model12 <- neuralnet(consumption ~ t1+t2, hidden = c(6,8), data = v3_train, linear.output = FALSE, act.fct = 'logistic')

#display network diagram
plot(model12)

#Evaluate Model
model12_result <- neuralnet::compute(model12,v3_test[2:5])

predicted_result12 <- model12_result$net.result

predicted_result12 <- unnomarlize(predicted_result12,min(v3_original_cons_train),max(v3_original_cons_train))

perform_indices12 <- evaluateResult(v3_original_cons_test,predicted_result12)
perform_indices12

actualVsPred12 <- actual_predict_table(v3_original_cons_test,predicted_result12)
actualVsPred12


#========Model 13===================== 
#(v3, hidden layer =2 , nodes = (8,10), linear = true)
inp_13 <- c("t1+t2+t3+4","2","(8,10)","true","none")
model13 <- neuralnet(consumption ~ t1+t2+t3+t4, hidden = c(8,10), data = v3_train, linear.output = TRUE)

#display network diagram
plot(model13)

#Evaluate Model
model13_result <- neuralnet::compute(model13,v3_test[2:5])

predicted_result13 <- model13_result$net.result

predicted_result13 <- unnomarlize(predicted_result13,min(v3_original_cons_train),max(v3_original_cons_train))

perform_indices13 <- evaluateResult(v3_original_cons_test,predicted_result13)
perform_indices13

actualVsPred13 <- actual_predict_table(v3_original_cons_test,predicted_result13)
actualVsPred13


#========Model 14===================== 
#(v4, hidden layer =1 , nodes = 6, linear = false, act.fact = logistic)
inp_14 <- c("t1+t2+t3+t4+t7","1","6","false","logistic")
model14 <- neuralnet(consumption ~ t1+t2+t3+t4+t7, hidden = 6, data = v4_train, linear.output = FALSE, act.fct = 'logistic')

#display network diagram
plot(model14)

#Evaluate Model
model14_result <- neuralnet::compute(model14,v4_test[2:6])

predicted_result14 <- model14_result$net.result

predicted_result14 <- unnomarlize(predicted_result14,min(v4_original_cons_train),max(v4_original_cons_train))

perform_indices14 <- evaluateResult(v4_original_cons_test,predicted_result14)
perform_indices14

actualVsPred14 <- actual_predict_table(v4_original_cons_test,predicted_result14)
actualVsPred14


#========Model 15===================== 
#(v4, hidden layer =2 , nodes = (4,6), linear = false, act.fact = tanh)
inp_15 <- c("t1+t2+t3+t4+t7","2","(4,6)","false","tanh")
model15 <- neuralnet(consumption ~ t1+t2+t3+t4+t7, hidden = c(4,6), data = v4_train, linear.output = FALSE, act.fct = 'tanh')

#display network diagram
plot(model15)

#Evaluate Model
model15_result <- neuralnet::compute(model15,v4_test[2:6])

predicted_result15 <- model15_result$net.result

predicted_result15 <- unnomarlize(predicted_result15,min(v4_original_cons_train),max(v4_original_cons_train))

perform_indices15 <- evaluateResult(v4_original_cons_test,predicted_result15)
perform_indices15

actualVsPred15 <- actual_predict_table(v4_original_cons_test,predicted_result15)
actualVsPred15

#Comparison table
compTable <- rbind(
  perform_indices1,
  perform_indices2,
  perform_indices3,
  perform_indices4,
  perform_indices5,
  perform_indices6,
  perform_indices7,
  perform_indices8,
  perform_indices9,
  perform_indices10,
  perform_indices11,
  perform_indices12,
  perform_indices13,
  perform_indices14,
  perform_indices15
  )
compTable <- cbind(
  rbind(
    inp_1,
    inp_2,
    inp_3,
    inp_4,
    inp_5,
    inp_6,
    inp_7,
    inp_8,
    inp_9,
    inp_10,
    inp_11,
    inp_12,
    inp_13,
    inp_14,
    inp_15
  ),
  compTable
)
colnames(compTable) <- c("inputs","Hidden Layers","nodes","linear","act.function","RMSE","MAE","MAPE","sMAPE")
rownames(compTable) <- c(
  "Model 1",
  "Model 2",
  "Model 3",
  "Model 4",
  "Model 5",
  "Model 6",
  "Model 7",
  "Model 8",
  "Model 9",
  "Model 10",
  "Model 11",
  "Model 12",
  "Model 13",
  "Model 14",
  "Model 15"
)
compTable
View(compTable)

#NARX approach

#18th hour prediction
h18_io <-bind_cols(h18_lag1 = lag(loadfile$`18H`,1),
                   h18_lag2 = lag(loadfile$`18H`,2),
                   h18_lag3 = lag(loadfile$`18H`,3),
                   h18_lag4 = lag(loadfile$`18H`,4),
                   h18_lag7 = lag(loadfile$`18H`,7),
                   h18 = loadfile$`18H`)   

h18_io <- h18_io[complete.cases(h18_io),]
h18_io_norm <- normalize(h18_io)

h18_train <- h18_io_norm[1:380,]
h18_test <- h18_io_norm[381:463,]

h18_nn <- neuralnet(h18 ~ h18_lag1 +  h18_lag2 +  h18_lag4 +  h18_lag4 + h18_lag7,
                    data = h18_train, hidden = 6)

#19th hour prediction
h19_io <-bind_cols(h19_lag1 = lag(loadfile$`19H`,1),
                   h19_lag2 = lag(loadfile$`19H`,2),
                   h19_lag3 = lag(loadfile$`19H`,3),
                   h19_lag4 = lag(loadfile$`19H`,4),
                   h19_lag7 = lag(loadfile$`19H`,7),
                   h19 = loadfile$`19H`)   

h19_io <- h19_io[complete.cases(h19_io),]
h19_io_norm <- normalize(h19_io)
h19_train <- h19_io_norm[1:380,]
h19_train$h18_pred <- neuralnet::compute(h18_nn,h18_train)$net.result

h19_test <- h19_io[381:463,]
h19_test$h18_pred <- neuralnet::compute(h18_nn,h18_test)$net.result

h19_nn <- neuralnet(h19 ~ h19_lag1 +  h19_lag2 +  h19_lag4 +  h19_lag4 + h19_lag7 + h18_pred,
                    data = h19_train, hidden = 6)

#20th hour prediction
v1_h20_io <-bind_cols(h20_lag1 = lag(loadfile$`20H`,1),
                   h20_lag2 = lag(loadfile$`20H`,2),
                   h20_lag3 = lag(loadfile$`20H`,3),
                   h20_lag4 = lag(loadfile$`20H`,4),
                   h20_lag7 = lag(loadfile$`20H`,7),
                   h20 = loadfile$`20H`)   

v2_h20_io <-bind_cols(h20_lag1 = lag(loadfile$`20H`,1),
                      h20_lag2 = lag(loadfile$`20H`,2),
                      h20_lag3 = lag(loadfile$`20H`,3),
                      h20_lag4 = lag(loadfile$`20H`,4),
                      h20 = loadfile$`20H`) 

v1_h20_io <- v1_h20_io[complete.cases(v1_h20_io),]
v2_h20_io <- v2_h20_io[complete.cases(v2_h20_io),]

v1_h20_io_norm <- normalize(v1_h20_io)
v2_h20_io_norm <- normalize(v1_h20_io)

v1_h20_train <- v1_h20_io_norm[1:380,]
v1_h20_train$h19_pred <- neuralnet::compute(h19_nn,h19_train)$net.result

v2_h20_train <- v2_h20_io_norm[1:380,]
v2_h20_train$h19_pred <- neuralnet::compute(h19_nn,h19_train)$net.result

v1_h20_test <- v1_h20_io[381:463,]
v1_h20_test$h19_pred <- neuralnet::compute(h19_nn,h19_test)$net.result

v2_h20_test <- v2_h20_io[384:466,]
v2_h20_test$h19_pred <- neuralnet::compute(h19_nn,h19_test)$net.result

#original values of consumption
v1_original_train <- v1_h20_io[1:380,"h20"]
v2_original_train <- v2_h20_io[1:380,"h20"]

v1_original_test <- v1_h20_io[381:463,"h20"]
v2_original_test <- v2_h20_io[381:466,"h20"]

h20_nn <- neuralnet(h20 ~ h20_lag1 +  h20_lag2 +  h20_lag4 +  h20_lag4 + h20_lag7 + h19_pred,
                    data = v1_h20_train, hidden = 6)

mod_res_1 <- neuralnet::compute(h20_nn,v1_h20_test)
pred_res_1 <- unnomarlize(mod_res_1$net.result,min(v1_original_train),max(v1_original_train))
perform_1 <- evaluateResult(v1_original_test,pred_res_1)
act_pred_com_1 <- actual_predict_table(v1_original_test,pred_res_1)



#IO matrix
io_1 <- cbind(dataset,t1,t2,t3,t4,loadfile$`18H`,loadfile$`19H`)
io_2 <- cbind(dataset,t1,t2,t3,t4,t7,loadfile$`18H`,loadfile$`19H`)

#Remove NA
io_1 <- io_1[complete.cases(io_1),]
io_2 <- io_2[complete.cases(io_2),]

colnames(io_1) <- c("consumption", "t1","t2","t3","t4","h18","h19")
colnames(io_2) <- c("consumption", "t1","t2","t3","t4","t7","h18","h19")

#normalize data
io_1_norm <- normalize(io_1)
io_2_norm <- normalize(io_2)

#split data 
dataset_train_1 <- io_1_norm[1:380,]
dataset_train_2 <- io_2_norm[1:380,]

dataset_test_1 <- io_1_norm[381:466,]
dataset_test_2 <- io_2_norm[381:463,]

#original values of consumption
io_1_original_train <- io_1[1:380,"consumption"]
io_2_original_train <- io_2[1:380,"consumption"]

io_1_original_test <- io_1[381:466,"consumption"]
io_2_original_test <- io_2[381:463,"consumption"]

#NARX Models

#===============Model 1=============================
nn_in1 <- c("t1,t2,t3,t4,h18,h19","1","8","logistic")
nn_model1 <- neuralnet(consumption ~ t1 +t2 +t3 +t4 +h18 +h19, data = dataset_train_1, hidden = 8, linear.output = FALSE, act.fct = 'logistic')
plot(nn_model1)

#===============Model 2=============================
nn_in2 <- c("t1,t2,t3,t4,h18,h19","2","8,10","logistic")
nn_model2 <- neuralnet(consumption ~ t1 +t2 +t3 +t4 +h18 +h19, data = dataset_train_1, hidden = c(8,10), linear.output = FALSE, act.fct = 'logistic')
plot(nn_model2)

#===============Model 3=============================
nn_in3 <- c("t1,t2,t3,t4,t7,h18,h19","1","8","logistic")
nn_model3 <- neuralnet(consumption ~ t1 +t2 +t3 +t4 +t7 +h18 +h19, data = dataset_train_2, hidden = 8, linear.output = FALSE, act.fct = 'logistic')
plot(nn_model3)

#===============Model 4=============================
nn_in4 <- c("t1,t2,t3,t4,t7,h18,h19","2","8,10","logistic")
nn_model4 <- neuralnet(consumption ~ t1 +t2 +t3 +t4 +t7 +h18 +h19, data = dataset_train_2, hidden = c(8,10), linear.output = FALSE, act.fct = 'logistic')
plot(nn_model4)

#===============Model 3=============================
nn_in5 <- c("t1,t2,t3,t4,t7,h18,h19","2","10,12","tanh")
nn_model5 <- neuralnet(consumption ~ t1 +t2 +t3 +t4 +t7 +h18 +h19, data = dataset_train_2, hidden = c(10,12), linear.output = FALSE, act.fct = 'tanh')
plot(nn_model5)


#Evaluate Models
#===============model 1========================
mod_res_1 <- neuralnet::compute(nn_model1,dataset_test_1)
pred_res_1 <- unnomarlize(mod_res_1$net.result,min(io_1_original_train),max(io_1_original_train))
perform_1 <- evaluateResult(io_1_original_test,pred_res_1)
act_pred_com_1 <- actual_predict_table(io_1_original_test,pred_res_1)

#===============model 2========================
mod_res_2 <- neuralnet::compute(nn_model2,dataset_test_1)
pred_res_2 <- unnomarlize(mod_res_2$net.result,min(io_1_original_train),max(io_1_original_train))
perform_2 <- evaluateResult(io_1_original_test,pred_res_2)
act_pred_com_2 <- actual_predict_table(io_1_original_test,pred_res_2)


#===============model 3========================
mod_res_3 <- neuralnet::compute(nn_model3,dataset_test_2)
pred_res_3 <- unnomarlize(mod_res_3$net.result,min(io_2_original_train),max(io_2_original_train))
perform_3 <- evaluateResult(io_2_original_test,pred_res_3)
act_pred_com_3 <- actual_predict_table(io_2_original_test,pred_res_3)

#===============model 4========================
mod_res_4 <- neuralnet::compute(nn_model4,dataset_test_2)
pred_res_4 <- unnomarlize(mod_res_4$net.result,min(io_2_original_train),max(io_2_original_train))
perform_4 <- evaluateResult(io_2_original_test,pred_res_4)
act_pred_com_4 <- actual_predict_table(io_2_original_test,pred_res_4)

#===============model 5========================
mod_res_5 <- neuralnet::compute(nn_model5,dataset_test_2)
pred_res_5 <- unnomarlize(mod_res_5$net.result,min(io_2_original_train),max(io_2_original_train))
perform_5 <- evaluateResult(io_2_original_test,pred_res_5)
act_pred_com_5 <- actual_predict_table(io_2_original_test,pred_res_5)


#Comparison Table
nn_comparison_table <- cbind(
  rbind(
    nn_in1,
    nn_in2,
    nn_in3,
    nn_in4,
    nn_in5
  ),
  rbind(
    perform_1,
    perform_2,
    perform_3,
    perform_4,
    perform_5
  )
)
colnames(nn_comparison_table) <- c(
  "inputs",
  "hidden layers",
  "nodes",
  "act.fct",
  "RMSE",
  "MAE",
  "MAPE",
  "sMAPE"
)

rownames(nn_comparison_table) <- c(
  "Model 1",
  "Model 2",
  "Model 3",
  "Model 4",
  "Model 5"
)
nn_comparison_table
