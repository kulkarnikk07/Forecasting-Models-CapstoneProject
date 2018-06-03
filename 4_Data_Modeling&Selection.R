##############################################################
#this file is loaded after data pre-processing is done

##Now to create time series objects and start the data modeling
#install.packages("padr")
#install.packages("forecast")
#install.packages("zoo")

require(padr)
require(forecast)
require(zoo)

#Sorting the final dataframe by levels of store id and then data:
final_df <- read.csv("final_dataset.csv", header = TRUE, na.strings = '')

#storing unique restaurants in a variable:
res <- levels(final_df$air_store_id)

#final_result:
result_df <- data.frame(matrix(nrow = 0, ncol = 6), stringsAsFactors = FALSE)
colnames(result_df) <- c("air_store_id", "Model_No.", "Selected_Model", "Training_R2", "Testing_R2", "RMSLE")

result <- result_df

#function for inserting the output
df_insert_result <- function (id,mod_n, mod, r2, r2_t, error) {
  if (mod == 12121){
    m <- "This time series is too short to forecast!"
  } else {
    m <- as.character(eval(as.name(mod)))
  }
  row1 <- c(id, mod_n, m, r2, r2_t, error)
  result[nrow(result)+1,] <- row1
  return(result)
}

#iteration for each restaurant:
for (i in res) {
  temp <- final_df[grep(i,final_df$air_store_id),]
  
  ###the following didn't work because the function ifelse returned values the same shape as the condition,
  ##and the vlaues are always either 1 or 0.
  #temp_x <- ifelse(sum(temp$visitors_reserved)==0,temp[,c(12:15)], temp[,c(11:15)])
  
  temp_y <- temp$log_visitors
  #temp_x - independent variables:
  ## we kept minimum 40 values for reserves data to be present for it be considered useful for the model. 
  ## and we also check for scenarios when there are no closed_days
  if (length(which(temp$visitors_reserved != 0)) < 40 | sum(temp$closed_days) == 0){
    if (length(which(temp$visitors_reserved != 0)) < 40 & sum(temp$closed_days) == 0) {
      temp_x <- temp[,c(13:15)]
    }else {
      if (length(which(temp$visitors_reserved != 0)) < 40) {
        temp_x <- temp[,c(12:15)]
      }else {
        temp_x <- temp[,c(11,13:15)]
      }
    }
  }else {
    temp_x <- temp[,c(11:15)]
  }
  
  #time series objects  
  temp_ts_y <- ts(temp_y, frequency = 7)
  temp_ts_x <- ts(temp_x, frequency = 7)
  
  
  ## checking for short time series
  if (length(temp_ts_y) < 70) {
    temp_result_df<- df_insert_result(i,"No Model",12121, "The length"," of ts is:", length(temp_ts_y))
    result_df <- rbind(result_df, temp_result_df)
    next
  } 
  
  #building the training and testing test
  dtrain <- subset(temp_ts_y, end = length(temp_ts_y)-39)
  dtrain_x <- subset(temp_ts_x, end = dim(temp_ts_x)[1]-39 )
  #dtrain_x <- subset(ts_x, end = length(ts_dat)-39)
  dtest <- subset(temp_ts_y, start = length(temp_ts_y)-38)
  dtest_x <- subset(temp_ts_x, start = dim(temp_ts_x)[1]-38)
  
  #########################################################
  ######################################################
  ## Data Modeling with time series techniques
  # 1) Exponential Smoothing Method:
  mod_01 <- ets(dtrain, allow.multiplicative.trend = TRUE)
  f_cast_01 <- forecast(mod_01, h = 39)
  plot(f_cast_01)
  cr_01 <- checkresiduals(f_cast_01)
  ac_01 <- accuracy(f_cast_01, dtest)
  #RMSLE
  rmsle_01 <- ac_01[2,"RMSE"]
  train_r2_01 <- cor(fitted(mod_01), dtrain)^2
  test_r2_01 <- cor(f_cast_01$mean, dtest)^2
  
  ########################################################
  # 2) Time Series Decomposition:
  mod_02 <- stl(dtrain, s.window = 7)
  f_cast_02 <- forecast(mod_02, h = 39)
  ## Ploting the various components in the time series
  plot(mod_02)
  plot(f_cast_02)
  cr_02 <- checkresiduals(f_cast_02)
  ac_02 <- accuracy(f_cast_02, dtest)
  #RMSLE
  rmsle_02 <- ac_02[2,"RMSE"]
  #not able calculate the train R2, since the fitted values are not found.
  train_r2_02 <- "NA for STL"
  test_r2_02 <- cor(f_cast_02$mean, dtest)^2
  
  ########################################################
  # 3) Multiple Linear Regression
  #This one is for selecting random train and test sets
  #set.seed(1)
  #id_train <- sample(1:nrow(temp_y), nrow(dat)-39)
  #id_test <- setdiff(1:nrow(temp_y), id.train)
  #selecting train and test set from dataframe because lm() has trouble predicting time series objects
  train <- length(temp_y)-39
  test <- train +1
  l_train <- temp_y[1:train]
  l_test <- temp_y[test:length(temp_y)]
  l_train_x <- temp_x[1:train,]
  l_test_x <- temp_x[test:nrow(temp_x),]
  df_temp <- cbind(l_train, l_train_x)
  
  if (dim(temp_x)[2]==5) {
    mod_03 <- lm(l_train ~ log_reserves + closed_days + mon_thurs + friday + saturday, data = df_temp)
  } else {
    if(length(which(temp$visitors_reserved != 0)) < 40 & (sum(temp$closed_days) == 0)) {
      mod_03 <- lm(l_train ~ mon_thurs + friday + saturday, data = df_temp)
    }else if (length(which(temp$visitors_reserved != 0)) < 40) {
      mod_03 <- lm(l_train ~ closed_days + mon_thurs + friday + saturday, data = df_temp)
    }else {
      mod_03 <- lm(l_train ~ log_reserves + mon_thurs + friday + saturday, data = df_temp)
    }
  }
  
  f_cast_03 <- predict(mod_03, l_test_x)
  plot(mod_03)
  cr_03 <- checkresiduals(f_cast_03)
  ac_03 <- accuracy(f_cast_03, l_test)
  #RMSLE
  rmsle_03 <- ac_03[,"RMSE"]
  #r2
  train_r2_03 <- cor(fitted(mod_03), l_train)^2
  test_r2_03 <- cor(f_cast_03, l_test)^2
  #require(hydroGOF)
  #rmse(l_test, p)
  #sqrt(mean((l_test - p)^2, na.rm=T))
  
  ##################################################3
  # 4) ARIMA Model
  mod_04 <- auto.arima(dtrain, start.p = 0, start.q = 0, start.P = 0, start.Q = 0, ic = "bic")
  f_cast_04 <- forecast(mod_04, h = 39)
  #plot(mod_04)
  plot(f_cast_04)
  ##summary(f_cast_04)
  cr_04 <- checkresiduals(f_cast_04)
  ##checking the accuracy
  ac_04 <- accuracy(f_cast_04, dtest)
  #RMSLE for train and test set (its RMSLE because we have taken log for all values except dummies)
  rmsle_04 <- ac_04[2,"RMSE"]
  #calculating R2 for train set the model
  train_r2_04 <- cor(fitted(mod_04), dtrain)^2
  #calculating R2 for test set of the model
  test_r2_04 <- cor(f_cast_04$mean, dtest)^2
  
  ###################################################
  # 5) Regression with ARIMA errors ( Dynamic Regression)
  mod_05 <- auto.arima(dtrain, start.p = 0, start.q = 0, start.P = 0, start.Q = 0, xreg= dtrain_x, ic = "bic")
  f_cast_05 <- forecast(mod_05, h = 39, xreg = dtest_x)
  plot(f_cast_05)
  ##summary(f_cast_05)
  cr_05 <- checkresiduals(f_cast_05)
  ##checking the accuracy
  ac_05 <- accuracy(f_cast_05, dtest)
  #RMSLE for train and test set (its RMSLE because we have taken log for all values except dummies)
  rmsle_05 <- ac_05[2,"RMSE"]
  #calculating R2 for train set the model
  train_r2_05 <- cor(fitted(mod_05), dtrain)^2
  #calculating R2 for test set of the model
  test_r2_05 <- cor(f_cast_05$mean, dtest)^2
  
  ######################################################################
  ##### Selecting the best model based on RMSE and r2:
  name <- c("Exponentail Smoothing", "Time Series Decomposition", "Multi Linear Regression", "ARIMA", "Dynamic Regression")
  RMSLE <- c(rmsle_01, rmsle_02, rmsle_03, rmsle_04, rmsle_05)
  #replacing NA with 0 if any
  RMSLE[is.na(RMSLE)] <- 0
  names(RMSLE) <- name
  R2_test <- c(test_r2_01, test_r2_02, test_r2_03, test_r2_04, test_r2_05)
  R2_test[is.na(R2)] <- 0
  R2_train <- c(train_r2_01, train_r2_02, train_r2_03, train_r2_04, train_r2_05)
  R2_train[is.na(R2_train)] <- 0
  names(R2_test) <- name
  names(R2_train) <- name
  #variables needed for insertion
  tex <- "model_0@"
  t1 <- "model_01"
  assign(t1, f_cast_01$method)
  t2 <- "model_02"
  assign(t2, f_cast_02$method)
  t3 <- "model_03"
  assign(t3, mod_03$call[2])
  t4 <- "model_04"
  assign(t4, f_cast_04$method)
  t5 <- "model_05"
  assign(t5, f_cast_05$method)
  
  for (j in 1:length(R2_test)){
    if (RMSLE[j] == min(RMSLE)) {
      temp_result_df<- df_insert_result(i,gsub("0@",j,tex), gsub("@",j,tex),R2_train[j], R2_test[j], RMSLE[j])
    }
  }
  ##################################################
  #Checking RMSLE forst and then R^2 in the model.
  
  # for (j in 1:length(R2)){
  #   if (RMSLE[j] == min(RMSLE)) {
  #     if (R2[j] == max(R2)){
  #       temp_result_df<- df_insert_result(i,gsub("0@",j,tex), gsub("@",j,tex),R2[j], RMSLE[j])
  #       break
  #     }else if (R2[j] == sort(R2,TRUE)[2]) {
  #       temp_result_df<- df_insert_result(i,gsub("0@",j,tex), gsub("@",j,tex),R2[j], RMSLE[j])
  #       break
  #     }
  #   }else if(x_var == 4){ #this is done to handle few models with 2nd min RMSLE and 2nd max R2:
  #     for (k in 1:length(R2)) {
  #       if (RMSLE[k] == sort(RMSLE)[2]){
  #         if (R2[k] == max(R2)){
  #           temp_result_df<- df_insert_result(i,gsub("0@",k,tex), gsub("@",k,tex),R2[k], RMSLE[k])
  #           break
  #         }else if (R2[k] == sort(R2,TRUE)[2]) {
  #           temp_result_df<- df_insert_result(i,gsub("0@",k,tex), gsub("@",k,tex),R2[k], RMSLE[k])
  #           break
  #         }
  #       }
  #     }
  # 
  #   }
  #   x_var = x_var + 1
  # }
  result_df <- rbind(result_df, temp_result_df)
}
#exporting the result file
write.csv(result_df, "forecasting_results.csv", row.names = FALSE)