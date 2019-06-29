mse_cal <- function(a,b){
  m1=sum((a-b)^2)
  m=m1/length(a)
  return(m)
}

train_model <- function(f_number){
  if(f_number == 1) {
    data=d_f1
  }
  else {
    data=d_f2
  }
  n_samples = floor(0.8 * nrow(data))
  sequences = seq_len(nrow(data))
  sample_ids = sample(sequences, size=n_samples)
  
  train = data[sample_ids, ]
  test = data[-sample_ids, ]
  
  model_ksvm = ksvm(train,test)
  model_keras = keras_ann(train,test)
  
  pred_ksvm=predict(model_ksvm,newdata=test)
  mse_ksvm =performance(pred = pred_ksvm)
  
  pred_keras = model_keras %>% predict(data.matrix(test[,1:3]))
  mse_keras = mse_cal(test$output, pred_keras[,1])
  
  compare_frame = data.frame(model = c("model_ksvm","model_keras"), mse = c(mse_ksvm,mse_keras))
  
  get_model_name = compare_frame[which.min(compare_frame[,2]),1]
  
  message('-------------------------')
  message(paste('model : ',get_model_name))
  message('-------------------------')
  
  
  if(get_model_name == compare_frame[1,1]){
    return(model_ksvm)
  }
  else if(get_model_name == compare_frame[2,1]){
    return(model_keras)
  }

}