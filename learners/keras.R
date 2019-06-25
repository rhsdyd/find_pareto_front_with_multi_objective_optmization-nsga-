library(keras)
source("./case_study/utils/data_processing.R")


train_model = function(df) {
  data = as.data.frame(lapply(df, normalize))
  
  n_samples = floor(0.8 * nrow(data))
  sequences = seq_len(nrow(data))
  sample_ids = sample(sequences, size=n_samples)
  
  train = data[sample_ids, ]
  test = data[-sample_ids, ]
  
  build_model <- function() {
    model <- keras_model_sequential() %>%
      layer_dense(units = 120, activation = "relu", input_shape = dim(train)[2] - 1) %>%
      layer_dense(units = 75, activation = "relu") %>%
      layer_dense(units = 30, activation = "relu") %>%
      layer_dense(units = 10, activation = "relu") %>%
      layer_dense(units = 5, activation = "relu") %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = "mse",
      optimizer = 'adam',
      metrics = list("mean_absolute_error")
    )
    
    return(model)
  }
  
  model = build_model()
  summary(model)
  
  history <- model %>% fit(
    x = data.matrix(train[,1:3]), # features  
    y = data.matrix(train$output), # labels
    validation_data = list(data.matrix(test[,1:3]), data.matrix(test$output)),
    epochs = 50,
    batch_size=20,
    shuffle=T,
    verbose=2
  )
  return(model)
}

predicted_test = model %>% predict(data.matrix(test[,1:3]))
plot(test$result, predicted_test[,1], xlim = c(0, 1), ylim = c(0, 1))
plot(test$result, predicted_test[,1], main='relu')

idx = 1:nrow(test)
plot(idx, test$result, type = "l")
points(idx, predicted_test, col="red", type = "l")
