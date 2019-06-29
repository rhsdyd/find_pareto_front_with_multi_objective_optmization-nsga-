build_model <- function(train) {
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

keras_ann <- function(train,test){
  
  model = build_model(train)

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