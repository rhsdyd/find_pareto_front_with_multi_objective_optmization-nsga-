library(keras)

data = as.data.frame(lapply(df_function_2, normalize))

n_samples = floor(0.75 * nrow(data))
sequences = seq_len(nrow(data))
sample_ids = sample(sequences, size=n_samples)

train = data[sample_ids, ]
test = data[-sample_ids, ]

build_model <- function() {
  model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", input_shape = dim(train)[2] - 1) %>%
    layer_dropout(rate=0.4) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  
  return(model)
}

model = build_model()
summary(model)

history <- model %>% fit(
  x = data.matrix(train[,1:3]), # features  
  y = data.matrix(train$result), # labels
  validation_data = list(data.matrix(test[,1:3]), data.matrix(test$result)),
  epochs = 50,
  validation_split = 0.2,
  shuffle=T,
  verbose=2
)

predicted = model %>% predict(data.matrix(test[,1:3]))
plot(test$result, predicted[,1], xlim = c(0, 1), ylim = c(0, 1))

