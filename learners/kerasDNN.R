source("/home/elivengood/Documents/Summer19/DA2/CaseStudy/nn_functions.R")
source("./api/api_helper.R")
library(plot3D)
require(keras)
library(tidyverse)

#request data
df_3d = createxyz(1500,-5,5)
data <- as.data.frame(fetch_test_data(df_3d, fun=2, dim=3, token=token, base=base))

#normalize data scale 0-1 and viaualize a bit
data = data %>% mutate_all(normalize)
hist(data$output)
scatter3D(x=data$x,y=data$y,z=data$z,colvar = data$output)

#scaling data
data$output = scale(data$output)
# check that we get mean of 0 and sd of 1
colMeans(data$output)  # faster version of apply(scaled.dat, 2, mean)
apply(data$output, 2, sd)

#split train & test data
set.seed(420)
train = sample_frac(tbl = data, replace = FALSE, size = 0.8)
test = anti_join(data, train)

#create deep neural network
build_model <- function() {
  model <- keras_model_sequential() %>%
    layer_dense(units = 120, activation = "relu", input_shape = dim(train)[2] - 1) %>%
    #layer_dropout(rate = 0.4) %>%
    layer_dense(units = 100, activation = "relu") %>%
    #layer_dropout(rate = 0.3) %>%
    layer_dense(units = 75, activation = "relu") %>%
    #layer_dropout(rate = 0.2) %>%
    layer_dense(units = 50, activation = "relu") %>%
    #layer_dropout(rate = 0.1) %>%
    layer_dense(units = 25, activation = "relu") %>%
    layer_dense(units = 15, activation = "relu") %>%
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
  epochs = 100,
  #batch_size= 32,
  shuffle=T,
  verbose=2
)

predicted_test = model %>% predict(data.matrix(test[,1:3]))
plot(test$output, predicted_test[,1])
abline(0,1)

idx = 1:nrow(test)
plot(idx, test$output, type = "l")
points(idx, predicted_test, col="red", type = "l")
  