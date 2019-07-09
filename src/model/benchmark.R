library(mlr)
library(keras)

# returns most accurate model based on input training data
# 20% of training data randomly picked serves as holdout for validation
# accuracy is compared by MSE
# no cross validation is used because keras doesn't support it out of the box and
#   it would slow down the benchmark even more
benchmark_models <- function(dataframe) {
  smp_size <- floor(0.8 * nrow(dataframe)) # all benchmarks use same test set
  train_ind <- sample(seq_len(nrow(dataframe)), size = smp_size)

  train_df <- dataframe[train_ind, ]
  test_df <- dataframe[-train_ind, ]

  mlr_output <- benchmark_mlr(train_df, test_df)
  keras_output <- benchmark_keras(train_df, test_df)

  print(paste("MLR MSE:", mlr_output[[2]] , sep=" "))
  print(paste("Keras MSE:", keras_output[[2]] , sep=" "))

  if(mlr_output[[2]] < keras_output[[2]]){ # compare MSE
    print("Best model overall: MLR")
    return (mlr_output) # return best mlr model plus MSE
  }
  else {
    print("Best model overall: Keras")
    return (keras_output) # return best keras model plus MSE
  }
}

benchmark_keras_single <- function(
    train_x, train_y, test_x, test_y, learning_rate, units, opti) {
  model <- keras_model_sequential()

  model %>%
    layer_dense(units = units, activation = "relu", input_shape = c(3)) %>%
    layer_dense(units = units, activation = "relu") %>%
    layer_dense(units = 1)

  model %>% compile(
    loss = 'mse',
    optimizer = opti(lr=learning_rate),
    metrics = list("mean_squared_error")
  )

  history <- model %>% fit(
    train_x, train_y,
    epochs = 400,
    validation_split = 0,
    verbose = 0
  )

  model_eval <- model %>% evaluate(test_x, test_y)
  return(list(model, model_eval$mean_squared_error)) # return model + mse tuple
}

# benchmark for different keras dense networks configs
# input: training data frame, test data frame
# output: trained model with optimal hyperparams plus mse
benchmark_keras <- function(training_df, test_df) {
  train_x <- data.matrix(training_df[, -4])
  train_y <- data.matrix(training_df[, 4])
  test_x <- data.matrix(test_df[, -4])
  test_y <- data.matrix(test_df[, 4])

  learning_rates = list(0.001, 0.002, 0.005, 0.01, 0.02, 0.05)
  units <- list(128, 256, 512, 1024)
  optimizers <- list(optimizer_adagrad, optimizer_adam, optimizer_rmsprop, optimizer_adamax)

  hyperparam_grid <- expand.grid(lr=learning_rates, units=units, opti=optimizers)
  min_mse = 99999999999999999999.0

  for (row in 1:nrow(hyperparam_grid)) {
    keras_output <- benchmark_keras_single(
      train_x, train_y, test_x, test_y,
      hyperparam_grid[[row, "lr"]], hyperparam_grid[[row, "units"]], hyperparam_grid[[row, "opti"]])

    if (keras_output[[2]] < min_mse) {
      min_mse <- keras_output[[2]]
      best_model <- keras_output[[1]]
      # write row number to csv to check hyperparams later
      capture.output(row, file = "keras_benchmark_info.txt")
    }
  }

  return(list(best_model, min_mse)) # return model + mse tuple
}

# benchmark for several mlr regression learners
# input: training data frame, test data frame
# output: trained model with optimal hyperparams plus mse
benchmark_mlr <- function(training_df, test_df) {
  full_df <- rbind(training_df, test_df)
  size <- nrow(full_df)
  train_ind <- seq_len(nrow(training_df))
  validation_ind <- seq.int(max(train_ind) + 1, size)

  task = makeRegrTask(data=full_df, target="output")
  ctrl = makeTuneControlGrid()

  res_ksvm = tuneParams( # 6*4*6*5 = 720
    "regr.ksvm", task = task,
    resampling = makeFixedHoldoutInstance(train_ind, validation_ind, size),
    par.set = makeParamSet(
      makeDiscreteParam("C", values = c(0.5, 1.0, 1.5, 2.0, 2.5, 3)),
      makeDiscreteParam("sigma", values = c(0.5, 1.0, 1.5, 2.0)),
      makeDiscreteParam("epsilon", values = c(0.01, 0.05, 0.1, 0.2, 0.5, 1)),
      makeDiscreteParam("tol", values = c(0.0005, 0.001, 0.002, 0.005, 0.01))),
    control = ctrl, measures = list(mse), show.info = TRUE)

  res_rf = tuneParams( # 8*5*5*3 = 600
    "regr.randomForest", task = task,
    resampling = makeFixedHoldoutInstance(train_ind, validation_ind, size),
    par.set = makeParamSet(
      makeDiscreteParam("ntree", values = c(100,300,500,700,900,1100,1300,1500)),
      makeDiscreteParam("nodesize", values = c(1,3,5,7,9)),
      makeDiscreteParam("nPerm", values = c(1,2,3,4,5)),
      makeDiscreteParam("mtry", values = c(1,2,3))),
    control = ctrl, measures = list(mse), show.info = TRUE)

  res_nnet = tuneParams( # 5*6 = 30
    "regr.nnet", task = task,
    resampling = makeFixedHoldoutInstance(train_ind, validation_ind, size),
    par.set = makeParamSet(
      makeDiscreteParam("maxit", values = c(100, 300, 500, 700, 900)),
      makeDiscreteParam("size", values = c(16, 32, 64, 128, 4, 8))),
    control = ctrl, measures = list(mse), show.info = TRUE)

  res_gausspr = tuneParams( # 6*6*6 = 216
    "regr.gausspr", task = task,
    resampling = makeFixedHoldoutInstance(train_ind, validation_ind, size),
    par.set = makeParamSet(
      makeDiscreteParam("kernel", values = c("rbfdot", "polydot", "vanilladot", "tanhdot", "laplacedot", "besseldot")),
      makeDiscreteParam("var", values = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5)),
      makeDiscreteParam("tol", values = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5))),
    control = ctrl, measures = list(mse), show.info = TRUE)

  res_rpart = tuneParams( # 5*5*5 = 125
    "regr.rpart", task = task,
    resampling = makeFixedHoldoutInstance(train_ind, validation_ind, size),
    par.set = makeParamSet(
      makeDiscreteParam("minsplit", values = c(4, 8, 16, 32, 64)),
      makeDiscreteParam("maxdepth", values = c(4, 8, 16, 32, 64)),
      makeDiscreteParam("cp", values = c(0.001, 0.005, 0.01, 0.05, 0.1))),
    control = ctrl, measures = list(mse), show.info = TRUE)

  mses = c(res_ksvm$y[[1]], res_rf$y[[1]], res_nnet$y[[1]], res_gausspr$y[[1]], res_blm$y[[1]], res_rpart$y[[1]])

  # create learner based on the best tuning result
  if (min(mses) == res_ksvm$y[[1]]) {
    print("Best MLR model: KSVM")
    learner <- makeLearner("regr.ksvm", par.vals=list(
      C=res_ksvm$x$C,
      sigma=res_ksvm$x$sigma,
      epsilon=res_ksvm$x$epsilon,
      tol=res_ksvm$x$tol), config = list(show.learner.output = FALSE))
  }
  else if (min(mses) == res_rf$y[1]) {
    print("Best MLR model: Random Forest")
    learner <- makeLearner("regr.randomForest", par.vals=list(
      ntree=res_rf$x$ntree,
      nodesize=res_rf$x$nodesize,
      nPerm=res_rf$x$nPerm,
      mtry=res_rf$x$mtry), config = list(show.learner.output = FALSE))
  }
  else if (min(mses) == res_nnet$y[1]) {
    print("Best MLR model: NNET")
    learner <- makeLearner("regr.nnet", par.vals=list(
      maxit=res_nnet$x$maxit,
      size=res_nnet$x$size), config = list(show.learner.output = FALSE))
  }
  else if (min(mses) == res_gausspr$y[1]) {
    print("Best MLR model: GAUSSPR")
    learner <- makeLearner("regr.gausspr", par.vals=list(
      kernel=res_gausspr$x$kernel,
      tol=res_gausspr$x$tol,
      var=res_gausspr$x$var), config = list(show.learner.output = FALSE))
  }
  else if (min(mses) == res_rpart$y[1]) {
    print("Best MLR model: RPART")
    learner <- makeLearner("regr.rpart", par.vals=list(
      minsplit=res_rpart$x$minsplit,
      maxdepth=res_rpart$x$maxdepth,
      cp=res_rpart$x$cp), config = list(show.learner.output = FALSE))
  }
  
  task <- makeRegrTask(id='train', data=full_df, target='output')
  model <- train(learner, task)  # no subset, learn for all, model not further validated
  return(list(model, min(mses))) # return model + mse tuple
}

#fast model built from benchmark results
train_model_f1 <- function(dataframe) {
  learner <- makeLearner("regr.ksvm", par.vals=list(
    C=2.5, sigma=1, epsilon=0.01, tol=0.0005), config = list(show.learner.output = FALSE))

  rdesc = makeResampleDesc("CV", iters = 5)
  task <- makeRegrTask(id = 'train', data = as.matrix(dataframe), target = 'output')
  model <- train(learner, task)
  sampled <- resample(learner, task, rdesc)
  
  # mse = sampled$aggr[[1]]
  # write(mse, file="data/mse/mse_ksvm", append=TRUE)

  return(model)
}

train_model_f2 <- function(dataframe) {
  train_x <- data.matrix(dataframe[, -4])
  train_y <- data.matrix(dataframe[, 4])
  
  model <- keras_model_sequential()
  
  model %>%
    layer_dense(units = 128, activation = "relu", input_shape = c(3)) %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = 'mse',
    optimizer = optimizer_adamax(lr=0.05),
    metrics = list("mean_squared_error")
  )
  
  history <- model %>% fit(
    train_x, train_y,
    epochs = 400,
    validation_split = 0.2,
    verbose = 0
  )
  
  mse = history$metrics$val_loss[length(history$metrics$val_loss)]
  write(mse, file="data/mse/mse_keras", append=TRUE)
  print(paste('MSE Keras:', mse))
  
  return(model)
}

train_model_f1_final <- function(dataframe) {
  train_x <- data.matrix(dataframe[, -4])
  train_y <- data.matrix(dataframe[, 4])
  
  model <- keras_model_sequential()
  
  model %>%
    layer_dense(units = 1024, activation = "relu", input_shape = c(3)) %>%
    layer_dense(units = 1024, activation = "relu") %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = 'mse',
    optimizer = optimizer_rmsprop(lr=0.05),
    metrics = list("mean_squared_error")
  )
  
  history <- model %>% fit(
    train_x, train_y,
    epochs = 400,
    validation_split = 0,
    verbose = 2
  )
  
  return(model)
}

train_model_f2_final = function (dataframe) {
  learner <- makeLearner("regr.randomForest", par.vals=list(
    ntree = 100, nodesize = 5, nPerm = 2, mtry = 3
  ), config = list(show.learner.output = FALSE))
  
  rdesc = makeResampleDesc("CV", iters = 5)
  task <- makeRegrTask(id = 'train', data = dataframe, target = 'output')
  model <- train(learner, task)
  sampled <- resample(learner, task, rdesc)
  
  return(model)
}
