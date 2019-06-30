library(mlr)

benchmark_models <- function(dataframe) {
  smp_size <- floor(0.8 * nrow(dataframe))
  train_ind <- sample(seq_len(nrow(fn2_5000)), size = smp_size)
  
  train_df <- dataframe[train_ind, ]
  test_df <- dataframe[-train_ind, ]
  
  best_model <- benchmark_mlr(train_df, test_df)
  return(best_model)
}


# benchmark for several mlr regression learners
# input: training data frame, test data frame
# output: trained model with optimal hyperparams
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
    control = ctrl, measures = list(mae, mse), show.info = TRUE)
  
  # rvm crashes for unknown reasons
  
  # res_rvm = tuneParams( # 5*5*5 = 125
  #   "regr.rvm", task = task,
  #   resampling = makeFixedHoldoutInstance(train_ind, validation_ind, size),
  #   par.set = makeParamSet(
  #     makeDiscreteParam("iterations", values = c(50, 100, 200, 500, 1000))),
  #   control = ctrl, measures = list(mae, mse), show.info = TRUE)
  
  res_rf = tuneParams( # 8*5*5*3 = 600
    "regr.randomForest", task = task,
    resampling = makeFixedHoldoutInstance(train_ind, validation_ind, size),
    par.set = makeParamSet(
      makeDiscreteParam("ntree", values = c(100,300,500,700,900,1100,1300,1500)),
      makeDiscreteParam("nodesize", values = c(1,3,5,7,9)),
      makeDiscreteParam("nPerm", values = c(1,2,3,4,5)),
      makeDiscreteParam("mtry", values = c(1,2,3))),
    control = ctrl, measures = list(mae, mse), show.info = TRUE)

  res_nnet = tuneParams( # 5*6 = 30
    "regr.nnet", task = task,
    resampling = makeFixedHoldoutInstance(train_ind, validation_ind, size),
    par.set = makeParamSet(
      makeDiscreteParam("maxit", values = c(100, 300, 500, 700, 900)),
      makeDiscreteParam("size", values = c(16, 32, 64, 128, 4, 8))),
    control = ctrl, measures = list(mae, mse), show.info = TRUE)

  res_gausspr = tuneParams( # 7*6*6 = 252
    "regr.gausspr", task = task,
    resampling = makeFixedHoldoutInstance(train_ind, validation_ind, size),
    par.set = makeParamSet(
      makeDiscreteParam("kernel", values = c("rbfdot", "polydot", "vanilladot", "tanhdot", "laplacedot", "besseldot", "splinedot")),
      makeDiscreteParam("var", values = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5)),
      makeDiscreteParam("tol", values = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5))),
    control = ctrl, measures = list(mae, mse), show.info = TRUE)

  res_blm = tuneParams( # 2*6*5 = 60
    "regr.blm", task = task,
    resampling = makeFixedHoldoutInstance(train_ind, validation_ind, size),
    par.set = makeParamSet(
      makeDiscreteParam("meanfn", values = c("linear", "constant")),
      makeDiscreteParam("bprior", values = c("bflat", "b0", "bmle", "bmzt", "b0not", "bmznot")),
      makeDiscreteParam("R", values = c(1, 2, 3, 4, 5))),
    control = ctrl, measures = list(mae, mse), show.info = TRUE)

  res_rpart = tuneParams( # 5*5*5 = 125
    "regr.rpart", task = task,
    resampling = makeFixedHoldoutInstance(train_ind, validation_ind, size),
    par.set = makeParamSet(
      makeDiscreteParam("minsplit", values = c(4, 8, 16, 32, 64)),
      makeDiscreteParam("maxdepth", values = c(4, 8, 16, 32, 64)),
      makeDiscreteParam("cp", values = c(0.001, 0.005, 0.01, 0.05, 0.1))),
    control = ctrl, measures = list(mae, mse), show.info = TRUE)
  
  mses = c(res_ksvm$y[1], res_rf$y[1], res_nnet$y[1], res_gausspr$y[1], res_blm$y[1], res_rpart$y[1])
  
  # create learner based on the best tuning result
  if (min(mses) == res_ksvm$y[1]) {
    print("KSVM wins")
    learner <- makeLearner("regr.ksvm", par.vals=list(
      C=res_ksvm$x$C,
      epsilon=res_ksvm$x$epsilon,
      tol=res_ksvm$x$tol,
      sigma=res_ksvm$x$sigma))
  }
  # else if (min(mses) == res_rvm$y[1]) {
  #   learner <- makeLearner("regr.rvm", par.vals=list(
  #     minmaxdiff=res_rvm$x$minmaxdiff,
  #     # var=res_rvm$x$var,
  #     iterations=res_rvm$x$iterations))
  # }
  else if (min(mses) == res_rf$y[1]) {
    print("RF wins")
    learner <- makeLearner("regr.randomForest", par.vals=list(
      ntree=res_rf$x$ntree,
      nodesize=res_rf$x$nodesize,
      nPerm=res_rf$x$nPerm,
      mtry=res_rf$x$mtry))
  }
  else if (min(mses) == res_nnet$y[1]) {
    learner <- makeLearner("regr.nnet", par.vals=list(
      maxit=res_nnet$x$maxit,
      size=res_nnet$x$size))
  }
  else if (min(mses) == res_gausspr$y[1]) {
    learner <- makeLearner("regr.gausspr", par.vals=list(
      kernel=res_gausspr$x$kernel,
      tol=res_gausspr$x$tol,
      var=res_gausspr$x$var))
  }
  else if (min(mses) == res_blm$y[1]) {
    learner <- makeLearner("regr.blm", par.vals=list(
      meanfn=res_blm$x$meanfn,
      bprior=res_blm$x$bprior,
      R=res_blm$x$R))
  }
  else if (min(mses) == res_rpart$y[1]) {
    learner <- makeLearner("regr.rpart", par.vals=list(
      minsplit=res_rpart$x$minsplit,
      maxdepth=res_rpart$x$maxdepth,
      cp=res_rpart$x$cp))
  }
  
  task <- makeRegrTask(id='train', data=full_df, target='output')
  model <- train(learner, task)  # no subset, learn for all, model not further validated
  return(model)
}