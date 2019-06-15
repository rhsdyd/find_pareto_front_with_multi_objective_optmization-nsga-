library(mlr)
library(rpart)


regr_part <- function(df_learn, df_test) {
  regr.task.learn <- makeRegrTask(id="regrlearn", data=df_learn, target="output")
  regr.task.test <- makeRegrTask(id="regrtest", data=df_test, target="output")
  
  regr.learner <- makeLearner("regr.rpart", par.vals=list(
    minsplit=4, maxdepth=20, cp=0.001))
  
  regr.model <- train(regr.learner, regr.task.learn)
  response <- as.data.frame(predict(regr.model, regr.task.test))
  
  return(response)
}