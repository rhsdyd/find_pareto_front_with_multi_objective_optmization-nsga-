tasks = list(makeRegrTask(id = 'function 1', data = df_normalized, target = 'result'))

learners = list(
  makeLearner('regr.ksvm'), 
  makeLearner('regr.randomForest'),
  makeLearner('regr.gausspr'),
  makeLearner('regr.glm'),
  makeLearner('regr.lm'),
  makeLearner('regr.nnet'),
  makeLearner('regr.featureless'),
  makeLearner('regr.rpart'),
  makeLearner('regr.rvm'),
  makeLearner('regr.svm')
  )

rdesc = makeResampleDesc("CV", iters = 20)

bmr = benchmark(learners, tasks, rdesc)
