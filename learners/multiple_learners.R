require(mlr)

tasks = list(makeRegrTask(id = 'Test 3D - 2', data = data, target = 'result'))

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

# Cross-Validation
cv = makeResampleDesc('CV', iters=20)
bmr_cv = benchmark(learners, tasks, cv)
plotBMRBoxplots(bmr_cv)

# Leave-one-out
loo = makeResampleDesc('LOO')
bmr_loo = benchmark(learners, tasks, loo)
plotBMRBoxplots(bmr_loo)

# Bootstrap
bootstrap = makeResampleDesc('Bootstrap', iters=20)
bmr_bootstrap = benchmark(learners, tasks, bootstrap)
plotBMRBoxplots(bmr_bootstrap)
