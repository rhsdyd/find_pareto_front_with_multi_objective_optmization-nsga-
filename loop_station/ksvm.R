ksvm <- function(train, test){
  
  train_task = makeRegrTask(id = 'train', data = train, target = 'output')
  test_task=makeRegrTask(id="test",data=test,target="output")
  ksvm_learner = makeLearner("regr.ksvm")
  ksvm_learner$par.set
  
  ps = makeParamSet(
    makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
    makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
  )
  ctrl = makeTuneControlGrid()
  rdesc = makeResampleDesc("CV", iters = 2L)
  res= tuneParams("regr.ksvm", train_task,resampling =rdesc, par.set = ps,control = ctrl)
  
  tuned_ksvm= makeLearner("regr.ksvm",par.vals = list(C=res$x$C,sigma=res$x$sigma))
  model<- train(tuned_ksvm,train_task)
  
  return(model)
}
