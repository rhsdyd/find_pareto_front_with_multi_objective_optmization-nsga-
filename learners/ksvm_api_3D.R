setwd("D:\\Muenster_class\\semester2\\DA2\\project\\code")

source("R_Client.R")
library(MASS)

fetch3D <- function(df,f_value){
  output = vector(mode="numeric", length=0)
  for(i in 0:19){
    a = 1 + (50*i)
    b = 50 + (50*i)
    data = df[a:b,]
    response = apirequest(data, f_value, "api-test3D")
    output = c(output, response)
  }
  df["result"] <- output
  return(df)
}
mse_cal <- function(a,b){
  m1=sum((a-b)^2)
  m=m1/length(a)
  return(m)
}

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

x = runif(1000,-5,5)
y = runif(1000,-5,5)
z = runif(1000,-5,5)
d1 = data.frame(x,y,z)
d_f1=fetch3D(d1,1)
d_f2=fetch3D(d1,2)

###function1
d.scale<- as.data.frame(lapply(d_f1,normalize))
n_samples = floor(0.8 * nrow(d.scale))
sequences = seq_len(nrow(d.scale))
sample_ids = sample(sequences, size=n_samples)
d_train = d.scale[sample_ids, ]
d_test = d.scale[-sample_ids, ]
summary(d_train)
train_task = makeRegrTask(id = 'train', data = d_train, target = 'result')
test_task=makeRegrTask(id="test",data=d_test,target="result")
ksvm_learner = makeLearner("regr.ksvm")
ksvm_learner$par.set

ps = makeParamSet(
  makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
)
ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 2L)
res = tuneParams("regr.ksvm", train_task,resampling =rdesc, par.set = ps,control = ctrl)
print(res)
summary(res)
res$opt.path
res$x$sigma
tuned_ksvm = makeLearner("regr.ksvm",par.vals = list(C=res$x$C,sigma=res$x$sigma))
ksvm<-train(tuned_ksvm,train_task)
response = predict(ksvm, test_task)

idx <-1:nrow(d_test)
plot(idx,d_test$result,type = "l")
points(idx,response$data[,3],type = "l",col="red")
plot(response$data[,3],d_test$result)
mse_cal(d_test$result,response$data[,3])

#function2
d.scale<- as.data.frame(lapply(d_f2,normalize))
n_samples = floor(0.8 * nrow(d.scale))
sequences = seq_len(nrow(d.scale))
sample_ids = sample(sequences, size=n_samples)
d_train = d.scale[sample_ids, ]
d_test = d.scale[-sample_ids, ]
summary(d_train)
train_task = makeRegrTask(id = 'train', data = d_train, target = 'result')
test_task=makeRegrTask(id="test",data=d_test,target="result")
ksvm_learner = makeLearner("regr.ksvm")
ksvm_learner$par.set

ps = makeParamSet(
  makeNumericParam("C", lower = -12, upper = 12, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
)
ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 2L)
res = tuneParams("regr.ksvm", train_task,resampling =rdesc, par.set = ps,control = ctrl)
print(res)
summary(res)
res$opt.path
res$x$sigma
tuned_ksvm = makeLearner("regr.ksvm",par.vals = list(C=res$x$C,sigma=res$x$sigma))
ksvm<-train(tuned_ksvm,train_task)
response = predict(ksvm, test_task)

idx <-1:nrow(d_test)
plot(idx,d_test$result,type = "l")
points(idx,response$data[,3],type = "l",col="red")
plot(response$data[,3],d_test$result)
mse_cal(d_test$result,response$data[,3])
