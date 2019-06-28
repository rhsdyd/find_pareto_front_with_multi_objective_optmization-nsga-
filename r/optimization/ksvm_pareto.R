library(keras)
library(MASS)
library(ecr)
library(ggplot2)
library(smoof)

source("case_study/api/R_Client.R")
source("case_study/api/api_helper.R")
source("case_study/utils/data_processing.R")

base="optim.uni-muenster.de:5000/"
token="5d5ff737873440f7989f234f821f125e"


# TODO: talk about the amount of fetch points!
x = runif(1000, -5, 5)
y = runif(1000, -5, 5)
z = runif(1000, -5, 5)

d = data.frame(x,y,z)
d_f1 = fetch_test_data(d, 1, 3, token, base)
d_f2 = fetch_test_data(d, 2, 3, token, base)

objective_space = data.frame(f1 = d_f1$output, f2 = d_f2$output)
non_dominated = objective_space[which.nondominated(t(objective_space)),]

plot(objective_space)
points(non_dominated, col = 'red')


fitness_function_real <- function(point) {
  x <- point[1]
  y <- point[2]
  z <- point[3]
  
  decision_space = data.frame(x, y, z)
  # change to predict from model
  data_f1 = fetch_test_data(decision_space, 1, 3, token, base)
  data_f2 = fetch_test_data(decision_space, 2, 3, token, base)
  
  objective_space = data.frame(f1 = data_f1$output, f2 = data_f2$output)
  
  return (t(objective_space))
}

lower = c(-5, -5, -5)
upper = c(5, 5, 5)
mu = 30L

pareto_real = nsga2(fitness_function_real, n.objectives = 2, n.dim = 3, lower = lower, upper = upper, mu = mu, lambda = mu, 
                mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = lower, upper = upper),
                recombinator = setup(recSBX, eta = 15, p = 0.7, lower = lower, upper = upper),
                terminators = list(stopOnIters(100L))
               )




####ksvm + pareto
library(mlr)

data = d_f1
f1<-data$output
n_samples = floor(0.8 * nrow(data))
sequences = seq_len(nrow(data))
sample_ids = sample(sequences, size=n_samples)

train = data[sample_ids, ]
test = data[-sample_ids, ]

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
res_f1 = tuneParams("regr.ksvm", train_task,resampling =rdesc, par.set = ps,control = ctrl)

tuned_ksvm_f1 = makeLearner("regr.ksvm",par.vals = list(C=res_f1$x$C,sigma=res_f1$x$sigma))
model_f1 <- train(tuned_ksvm_f1,train_task)
pred = predict(model_f1, test_task)
performance(pred = pred)
plot(test$output,pred$data[,3])
#f2

# d2=d_f2[,1:3]
# data<- as.data.frame(lapply(d2,normalize))
# data['output']=d_f2[,4]

#d2=d_f2
#data<- as.data.frame(lapply(d2,normalize))
data = d_f2
f2<-data$output
n_samples = floor(0.8 * nrow(data))
sequences = seq_len(nrow(data))
sample_ids = sample(sequences, size=n_samples)

train = data[sample_ids, ]
test = data[-sample_ids, ]

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
res_f2 = tuneParams("regr.ksvm", train_task,resampling =rdesc, par.set = ps,control = ctrl)

tuned_ksvm_f2 = makeLearner("regr.ksvm",par.vals = list(C=res_f2$x$C,sigma=res_f2$x$sigma))
model_f2 <- train(tuned_ksvm_f2, train_task)
pred = predict(model_f2, test_task)
mse_f2 = performance(pred = pred)
plot(test$output,response$data[,3])

####pareto
fitness_function_predict = function(point) {
  x = point[1]
  y = point[2]
  z = point[3]
  
  decision_space = data.frame(x, y, z)
  pred_f1 = predict(model_f1, newdata = decision_space)
  pred_f2 = predict(model_f2, newdata = decision_space)
  
  objective_space = data.frame(f1=pred_f1$data, f2=pred_f2$data)
  return (t(objective_space))
}

lower=c(-5,-5,-5)
upper = c(5,5,5)
mu = 20L

pareto_predict = nsga2(fitness_function_predict, n.objectives = 2, n.dim = 3, lower = lower, upper = upper, mu = mu, lambda = mu,
              mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = lower, upper = upper), 
              recombinator = setup(recSBX, eta = 15, p = 0.7, lower = lower, upper = upper), 
              terminators = list(stopOnIters(100L)))

plot(pareto_predict$pareto.front, xlab="f1", ylab = "f2")
points(pareto_predict$pareto.front,col="red")
p_pre$task

d=data.frame(f1,f2)

idxs<-which.nondominated(t(d))

plot(d)
points(d[idxs,],col = "green")
points(p$pareto.front,col="red")
points(p_pre$pareto.front,col="blue")
