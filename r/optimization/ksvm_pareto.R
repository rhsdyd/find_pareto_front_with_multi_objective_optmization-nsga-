setwd("D:\\Muenster_class\\semester2\\DA2\\project\\code")
library(keras)
library(MASS)
library(ecr)
library(ggplot2)
library(smoof)
#install_keras()

source("R_Client.R")



fetch_test_data <- function(df, fun, dim, token, base) {
  # split into chunks of 50 rows
  splitted <- split(df, (as.numeric(rownames(df)) - 1) %/% 50)
  output <- vector(mode="numeric", length=0)
  
  if (dim == 2) {
    api <- "api-test2D"
  } else if (dim == 3) {
    api <- "api-test3D"
  }
  
  for(data in splitted) {
    response = apirequest(data, fun, api, token, base)
    output <- c(output, response)
  }
  
  df["output"] <- output
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

base="optim.uni-muenster.de:5000/"
token="5d5ff737873440f7989f234f821f125e"

x = runif(1000,-5,5)
y = runif(1000,-5,5)
z = runif(1000,-5,5)
d1 = data.frame(x,y,z)
d_f1=fetch_test_data(d1,1,3,token,base)
d_f2=fetch_test_data(d1,2,3,token,base)

data = d_f1
data<- as.data.frame(lapply(data,normalize))
data1 = data

data = d_f2
data<- as.data.frame(lapply(data,normalize))
data2 = data

d=data.frame(f1=data1$output,f2=data2$output)

nondom_ecr <- d[which.nondominated(t(d)),]
plot(d)
points(nondom_ecr, col='red')

#rank
r=doNondominatedSorting(t(d))
d1=d
d1['rank']=r$ranks
d1
d1[d1['rank']==1,]
nondom_ecr


fn <- function(i) {
  x <- i[1]
  y <- i[2]
  z <- i[3]
  data = data.frame(x,y,z)
  d_f1=fetch_test_data(data,1,3,token,base)
  d_f2=fetch_test_data(data,2,3,token,base)
  dt = data.frame(f1=d_f1$output,f2=d_f2$output)
  s=as.matrix(dt)
  return (s)
}


##mco
library(mco)
r1 <- nsga2(fn, idim=3, odim=2 ,
            generations=32, popsize=32,
            cprob=0.7, cdist=20,
            mprob=0.2, mdist=20,
            lower.bounds=c(-5,-5,-5),
            upper.bounds=c(5,5,5))
plot(r1)
help("nsga2")

#ecr #not working  Ctrl+Shift+C
library(ecr)
fn_ecr <- function(i) {
  x <- i[1]
  y <- i[2]
  z <- i[3]
  data = data.frame(x,y,z)
  d_f1=fetch_test_data(data,1,3,token,base)
  d_f2=fetch_test_data(data,2,3,token,base)
  dt = data.frame(f1=d_f1$output,f2=d_f2$output)
  return (t(dt))
}



lower=c(-5,-5,-5)
upper = c(5,5,5)
MU = 30L
p=nsga2(fn_ecr,n.objectives = 2, n.dim = 3, minimize = NULL,
        lower = lower, upper = upper, mu = MU, lambda = MU,
        mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = lower, upper =
                          upper), recombinator = setup(recSBX, eta = 15, p = 0.7, lower = lower, upper
                                                       = upper), terminators = list(stopOnIters(100L)))


####ksvm + pareto
library(mlr)

#f1
#d1=d_f1
#data<- as.data.frame(lapply(d1,normalize))
#data['output']=d_f1[,4]d1=d_f1[,1:3]
#d

#d1=d_f1
#data<- as.data.frame(lapply(d1,normalize))
data=d_f1
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
ksvm_f1<-train(tuned_ksvm_f1,train_task)
response = predict(ksvm_f1, test_task)
mse_cal(test$output,response$data[,3])
plot(test$output,response$data[,3])
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
ksvm_f2<-train(tuned_ksvm_f2,train_task)
response = predict(ksvm_f2, test_task)
mse_cal(test$output,response$data[,3])
plot(test$output,response$data[,3])
c=data.frame(x=1,y=1,z=1)

####pareto
fn2 <- function(i) {
  x <- i[1]
  y <- i[2]
  z <- i[3]
  input_data = data.frame(x,y,z)
  pred_f1=predict(ksvm_f1, newdata =  input_data)
  pred_f2=predict(ksvm_f2, newdata =  input_data)
  dt = data.frame(f1=pred_f1$data,f2=pred_f2$data)
  s=as.matrix(dt)
  return (s)
}


#library(mco)
r2 <- nsga2(fn2, idim=3, odim=2 ,
            generations=150, popsize=100,
            cprob=0.7, cdist=20,
            mprob=0.2, mdist=20,
            lower.bounds=c(-5,-5,-5),
            upper.bounds=c(5,5,5))

plot(r2,xlab="f1",ylab="f2")

#ecr
fn2_ecr <- function(i) {
  x <- i[1]
  y <- i[2]
  z <- i[3]
  input_data = data.frame(x,y,z)
  pred_f1=predict(ksvm_f1, newdata =  input_data)
  pred_f2=predict(ksvm_f2, newdata =  input_data)
  dt = data.frame(f1=pred_f1$data,f2=pred_f2$data)
  s=t(dt)
  return (s)
}

lower=c(-5,-5,-5)
upper = c(5,5,5)
MU = 20L
p_pre=nsga2(fn2_ecr, n.objectives = 2, n.dim = 3, minimize = NULL,
            lower = lower, upper = upper, mu = MU, lambda = MU,
            mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = lower, upper =
                              upper), recombinator = setup(recSBX, eta = 15, p = 0.7, lower = lower, upper
                                                           = upper), terminators = list(stopOnIters(100L)))
plot(p)
plot(p$pareto.front,xlab="f1",ylab="f2")
points(p_pre$pareto.front,col="red")
p_pre$pareto.set
plot(p_pre$pareto.front)
p_pre$task

d=data.frame(f1,f2)

idxs<-which.nondominated(t(d))

plot(d)
points(d[idxs,],col = "green")
points(p$pareto.front,col="red")
points(p_pre$pareto.front,col="blue")
