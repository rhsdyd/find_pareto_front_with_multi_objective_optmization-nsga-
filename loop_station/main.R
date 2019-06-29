library(keras)
library(MASS)
library(ecr)
library(ggplot2)
library(gridExtra)
library(smoof)
library(mlr)
source("case_study/api/R_Client.R")
source("case_study/api/api_helper.R")
source("ksvm.R")
source("keras_ann.R")
source("train_model.R")
source("nsga_pareto.R")

base="optim.uni-muenster.de:5000/"
token="5d5ff737873440f7989f234f821f125e"

x = seq(-5,5,length.out = 7)
y = seq(-5,5,length.out = 7)
z = seq(-5,5,length.out = 7)

point = expand.grid(x=x,y=y,z=z)

d_f1=fetch_test_data(point,1,3,token,base)
d_f2=fetch_test_data(point,2,3,token,base)

pareto_front_history = list()

#main

for (i in 1:5){
  if( i == i){
    point = point
    d_f1 = d_f1
    d_f2 = d_f2
  }
  
  else{
    combined = rbind(point, pareto_front[,1:3])
    rownames(combined) = NULL
    combined[-(duplicated(combined) == TRUE),]
    new_point = nrow(combined[-(1:nrow(point)),])
    point = combined
    
    new_d_f1=fetch_test_data(new_point,1,3,token,base)
    new_d_f2=fetch_test_data(new_point,2,3,token,base)
    
    d_f1 = rbind(d_f1,new_d_f1)
    d_f2 = rbind(d_f2,new_d_f2)
  }
  
  model_f1 = train_model(1)
  model_f2 = train_model(2)
  
  mu = 20L
  iter = 100L
  
  lower=c(-5,-5,-5)
  upper = c(5,5,5)
  
  m_set1 = setup(mutPolynomial, eta = 25, p = 0.2, lower = lower, upper = upper)
  rec_set1 = setup(recSBX, eta = 15, p = 0.7, lower = lower, upper = upper)
  
  m_set2 = setup(mutGauss, p = 0.2, sdev = 0.05, lower = lower, upper = upper)
  rec_set2 = setup(recSBX, eta = 15, p = 0.7, lower = lower, upper = upper)
  
  nsga_1 = generate_nsga(mu,m_set1,rec_set1,iter)
  nsga_2 = generate_nsga(mu,m_set2,rec_set2,iter)
  
  nsga_list = list(nsga_1,nsga_2)
  pareto_front = best_pareto(nsga_list)
  
  pareto_front_history[[i]] = pareto_front
  # pl = ggplot(pareto_front[,4:5],aes(x= y1, y= y2)) + xlab('f1') + ylab('f2') + ggtitle("Pareto Front") + 
  #   geom_line(colour = 'red') +geom_point(colour = 'black')
  # print(pl)
  
}

##visual option 1
myplot <- function(df){
  pl = ggplot(df[,4:5],aes(x= y1, y= y2)) + xlab('f1') + ylab('f2') + ggtitle(paste("Pareto Front")) + 
    geom_line(colour = 'red') +geom_point(colour = 'black')

  return(print(pl))
}

p <- lapply(pareto_front_history, myplot)

do.call(grid.arrange, c(p))

#visuall option2
myplot2 <- function(df,i){
  pl = ggplot(df[,4:5],aes(x= y1, y= y2)) + xlab('f1') + ylab('f2') + ggtitle(paste("Pareto Front iter:",i)) + 
    geom_line(colour = 'red') +geom_point(colour = 'black')
  
  return(print(pl))
}

p2 = list()

for(i in 1:length(pareto_front_history)){
  p2[[i]] = myplot2(pareto_front_history[[i]],i)  
}

grid.arrange(grobs=p2)


