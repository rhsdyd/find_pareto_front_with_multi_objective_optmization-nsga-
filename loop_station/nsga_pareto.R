fitness_function_predict = function(point) {
  x = point[1]
  y = point[2]
  z = point[3]
  
  decision_space = data.frame(x, y, z)
  pred_f1 = tryCatch(predict(model_f1, newdata = decision_space), error = function(e) model_f1%>% predict(data.matrix(decision_space[,1:3])))
  pred_f2 = tryCatch(predict(model_f2, newdata = decision_space), error = function(e) model_f2%>% predict(data.matrix(decision_space[,1:3])))
  
  objective_space = data.frame(f1=pred_f1$data, f2=pred_f2$data)
  return (t(objective_space))
}

generate_nsga = function(mu,m_set,rec_set,iter){

  pareto_predict = nsga2(fitness_function_predict, n.objectives = 2, n.dim = 3, lower = lower, upper = upper, mu = mu, lambda = mu,
                         mutator = m_set, 
                         recombinator = rec_set, 
                         terminators = list(stopOnIters(iter)))
  
  return(pareto_predict)
}

best_pareto = function(nsga_list){
  pareto_all_list = list()
  for (i in 1:length(nsga_list)){
    pareto_predict = nsga_list[[i]]
    
    pareto_set=data.frame(x=0,y=0,z=0)
    for( j in 1:length(pareto_predict$pareto.set)){
      pareto_set=rbind(pareto_set,pareto_predict$pareto.set[[j]])
      
    }
    pareto_set=pareto_set[-1,]
    pareto_all = data.frame(pareto_set,pareto_predict$pareto.front)
    rownames(pareto_all) = NULL
    
    pareto_all_list[[i]] = pareto_all
    if(i == 1){
      f1_max = max(pareto_all$y1)
      f2_max = max(pareto_all$y2)
    }
    else{
      f1_max = max(f1_max,pareto_all$y1)
      f2_max = max(f1_max,pareto_all$y2)
    }
  }
  HV_vector = c()
  for ( i in 1:length(pareto_all_list)){
    HV = computeHV(t(pareto_all_list[[i]][,4:5]), ref.point = c(f1_max,f2_max))
    HV_vector = append(HV_vector,HV)
  }
  
  
  idx <- which.max(HV_vector)
  
  
  message('-------------------------')
  message(paste('nsga_',idx))
  message('-------------------------')

  pareto_front = data.frame(pareto_all_list[[idx]])
  
  
  return(pareto_front)
}

