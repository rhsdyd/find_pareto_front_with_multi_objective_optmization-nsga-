require(ecr)

source("src/api/api_client.R")
source("src/model/benchmark.R")
source("src/multi_objective/nsga.R")

multi_objective_optimization <- function () {
  pareto_front_history = list()
  
  for (i in 1:8){
    model_f1 = benchmark_placeholder(df_function_1)
    model_f2 = benchmark_placeholder(df_function_2)
    
    pareto_front = determine_pareto_front(model_f1, model_f2)
    pareto_front_history[[i]] = pareto_front
    
    new_pareto_set = pareto_front[1:3]
    new_data_f1 = fetch_data(new_pareto_set, 1)
    new_data_f2 = fetch_data(new_pareto_set, 2)

    df_function_1<<- rbind(df_function_1, new_data_f1)
    df_function_2<<- rbind(df_function_2, new_data_f2)
  }
  
  return(pareto_front_history)
}

determine_pareto_front <- function (model_f1, model_f2) {
  pareto_front_list = determine_nsga_pareto_fronts(model_f1, model_f2)
  return(determine_best_pareto_front(pareto_front_list))
}

determine_best_pareto_front <- function(pareto_front_list){
  pareto_all_list = list()
  for (i in 1:length(pareto_front_list)){
    pareto_predict = pareto_front_list[[i]]
    
    pareto_set <- matrix(ncol = 3, nrow = 0)
    colnames(pareto_set) <- c("x", "y", "z")
    
    for(j in 1:length(pareto_predict$pareto.set)){
      pareto_set = rbind(pareto_set, pareto_predict$pareto.set[[j]])
    }
    
    pareto_all = data.frame(pareto_set, pareto_predict$pareto.front)
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
  
  # find largest Hypervolume
  HV_vector = c()
  for (i in 1:length(pareto_all_list)){
    HV = computeHV(t(pareto_all_list[[i]][,4:5]), ref.point = c(f1_max, f2_max))
    HV_vector = append(HV_vector,HV)
  }
  
  idx <- which.max(HV_vector)
  result = data.frame(pareto_all_list[[idx]])
  
  return(result)
}