require(ecr)

source("src/api/api_client.R")
source("src/model/benchmark.R")
source("src/multi_objective/nsga.R")

multi_objective_optimization <- function (df_1, df_2,loop_iter, store_data_frame_each_iteration = FALSE) {
  history_pareto_front_models = list()
  history_pareto_front_data = list()
  learning_model_f1 = list()
  learning_model_f2 = list()
  data_to_train_f1 = list()
  data_to_train_f2 = list()
  
  for (i in 1:loop_iter) {
    print(paste(i, loop_iter, sep = "/"))
    
    data_to_train_f1[[i]] = df_1
    data_to_train_f2[[i]] = df_2
    model_f1 = benchmark_placeholder(df_1)[[1]]
    model_f2 = benchmark_placeholder(df_2)[[1]]
    learning_model_f1[[i]] = model_f1
    learning_model_f2[[i]] = model_f2
    
    pareto_front = determine_pareto_front(model_f1, model_f2)
    
    history_pareto_front_models[[i]] = pareto_front
    
    new_pareto_set = pareto_front[1:3]
    new_data_f1 = fetch_non_redundant_data(df_1, new_pareto_set, 1)
    new_data_f2 = fetch_non_redundant_data(df_2, new_pareto_set, 2)

    df_1 = rbind(df_1, new_data_f1)
    df_2 = rbind(df_2, new_data_f2)
    
    if(store_data_frame_each_iteration == TRUE) {
      write.csv(df_1, paste('data/df_1_', i, '.csv', sep=""))
      write.csv(df_2, paste('data/df_2_', i, '.csv', sep=""))
    }
    
    history_pareto_front_data[[i]] = get_non_dominated_sortings(df_1, df_2)
  }
  
  result = list('last_data_df1' = df_1, 'last_data_f2' = df_2, 'data_to_train_f1' = data_to_train_f1, 'data_to_train_f2' = data_to_train_f2, 
                'learning_model_f1' = learning_model_f1, 'learning_model_f2' = learning_model_f1,
                'history_pareto_front_models' = history_pareto_front_models, 'history_pareto_front_data' = history_pareto_front_data)
  return(result)
}

get_non_dominated_sortings = function (df_1, df_2) {
  outputs = data.frame(x1 = df_1[,4], x2 = df_2[,4])
  return(outputs[which.nondominated(t(outputs)),])
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