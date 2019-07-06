setwd("case_study/")

source("src/multi_objective/multi_objective.R")

multi_objective_result = multi_objective_optimization(df_function_1, df_function_2,100, store_data_frame_each_iteration = FALSE)

multi_objective_result$df_1
multi_objective_result$df_2
multi_objective_result$history_models_pareto_front
multi_objective_result$non_dominated_sorting_history


domsort = multi_objective_result$non_dominated_sorting_history

plot(domsort[[1]], ylim=c(0, 100), xlim=c(0, 3000))
for(i in 2:length(domsort)) {
  points(domsort[[i]], col=i)
  Sys.sleep(1)
}
