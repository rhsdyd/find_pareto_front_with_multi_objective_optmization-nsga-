setwd("case_study/")

source("src/multi_objective/multi_objective.R")

multi_objective_result = multi_objective_optimization(df_function_1, df_function_2)

multi_objective_result$df_1
multi_objective_result$df_2
multi_objective_result$pareto_front_history