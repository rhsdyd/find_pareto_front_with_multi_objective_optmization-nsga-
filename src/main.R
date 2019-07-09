setwd("case_study/")

source("src/api/api_client.R")
source("src/fetch/init_fetch.R")
source("src/multi_objective/multi_objective.R")

multi_objective_optimization_result = multi_objective_optimization(df_function_1, df_function_2, 150, store_data_frame_each_iteration = FALSE)