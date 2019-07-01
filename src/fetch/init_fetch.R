source("./src/api/R_Client.R")
source("./src/api/api_client.R")

fetch_init_data <- function (function_number) {
  grid_sequence = seq(-4.5, 4.5, 3)
  df_input = expand.grid(x = grid_sequence, y = grid_sequence, z = grid_sequence)
  
  result = fetch_data(df_input = df_input, fun = function_number)
  return(result)
}

df_function_1 = fetch_init_data(1)
# write.csv(df_function_1, 'api_function_1.csv')

df_function_2 = fetch_init_data(2)
# write.csv(df_function_2, 'api_function_2.csv')