source("./src/api/R_Client.R")
source("./src/api/api_client.R")

fetch_init_data <- function (function_number) {
  grid_sequence = seq(-4.5, 4.5, by = 3)
  df_input = expand.grid(x = grid_sequence, y = grid_sequence, z = grid_sequence)

  result = fetch_data(df_input = df_input, fun = function_number)
  return(result)
}

fetch_init_data_length_out = function (function_number) {
  grid_sequence = seq(-5, 5, length.out = 7)
  df_input = expand.grid(x = grid_sequence, y = grid_sequence, z = grid_sequence)

  result = fetch_data(df_input = df_input, fun = function_number)
  return(result)
}


df_function_1 = fetch_init_data(1)
df_function_2 = fetch_init_data(2)

df_function_1 = rbind(df_function_1, fetch_init_data_length_out(1))
df_function_2 = rbind(df_function_2, fetch_init_data_length_out(2))

# write.csv(df_function_2, 'data/df_2_init.csv')
# write.csv(df_function_2, 'api_function_2.csv')
