source("src/api/api_client.R")

# fill data with grid_sequence  >> -5, 0, 5 <<
fill_data_boundaries = function (function_number) {
  grid_sequence = seq(-5, 5, 5)
  grid = expand.grid(x = grid_sequence, y = grid_sequence, z = grid_sequence)
  result = fetch_data(grid, fun = function_number)
  
  return(result)
}

# fill data with grid_sequence  >> -3, -1, 1, 3 <<
fill_data_3 = function (function_number) {
  grid_sequence = seq(-3, 3, 2)
  grid = expand.grid(x = grid_sequence, y = grid_sequence, z = grid_sequence)
  result = fetch_data(grid, fun=function_number)
  
  return(result)
}

# fill data with grid_sequence >> -4, -2, 0, 2, 4 <<
fill_data_4 = function (function_number) {
  grid_sequence = seq(-4, 4, 2)
  grid = expand.grid(x = grid_sequence, y = grid_sequence, z = grid_sequence)
  result = fetch_data(grid, fun = function_number)
  
  return(result)
}

df_function_1 = rbind(df_function_1, fill_data_boundaries(1))
df_function_1 = rbind(df_function_1, fill_data_3(1))
df_function_1 = rbind(df_function_1, fill_data_4(1))
# write.csv(df_function_1, 'api_function_1.csv')

df_function_2 = rbind(df_function_2, fill_data_boundaries(2))
df_function_2 = rbind(df_function_2, fill_data_3(2))
df_function_2 = rbind(df_function_2, fill_data_4(2))
# write.csv(df_function_2, 'api_function_2.csv')