grid_sequence = seq(-5, 5, 0.2)
grid = expand.grid(x = grid_sequence, y = grid_sequence, z = grid_sequence)
grid_normalized = as.data.frame(lapply(grid, normalize))

minvec = sapply(grid_normalized, min)
maxvec = sapply(grid_normalized, max)

new_data_points = data.frame(x = double(), y = double(), z = double(), output = double())

rnorm_point = function (value) {
  # TODO: find optimal sd 
  rnorm(1, value, 0.1)
}

findMinimum = function () {
  return(TRUE)
}

for(i in 1:10) {
  model = train_model()
  prediction_result = data.frame(grid_normalized, result = model %>% predict(data.matrix(grid_normalized)))
  
  if(findMinimum()) {
    ordered_prediction_result = prediction_result[order(prediction_result$result),]
  }
  else {
    ordered_prediction_result = prediction_result[order(prediction_result$result, decreasing = TRUE),]
  }
  
  denormalized_prediction_result = as.data.frame(Map(denormalize, ordered_prediction_result[1:3], minvec, maxvec))
  best_point = denormalized_prediction_result[1,]
  
  best_point_rnorm = apply(best_point, 2, rnorm_point)
  
  new_data_point = perform_lookup(best_point_rnorm[1], best_point_rnorm[2], best_point_rnorm[3], function_number = 2)
  df_function_2 = rbind(df_function_2, new_data_point)
  
  new_data_points = rbind(new_data_points, new_data_point)
  print(new_data_point)
}
