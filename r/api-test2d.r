require(plot3D)
source("./case_study/r/R_Client.R")

perform_lookup = function(x, y, function_number) {
  input = data.frame(x = x, y = y)
  response = apirequest(input, function_number, "api-test2D")
  
  return(data.frame(x = x, y = y, result = response))
}

load_data = function (function_number) {
  df <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames <- c("x", "y", "result")
  colnames(df) <- colnames
  
  for(i in 1:100) {
    x = runif(50, -5, 5)
    y = runif(50, -5, 5)
    df = rbind(df, perform_lookup(x, y, function_number = function_number))
  }
  return(df)
}

df_function_1 = load_data(function_number = 1)
df_function_2 = load_data(function_number = 2)

### visualize function 1
for(i in 1:36) {
  points3D(df_function_1$x, df_function_1$y, df_function_1$result, theta = i * 10, phi = i, pch = 19, cex = 0.5, main="Function 1")
  Sys.sleep(0.1)
}

## visualize function 2
for(i in 1:36) {
  points3D(df_function_2$x, df_function_2$y, df_function_2$result, theta = i * 10, phi = i, pch = 19, cex = 0.5, main="Function 2")
  Sys.sleep(0.1)
}
