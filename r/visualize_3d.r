if(!exists("apirequest", mode="function")) source("./case_study/r/R_Client.R")


perform_lookup = function(x, y) {
  input = data.frame(x = x, y = y)
  response = apirequest(input, 1, "api-test2D")
  
  return(data.frame(x = x, y = y, result = response))
}

## new dataframe 
df <- data.frame(matrix(ncol = 3, nrow = 0))
colnames <- c("x", "y", "result")
colnames(df) <- colnames

for(i in 1:100) {
  x = runif(50, -6, 6)
  y = runif(50, -6, 6)
  df = rbind(df, perform_lookup(x, y))
}

for(i in 1:36) {
  points3D(df$x, df$y, df$result, theta = i * 10, phi = i, pch = 19, cex = 0.5)
  Sys.sleep(0.1)
}
