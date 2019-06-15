source("./api/R_Client.R")

fetch_test_data <- function(df, fun, dim, token, base) {
  # split into chunks of 50 rows
  splitted <- split(df, (as.numeric(rownames(df)) - 1) %/% 50)
  output <- vector(mode="numeric", length=0)
  
  if (dim == 2) {
    api <- "api-test2D"
  } else if (dim == 3) {
    api <- "api-test3D"
  }
  
  for(data in splitted) {
    response = apirequest(data, fun, api, token, base)
    output <- c(output, response)
  }
  
  df["output"] <- output
  return(df)
}
