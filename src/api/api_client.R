source("src/api/R_Client.R")
source("src/utils/redundance_filter.R")

fetch_data <- function(df_input, fun) {
  # split into chunks of 50 rows
  splitted = split(df_input, (as.numeric(rownames(df_input)) - 1) %/% 50)
  output = vector(mode="numeric", length=0)
  
  api = "api-test3D"
  base = "optim.uni-muenster.de:5000/"
  token = "5d5ff737873440f7989f234f821f125e"

  for(data in splitted) {
    response = apirequest(data, fun, api, token, base)
    output = c(output, response)
  }

  df_input["output"] = output
  return(df_input)
}

fetch_non_redundant_data <- function(df, df_input, fun) {
  df_input = filter_redundancies_within_df(df_input)
  df_input = filter_redundancies_for_df(df, df_input)
  
  return(fetch_data(df_input, fun))
}
