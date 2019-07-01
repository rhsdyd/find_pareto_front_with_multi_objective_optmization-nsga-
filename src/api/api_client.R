source("src/api/R_Client.R")

api = "api-test3D"

fetch_data = function(df_input, fun) {
  # split into chunks of 50 rows
  splitted = split(df_input, (as.numeric(rownames(df_input)) - 1) %/% 50)
  output = vector(mode="numeric", length=0)
  
  base = "optim.uni-muenster.de:5000/"
  token = "5d5ff737873440f7989f234f821f125e"

  for(data in splitted) {
    response = apirequest(data, fun, api, token, base)
    output = c(output, response)
  }

  df_input["output"] = output
  return(df_input)
}
