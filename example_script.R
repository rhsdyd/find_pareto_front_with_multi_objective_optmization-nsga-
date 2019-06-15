source("./api/api_helper.R")
source("./learners/decision_trees.R")
source("./visualize/2d.R")
library(plot3D)


base <- "ulr/path"
token <- "token"

# create data frame
df_2d <- expand.grid(x=seq(-5, 5, by=0.2),y=seq(-5, 5, by=0.2))

# get output data from API
df <- fetch_test_data(df_2d, fun=1, dim=2, token, base)

# create a sample for the learning set
df.learn <- df[sample(nrow(df), 500), ]

# creating the model and get a prediction
result <- regr_part(df.learn, df)

# compare original data and prediction
compare_prediction(df, result)
