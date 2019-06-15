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

# use "furthest neighbour approach for sampling"
for(i in seq(from=1, to=10, by=1)){
  new_obs <- fetch_by_distance(df)
  df <- rbind(df, new_obs)
  
  points3D(df$x, df$y, df$output, colvar=df$output,
           theta = 70, phi = 40, cex = 1, pch=19, bty="b2")
}
