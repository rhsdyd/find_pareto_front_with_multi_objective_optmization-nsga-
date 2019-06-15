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

get_closest_2D <- function(df, row) {
  same_x <- df[df$x == df$x[row], ]
  same_y <- df[df$y == df$y[row], ]
  
  same_x["diff"] <- abs(same_x$y - df$y[row])
  same_x <- same_x[same_x$diff > 0, ]
  
  same_y["diff"] <- abs(same_y$x - df$x[row])
  same_y <- same_y[same_y$diff > 0, ]
  
  closest <- rbind(
    same_x[same_x$diff == min(same_x$diff), ],
    same_y[same_y$diff == min(same_y$diff), ])
  # TODO: maybe filter min of x and y combined
  
  return(closest)
}

most_isolated <-  function(df) {
  max_dist <- 0
  
  for (row in 1:nrow(df)) {
    closest <- get_closest_2D(df, row)
    closest["diff"] <- abs(df[row, "output"] - closest$output)
    
    if (max_dist < max(closest$diff)) {
      max_dist <- max(closest$diff)
      neighbour = subset(closest[closest$diff == max_dist, ], select = -c(diff))
      most_isolated <- rbind(df[row,], neighbour)
    }
  }
  
  return(most_isolated)
}

fetch_by_distance <- function(df) {
  #Sys.sleep(5)
  iso = most_isolated(df)
  # new_df <- expand.grid(x=mean(iso$x),y=mean(iso$y))
  # return(fetch_test_data(new_df, fun=1, dim=2, token, base))
  
  if (iso[1,"x"] == iso[2,"x"]){
    y_mean <- mean(iso$y)
    new_df <- expand.grid(x=unique(df$x),y=y_mean)
    return(fetch_test_data(new_df, fun=2, dim=2, token, base))
  }
  else {
    x_mean <- mean(iso$x)
    new_df <- expand.grid(x=x_mean,y=unique(df$y))
    return(fetch_test_data(new_df, fun=2, dim=2, token, base))
  }
}