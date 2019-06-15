library(plot3D)

compare_prediction <- function(df, prediction) {
  #compares dataframe.output column to prediction.response column
  
  df["estimate"] <- prediction[,'response']
  df["diff"] <- abs(df$output - df$estimate)
  
  points3D(df$x, df$y, df$output, colvar=df$diff,
    theta = 70, phi = 40, cex = 0.5, pch=19, bty="b2")
}