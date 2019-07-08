library(plot3D)

visualize_3D = function (df, from = 1, to = nrow(df), main = "") {
  dataframe = df[from:to,]
  scatter3D(
    dataframe$x, dataframe$y, dataframe$z,
    colvar = dataframe$output, clim=c(min(df[,4]), max(df[,4])), clab = "Output Value",
    main = main,
    theta= 70, phi = 20,
    pch = 20
  )
}


par(mfrow = c(1,2))
visualize_3D(df_function_1, 1, 407, "Initial Dataset Function 1")
visualize_3D(df_function_2, 1, 407, "Initial Dataset Function 2")
