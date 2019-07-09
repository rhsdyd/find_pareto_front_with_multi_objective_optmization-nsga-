library(plot3D)

visualize_3D = function (df, from = 1, to = nrow(df), main = "", i = NULL) {
  dataframe = df[from:to,]
  scatter3D(
    dataframe$x, dataframe$y, dataframe$z,
    xlim=c(-5,5),
    ylim=c(-5,5),
    zlim=c(-5,5),
    colvar = dataframe$output, clim=c(min(df[,4]), max(df[,4])), clab = "Output Value",
    main = main,
    theta= ifelse(!is.null(i), i*5, 70), phi = 10,

    pch = 20
  )
}



# video for presentation
par(mfrow = c(1, 2))
visualize_3D(df_function_1, 407, round(407 + 1 * 27), "Function 1", i = 1)
visualize_3D(df_function_2, 407, round(407 + 1 * 27), "Function 2", i = 1)

for (i in 1:36) {
  visualize_3D(df_function_1, 407, round(407 + i * 27), "Function 1", i = i)
  visualize_3D(df_function_2, 407, round(407 + i * 27), "Function 2", i = i)
  title("Initial Dataset ", outer = TRUE, line = -1, cex=4)

  Sys.sleep(0.1)
}
