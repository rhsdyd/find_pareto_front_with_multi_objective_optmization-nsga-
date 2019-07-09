tracking <- function(result){
  require(ecr)
  df_track = data.frame(idg_real_model = double())
  
  for (i in 1:length(result$history_pareto_front_data)){
    real = result$history_pareto_front_data[[i]]
    model = result$history_pareto_front_models[[i]]
    idg_r_m = computeInvertedGenerationalDistance(t(model[,4:5]) ,t(real), p = 1, normalize = FALSE, dist.fun = computeEuclideanDistance)
    df_track = rbind(df_track,c(idg_r_m))
  }
  colnames(df_track) = c('idg_real_model')
  return(df_track)
}

plot_igd_over_time <- function (tracking_data) {
  idx = c(1:nrow(tracking_data))
  plot(idx,tracking_data$idg_real_model,ty='l',col='red')
  legend(145, 480, legend=c("idg_truth_model", "idg_truth_real", "idg_real_model"),
         col=c("black","red", "green"), lty=1, cex=0.5)
}

# usage = plot_igd_over_time(tracking(multi_objective_optimization_result))