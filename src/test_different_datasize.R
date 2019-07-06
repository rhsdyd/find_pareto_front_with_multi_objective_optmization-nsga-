source("src/multi_objective/multi_objective.R")
library(ggplot2)
library(gridExtra)
library(ggcorrplot)
pareto_truth = read.csv("pareto_truth.csv")
true_front_line = ggplot(pareto_truth,aes(x= x1, y= x2)) + xlab('f1') + ylab('f2') + 
  geom_line(colour = 'red') +geom_point(colour = 'blue')


true_front_line



computeEuclideanDistance <- function(x) sqrt(sum((x[1] - x[2]) ^ 2))

tracking <- function(result){
  df_track = data.frame(idg_truth_model = double(),
                        idg_truth_real = double(),
                        idg_real_model = double())
  
  for (i in 1:length(result$history_pareto_front_data)){
    truth = pareto_truth
    real = result$history_pareto_front_data[[i]]
    model = result$history_pareto_front_models[[i]]
    idg_t_m = computeInvertedGenerationalDistance(t(model[,4:5]) ,t(truth), p = 1, normalize = FALSE, dist.fun = computeEuclideanDistance)
    idg_t_r= computeInvertedGenerationalDistance(t(real) ,t(truth), p = 1, normalize = FALSE, dist.fun = computeEuclideanDistance)
    idg_r_m = computeInvertedGenerationalDistance(t(model[,4:5]) ,t(real), p = 1, normalize = FALSE, dist.fun = computeEuclideanDistance)
    df_track = rbind(df_track,c(idg_t_m,idg_t_r,idg_r_m))
  }
  colnames(df_track) = c('idg_truth_model','idg_truth_real','idg_real_model')
  return(df_track)
}

fetch_init_data <- function (function_number, len) {
  grid_sequence = seq(-5, 5, length.out = len)
  df_input = expand.grid(x = grid_sequence, y = grid_sequence, z = grid_sequence)
  
  result = fetch_data(df_input = df_input, fun = function_number)
  return(result)
}

### initi 64 iter 180

df4_function_1 = fetch_init_data(1,4)
df4_function_2 = fetch_init_data(2,4)
multi_objective_result1 = multi_objective_optimization(df4_function_1, df4_function_2, 180, store_data_frame_each_iteration = FALSE)

track1 = tracking(multi_objective_result1)



###  inital 125 iter 150

df5_function_1 = fetch_init_data(1,5)
df5_function_2 = fetch_init_data(2,5)
multi_objective_result2 = multi_objective_optimization(df5_function_1, df5_function_2, 150, store_data_frame_each_iteration = FALSE)

track2 = tracking(multi_objective_result2)



### inital 216 iter 120
df6_function_1 = fetch_init_data(1,6)
df6_function_2 = fetch_init_data(2,6)
multi_objective_result3 = multi_objective_optimization(df6_function_1, df6_function_2, 120, store_data_frame_each_iteration = FALSE)

track3 = tracking(multi_objective_result3)


### inital 343 iter 90
df7_function_1 = fetch_init_data(1,7)
df7_function_2 = fetch_init_data(2,7)
multi_objective_result4 = multi_objective_optimization(df7_function_1, df7_function_2, 90, store_data_frame_each_iteration = FALSE)

track4 = tracking(multi_objective_result4)

### inital 512 iter 60
df8_function_1 = fetch_init_data(1,8)
df8_function_2 = fetch_init_data(2,8)
multi_objective_result5 = multi_objective_optimization(df8_function_1, df8_function_2, 60, store_data_frame_each_iteration = FALSE)

track5 = tracking(multi_objective_result5)


### inital 729 iter 30
df9_function_1 = fetch_init_data(1,9)
df9_function_2 = fetch_init_data(2,9)
multi_objective_result6 = multi_objective_optimization(df9_function_1, df9_function_2, 30, store_data_frame_each_iteration = FALSE)

track6 = tracking(multi_objective_result6)

#visual
#correlation
ggcorrplot(cor(track6), type = "lower",
           lab = TRUE)


#par(mfrow = c(1, 1))
idx = c(1:nrow(track1))
plot(idx,track1$idg_truth_model,ty='l', xlab = c('loop'), ylab= c('IGD'),main = "inital data size : 64, loop : 180")
points(idx,track1$idg_real_model ,ty='l',col='red')
points(idx,track1$idg_truth_real,ty='l',col='green')
legend(145, 480, legend=c("idg_truth_model", "idg_truth_real", "idg_real_model"),
       col=c("black","red", "green"), lty=1, cex=0.5)


track2
idx = c(1:nrow(track2))
plot(idx,track2$idg_truth_model,ty='l', xlab = c('loop'), ylab= c('IGD'), main = "inital data size : 125, loop : 150")
points(idx,track2$idg_real_model ,ty='l',col='red')
points(idx,track2$idg_truth_real,ty='l',col='green')
legend(145, 480, legend=c("idg_truth_model", "idg_truth_real", "idg_real_model"),
       col=c("black","red", "green"), lty=1, cex=0.5)

track3
idx = c(1:nrow(track3))
plot(idx,track3$idg_truth_model,ty='l', xlab = c('loop'), ylab= c('IGD'), main = "inital data size : 216, loop : 120")
points(idx,track3$idg_real_model ,ty='l',col='red')
points(idx,track3$idg_truth_real,ty='l',col='green')
legend(145, 480, legend=c("idg_truth_model", "idg_truth_real", "idg_real_model"),
       col=c("black","red", "green"), lty=1, cex=0.5)

cor(track4)
idx = c(1:nrow(track4))
plot(idx,track4$idg_truth_model,ty='l', xlab = c('loop'), ylab= c('IGD'), main = "inital data size : 343, loop : 90")
points(idx,track4$idg_real_model ,ty='l',col='red')
points(idx,track4$idg_truth_real,ty='l',col='green')
legend(145, 480, legend=c("idg_truth_model", "idg_truth_real", "idg_real_model"),
       col=c("black","red", "green"), lty=1, cex=0.5)

idx = c(1:nrow(track5))
plot(idx,track5$idg_truth_model,ty='l',ylim=c(0,400), xlab = c('loop'), ylab= c('IGD'), main = "inital data size : 512, loop : 60")
points(idx,track5$idg_real_model,ty='l',col='red')
points(idx,track5$idg_truth_real,ty='l',col='green')
legend(145, 480, legend=c("idg_truth_model", "idg_truth_real", "idg_real_model"),
       col=c("black","red", "green"), lty=1, cex=0.5)

idx = c(1:nrow(track6))
plot(idx,track6$idg_truth_model,ty='l',ylim=c(0,400), xlab = c('loop'), ylab= c('IGD'), main = "inital data size : 729, loop : 60")
points(idx,track6$idg_real_model,ty='l',col='red')
points(idx,track6$idg_truth_real,ty='l',col='green')
legend(145, 480, legend=c("idg_truth_model", "idg_truth_real", "idg_real_model"),
       col=c("black","red", "green"), lty=1, cex=0.5)


