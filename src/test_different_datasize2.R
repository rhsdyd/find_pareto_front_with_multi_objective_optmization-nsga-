### initi 64 iter 180

multi_objective_result1_2 = multi_objective_optimization(df4_function_1, df4_function_2, 180, store_data_frame_each_iteration = FALSE)

track1_2 = tracking(multi_objective_result1_2)

multi_objective_result1_2$learning_model_f1


###  inital 125 iter 150

multi_objective_result2_2 = multi_objective_optimization(df5_function_1, df5_function_2, 150, store_data_frame_each_iteration = FALSE)

track2_2 = tracking(multi_objective_result2_2)



### inital 216 iter 120

multi_objective_result3_2 = multi_objective_optimization(df6_function_1, df6_function_2, 120, store_data_frame_each_iteration = FALSE)

track3_2 = tracking(multi_objective_result3_2)



### inital 343 iter 90

multi_objective_result4_2 = multi_objective_optimization(df7_function_1, df7_function_2, 90, store_data_frame_each_iteration = FALSE)

track4_2 = tracking(multi_objective_result4_2)

### inital 512 iter 60

multi_objective_result5_2 = multi_objective_optimization(df8_function_1, df8_function_2, 60, store_data_frame_each_iteration = FALSE)

track5_2 = tracking(multi_objective_result5_2)


### inital 729 iter 30

multi_objective_result6_2 = multi_objective_optimization(df9_function_1, df9_function_2, 30, store_data_frame_each_iteration = FALSE)

track6_2 = tracking(multi_objective_result6_2)


#save global environment 
#save.image("D:/Muenster_class/semester2/DA2/project/code/test_size_global_environment.RData")



idx = c(1:nrow(track1_2))
plot(idx,track1_2$idg_truth_model,ty='l', xlab = c('loop'), ylab= c('IGD'),main = "inital data size : 64, loop : 180")
points(idx,track1_2$idg_real_model ,ty='l',col='red')
points(idx,track1_2$idg_truth_real,ty='l',col='green')
legend(145, 480, legend=c("idg_truth_model", "idg_truth_real", "idg_real_model"),
       col=c("black","red", "green"), lty=1, cex=0.5)
true_front_line + geom_point(data=multi_objective_result1$history_pareto_front_models[[179]],aes(x= y1, y= y2), colour = "green") 
track1_2




track2_2
idx = c(1:nrow(track2_2))
plot(idx,track2_2$idg_truth_model,ty='l', xlab = c('loop'), ylab= c('IGD'), main = "inital data size : 125, loop : 150")
points(idx,track2_2$idg_real_model ,ty='l',col='red')
points(idx,track2_2$idg_truth_real,ty='l',col='green')
legend(145, 480, legend=c("idg_truth_model", "idg_truth_real", "idg_real_model"),
       col=c("black","red", "green"), lty=1, cex=0.5)

track3_2
idx = c(1:nrow(track3_2))
plot(idx,track3_2$idg_truth_model,ty='l', xlab = c('loop'), ylab= c('IGD'), main = "inital data size : 216, loop : 120")
points(idx,track3_2$idg_real_model ,ty='l',col='red')
points(idx,track3_2$idg_truth_real,ty='l',col='green')
legend(145, 480, legend=c("idg_truth_model", "idg_truth_real", "idg_real_model"),
       col=c("black","red", "green"), lty=1, cex=0.5)

cor(track4_2)
idx = c(1:nrow(track4_2))
plot(idx,track4_2$idg_truth_model,ty='l', xlab = c('loop'), ylab= c('IGD'), main = "inital data size : 343, loop : 90")
points(idx,track4_2$idg_real_model ,ty='l',col='red')
points(idx,track4_2$idg_truth_real,ty='l',col='green')
legend(145, 480, legend=c("idg_truth_model", "idg_truth_real", "idg_real_model"),
       col=c("black","red", "green"), lty=1, cex=0.5)
track4_2

idx = c(1:nrow(track5_2))
plot(idx,track5_2$idg_truth_model,ty='l',ylim=c(0,400), xlab = c('loop'), ylab= c('IGD'), main = "inital data size : 512, loop : 60")
points(idx,track5_2$idg_real_model,ty='l',col='red')
points(idx,track5_2$idg_truth_real,ty='l',col='green')
legend(145, 480, legend=c("idg_truth_model", "idg_truth_real", "idg_real_model"),
       col=c("black","red", "green"), lty=1, cex=0.5)

idx = c(1:nrow(track6_2))
plot(idx,track6_2$idg_truth_model,ty='l',ylim=c(0,400), xlab = c('loop'), ylab= c('IGD'), main = "inital data size : 729, loop : 60")
points(idx,track6_2$idg_real_model,ty='l',col='red')
points(idx,track6_2$idg_truth_real,ty='l',col='green')
legend(145, 480, legend=c("idg_truth_model", "idg_truth_real", "idg_real_model"),
       col=c("black","red", "green"), lty=1, cex=0.5)



