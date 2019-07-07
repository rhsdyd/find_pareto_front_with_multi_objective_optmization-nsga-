source("./multi_objective/multi_objective.R")
source("api_helper.R")
source("R_client.R")

###run comb1 comb2 first
fitness_function <- function(point) {
  x = point[1]
  y = point[2]
  z = point[3]
  
  decision_space = data.frame(x, y, z)
  pred_f1 = tryCatch(predict(model_f1, newdata = decision_space), error = function(e) model_f1 %>% predict(data.matrix(decision_space[,1:3])))
  pred_f2 = tryCatch(predict(model_f2, newdata = decision_space), error = function(e) model_f2 %>% predict(data.matrix(decision_space[,1:3])))
  
  objective_space = data.frame(f1 = pred_f1, f2 = pred_f2)
  return (t(objective_space))
}


do_nsga_for_hyperparam_set <- function (hyperparam_set) {
  nsga2(fitness_function, n.objectives = 2, n.dim = 3, lower = lower, upper = upper, mu = mu, lambda = mu,
        mutator = hyperparam_set$mutator,
        recombinator = hyperparam_set$recombinator,
        terminators = list(stopOnIters(iter)))
}

base="optim.uni-muenster.de:5000/"
token="5d5ff737873440f7989f234f821f125e"

#no loop just simple precedure
#train the model with 1000 observations for each function
x = seq(-5,5,length.out = 10)
y = seq(-5,5,length.out = 10)
z = seq(-5,5,length.out = 10)
point = expand.grid(x=x,y=y,z=z)


d_f1=fetch_test_data(point,1,3,token,base)
d_f2=fetch_test_data(point,2,3,token,base)

model_f1 = benchmark_placeholder(d_f1)
model_f2 = benchmark_placeholder(d_f2)

mu = 20L
iter = 50L

lower = c(-5, -5, -5)
upper = c(5, 5, 5)
nrow(result)
hyperparam_sets = result

pareto_front_list = apply(hyperparam_sets[,1:2], 1, do_nsga_for_hyperparam_set) 
p1 = pareto_front_list

pareto_all_list = list()
for (i in 1:length(pareto_front_list)){
  pareto_predict = pareto_front_list[[i]]
  
  pareto_set <- matrix(ncol = 3, nrow = 0)
  colnames(pareto_set) <- c("x", "y", "z")
  
  for(j in 1:length(pareto_predict$pareto.set)){
    pareto_set = rbind(pareto_set, pareto_predict$pareto.set[[j]])
  }
  
  pareto_all = data.frame(pareto_set, pareto_predict$pareto.front)
  rownames(pareto_all) = NULL
  
  pareto_all_list[[i]] = pareto_all
  if(i == 1){
    f1_max = max(pareto_all$y1)
    f2_max = max(pareto_all$y2)
  }
  else{
    f1_max = max(f1_max,pareto_all$y1)
    f2_max = max(f1_max,pareto_all$y2)
  }
}

HV_vector = c()
for (i in 1:length(pareto_all_list)){
  HV = computeHV(t(pareto_all_list[[i]][,4:5]), ref.point = c(f1_max, f2_max))
  HV_vector = append(HV_vector,HV)
}

hyperparam_sets
length(HV_vector)
hyperparam_sets_HV=cbind(hyperparam_sets,HV_vector)


#sort by HV_value
idxs=order(HV_vector,decreasing = TRUE)
sort_hyperparam_sets_HV = hyperparam_sets_HV[idxs,]
row.names(sort_hyperparam_sets_HV) = NULL

nsga_parameter_comparision = data.frame(idx1 = 0, idx2 = 0, comb = 0, HV = 0)
for(i in 1:nrow(sort_hyperparam_sets_HV)){
  nsga_parameter_comparision = rbind(nsga_parameter_comparision,data.frame(idx1=sort_hyperparam_sets_HV[[i,3]],
                                                     idx2=sort_hyperparam_sets_HV[[i,4]],
                                                     comb=sort_hyperparam_sets_HV[[i,5]],
                                                     HV=sort_hyperparam_sets_HV[[i,6]]))
}
nsga_parameter_comparision = nsga_parameter_comparision[-1,]
row.names(nsga_parameter_comparision) = NULL

head(nsga_parameter_comparision)
tail(nsga_parameter_comparision)

#gap between min and max is just 1%
(max(nsga_parameter_comparision$HV)-min(nsga_parameter_comparision$HV))/max(nsga_parameter_comparision$HV)


#save
#write.csv(nsga_parameter_comparision,"nsga_model_comparision.csv",row.names = FALSE)


##find parameter values for the best combination

nsga_parameter_comparision$comb[1]

poly_set[nsga_parameter_comparision$idx1[1],]

sbx_set[nsga_parameter_comparision$idx2[1],]


