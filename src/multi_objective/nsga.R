require(ecr)

determine_nsga_pareto_fronts <- function(model_f1, model_f2) {
  mu = 20L
  iter = 50L
  
  lower = c(-5, -5, -5)
  upper = c(5, 5, 5)

  fitness_function <- function(point) {
    x = point[1]
    y = point[2]
    z = point[3]
    
    decision_space = data.frame(x, y, z)
    pred_f1 = tryCatch(predict(model_f1, newdata = decision_space), error = function(e) model_f1 %>% predict(data.matrix(decision_space[,1:3])))
    pred_f2 = tryCatch(predict(model_f2, newdata = decision_space), error = function(e) model_f2%>% predict(data.matrix(decision_space[,1:3])))
    
    objective_space = data.frame(f1 = pred_f1, f2 = pred_f2)
    return (t(objective_space))
  }
  
  do_nsga_for_hyperparam_set <- function (hyperparam_set) {
    nsga2(fitness_function, n.objectives = 2, n.dim = 3, lower = lower, upper = upper, mu = mu, lambda = mu,
          mutator = hyperparam_set$mutator,
          recombinator = hyperparam_set$recombinator,
          terminators = list(stopOnIters(iter)))
  }
  
  get_hyperparam_sets <- function () {
    result = matrix(ncol = 2, nrow = 0)
    colnames(result) = c('mutator', 'recombinator')
    
    mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = lower, upper = upper)
    recombinator = setup(recSBX, eta = 15, p = 0.7, lower = lower, upper = upper)
    result = rbind(result, list(mutator, recombinator))
    
    mutator = setup(mutGauss, p = 0.2, sdev = 0.05, lower = lower, upper = upper)
    recombinator = setup(recSBX, eta = 15, p = 0.7, lower = lower, upper = upper)
    result = rbind(result, list(mutator, recombinator))
    
    return(result)
  }
  
  hyperparam_sets = get_hyperparam_sets()
  pareto_front_list = apply(hyperparam_sets, 1, do_nsga_for_hyperparam_set)  
  
  return(pareto_front_list)
}
