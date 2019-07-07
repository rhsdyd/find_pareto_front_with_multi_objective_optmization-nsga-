library(ecr)

##create all possible parmeter set for each algorithm(mutation and recombination)
eta_set = seq(0,20,5)[-1]
seq(0,20,5)[-1]
sdev_set = seq(0,0.5,0.05)[-1]
p_set = c(0.2,0.5,0.7,1)

poly_set = expand.grid(p = p_set, eta = eta_set)
gauss_set = expand.grid(p = p_set, sdev = sdev_set)
sbx_set = expand.grid(p = p_set, eta = eta_set)
unicross_Set = data.frame(p=p_set)

lower = c(-5, -5, -5)
upper = c(5, 5, 5)

####### applincation of parameter set on mutator
#all-poly-mutator
poly_insert_value = function(set){
  mutator = setup(mutPolynomial, eta = set[2], p = set[1], lower = lower, upper = upper)
  return(mutator)
}

p_mutators <- as.matrix(apply(poly_set,1,poly_insert_value))

#all-gauss-mutator
gauss_insert_value = function(set){
  mutator = setup(mutGauss, sdev = set[2], p = set[1], lower = lower, upper = upper)
  return(mutator)
}

g_mutators <- as.matrix(apply(gauss_set,1,gauss_insert_value))

#inversion_mutator
inv <- setup(mutInversion)
inv_mutator = matrix(ncol = 1, nrow = 0)
inv_mutator = rbind(inv_mutator,list(inv))


## applincation of parameter set on recombination
###########recombination
#all-sbx-recom
sbx_insert_value = function(set){
  recombinator = setup(recSBX, eta = set[2], p = set[1], lower = lower, upper = upper)
  return(recombinator)
}

sbx_recombinators <- as.matrix(apply(sbx_set,1,sbx_insert_value))

#all-unicross-recom
unicross_insert_value = function(set){
  recombinator = setup(recUnifCrossover, p =set[1])
  return(recombinator)
}

unicross_recombinator <- as.matrix(apply(unicross_Set,1,unicross_insert_value))

#intermediate_recom
inter <- setup(recIntermediate)
inter_recombinator = matrix(ncol = 1, nrow = 0)
inter_recombinator = rbind(inter_recombinator,list(inter))

#crossover_recom
cross <- setup(recCrossover)
cross_recombinator = matrix(ncol = 1, nrow = 0)
cross_recombinator = rbind(cross_recombinator,list(cross))






