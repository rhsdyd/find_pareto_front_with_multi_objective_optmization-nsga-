#create a combination X recombination

result = matrix(ncol = 5, nrow = 0)
colnames(result) = c('mutator', 'recombinator','idx1','idx2','combination')

#poly_sbx
poly_sbx = expand.grid(p_mutators,sbx_recombinators)
poly_sbx_idx = expand.grid(rownames(poly_set),rownames(sbx_set))
temp=cbind(poly_sbx,poly_sbx_idx)
temp$combination = "poly_sbx"

result = rbind(result, as.matrix(temp))

#poly_uni
poly_uni = expand.grid(p_mutators,unicross_recombinator)
poly_uni_idx = expand.grid(rownames(poly_set),rownames(unicross_Set))
temp=cbind(poly_uni,poly_uni_idx)
temp$combination = "poly_uni"

result = rbind(result, as.matrix(temp))

#poly_inter
poly_inter = expand.grid(p_mutators,inter_recombinator)
poly_inter_idx = expand.grid(rownames(poly_set),0)
temp=cbind(poly_inter,poly_inter_idx)
temp$combination = "poly_inter"

result = rbind(result, as.matrix(temp))

#poly_cross
poly_cross = expand.grid(p_mutators,cross_recombinator)
poly_cross_idx = expand.grid(rownames(poly_set),0)
temp=cbind(poly_inter,poly_cross_idx)
temp$combination = "poly_cross"

result = rbind(result, as.matrix(temp))

#gauss_sbx
gauss_sbx = expand.grid(g_mutators,sbx_recombinators)
gauss_sbx_idx = expand.grid(rownames(gauss_set),rownames(sbx_set))
temp=cbind(gauss_sbx,gauss_sbx_idx)
temp$combination = "gauss_sbx"

result = rbind(result, as.matrix(temp))


#gauss_uni
gauss_uni = expand.grid(g_mutators,unicross_recombinator)
gauss_uni_idx = expand.grid(rownames(gauss_set),rownames(unicross_Set))
temp=cbind(gauss_uni,gauss_uni_idx)
temp$combination = "gauss_uni"

result = rbind(result, as.matrix(temp))

#gauss_inter
gauss_inter = expand.grid(g_mutators,inter_recombinator)
gauss_inter_idx = expand.grid(rownames(gauss_set),0)
temp=cbind(gauss_inter,gauss_inter_idx)
temp$combination = "gauss_inter"

result = rbind(result, as.matrix(temp))

#gauss_cross
gauss_cross = expand.grid(g_mutators,cross_recombinator)
gauss_cross_idx = expand.grid(rownames(gauss_set),0)
temp=cbind(gauss_inter,gauss_cross_idx)
temp$combination = "gauss_cross"

result = rbind(result, as.matrix(temp))

#inv_sbx
inv_sbx = expand.grid(inv_mutator,sbx_recombinators)
inv_sbx_idx = expand.grid(0,rownames(sbx_set))
temp=cbind(inv_sbx,inv_sbx_idx)
temp$combination = "inv_sbx"

result = rbind(result, as.matrix(temp))


#inv_uni
inv_uni = expand.grid(inv_mutator,unicross_recombinator)
inv_uni_idx = expand.grid(0,rownames(unicross_Set))
temp=cbind(inv_uni,inv_uni_idx)
temp$combination = "inv_uni"

result = rbind(result, as.matrix(temp))

#inv_inter
inv_inter = expand.grid(inv_mutator,inter_recombinator)
inv_inter_idx = expand.grid(0,0)
temp=cbind(inv_inter,inv_inter_idx)
temp$combination = "inv_inter"

result = rbind(result, as.matrix(temp))

#inv_cross
inv_cross = expand.grid(inv_mutator,cross_recombinator)
inv_cross_idx = expand.grid(0,0)
temp=cbind(inv_inter,inv_cross_idx)
temp$combination = "inv_cross"

result = rbind(result, as.matrix(temp))

## 1254 combination in total
