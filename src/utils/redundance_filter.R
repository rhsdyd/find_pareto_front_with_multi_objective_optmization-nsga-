max_distance = 0.05

calc_euc_dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  
filter_redundancies_for_df <- function (df, df_input) {
  redundant_predicate <- function (point, df_filter) {
    euc_distances = apply(df_filter[1:length(df_input)], 1, calc_euc_dist, point)
    indexes = which(euc_distances <= max_distance)
    
    has_not_found_point = is.null(indexes) || length(indexes) == 0
    ifelse(has_not_found_point, return(TRUE), return(FALSE))
  }
  
  return(df_input[apply(df_input, 1, redundant_predicate, df),])
}

filter_redundancies_within_df <- function (df_input) {
  logical_vector = c()
  
  for(i in 1:(length(df_input[,1]))) {
    logical_vector[i] = TRUE
    
    for(j in i:length(df_input[,1])) {
      if (i==j) { next }
      else {
        distance = calc_euc_dist(df_input[i,], df_input[j,])
        if(distance <= max_distance) {
          logical_vector[i] = FALSE
          break
        }
      }
    }
  }
  
  return(df_input[logical_vector,])
}