normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df = load_data_all(1)
df_normalized = as.data.frame(lapply(df_function_1_all, normalize))
