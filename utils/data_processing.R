normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

denormalize <- function(x, minval, maxval) {
  x * (maxval - minval) + minval
}
