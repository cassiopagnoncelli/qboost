scale_new_data <- function(new_data, center, scale) {
  # assume new_data is a data.frame or matrix
  new_data <- as.matrix(new_data)

  # broadcast center/scale correctly
  sweep(sweep(new_data, 2, center, FUN = "-"), 2, scale, FUN = "/")
}
