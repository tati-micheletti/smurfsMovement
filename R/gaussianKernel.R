# Create a custom Gaussian kernel
gaussianKernel <- function(size, sigma) {
  # Create a grid of coordinates
  grid <- expand.grid(-size:size, -size:size)
  dist <- sqrt(grid[, 1]^2 + grid[, 2]^2)
  kernel <- exp(-(dist^2) / (2 * sigma^2))
  kernel <- kernel / sum(kernel)  # Normalize to sum to 1
  finalK <- matrix(kernel, nrow = 2 * size + 1, ncol = 2 * size + 1)
  return(finalK)
}
