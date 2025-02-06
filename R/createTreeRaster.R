createTreeRaster <- function(landscape){
  
  # Create a raster with some relationship between slope (raster landscape) and 4 types of fictitious trees 
  # resembling temperate forest, with each species preferring -- but not exclusive to -- different slopes
  # Lowland Oak (Type 1) – Prefers flat areas (low altitudes).
  # Mountain Pine (Type 2) – Prefers steep areas (high altitudes).
  # Mixed Beech (Type 3) – Prefers moderate altitudes
  # Spruce (Type 4) – Prefers a mix of moderate to high altitudes
  # Define slope ranges for species preferences
  tree_classes <- c("Lowland Oak", "Mountain Pine", "Mixed Beech", "Spruce")
  
  # Extract slope values from the landscape
  slope_values <- values(landscape)
  
  # Assign tree species based on slope probability
  tree_raster <- landscape  # Copy original raster structure
  tree_values <- rep(NA, length(slope_values))
  
  # Define probabilistic assignment based on slope
  tree_values[slope_values <= quantile(slope_values, 0.25, na.rm = TRUE)] <- 1  # Lowland Oak
  tree_values[slope_values > quantile(slope_values, 0.25, na.rm = TRUE) & 
                slope_values <= quantile(slope_values, 0.50, na.rm = TRUE)] <- 3  # Mixed Beech
  tree_values[slope_values > quantile(slope_values, 0.50, na.rm = TRUE) & 
                slope_values <= quantile(slope_values, 0.75, na.rm = TRUE)] <- 4  # Spruce
  tree_values[slope_values > quantile(slope_values, 0.75, na.rm = TRUE)] <- 2  # Mountain Pine
  
  # Introduce some randomness: Each pixel has a small chance of being a different species
  random_noise <- runif(length(tree_values))  # Generate random numbers [0,1]
  noise_threshold <- 0.20  # 10% chance of random species assignment
  
  # Apply noise: If random number < threshold, assign a random tree species
  random_indices <- which(random_noise < noise_threshold)
  tree_values[random_indices] <- sample(1:4, length(random_indices), replace = TRUE)
  
  # Assign values to the new raster
  values(tree_raster) <- tree_values
  
  # Assign categorical labels
  tree_raster <- classify(tree_raster, rcl = cbind(1:4, 1:4), right = FALSE)
  names(tree_raster) <- "Tree Species"
  
  # Define color palette
  tree_colors <- c("darkgreen", "saddlebrown", "chartreuse4", "darkolivegreen3")
  tree_legend <- c("Lowland Oak", "Mountain Pine", "Mixed Beech", "Spruce")
  browser() # Make this to the raster as classification!
  return()
}