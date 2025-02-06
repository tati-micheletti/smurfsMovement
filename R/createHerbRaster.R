createHerbRaster <- function(landscape){
  # Create a raster of completely unrelated feature -- say 3 types of fictitious herbs
  # Starleaf (Type 1) 🌿 – Appears randomly across the landscape.
  # Redbloom (Type 2) 🌺 – Tends to form patches.
  # Moonvine (Type 3) 🌙 – Prefers isolated spots.
  # Create an empty raster with the same dimensions as the landscape
  herb_raster <- rast(landscape)
  
  # Generate a random distribution of herbs using uniform noise
  herb_values <- runif(ncell(herb_raster))  # Random values [0,1]
  
  # Convert the numeric vector to a raster
  herb_raster <- setValues(herb_raster, herb_values)
  
  # Apply a weaker Gaussian smoothing to retain more randomness
  smoothed_herb_values <- focal(herb_raster, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)
  
  # Normalize the values to ensure they stay between 0 and 1
  smoothed_herb_values <- (smoothed_herb_values - min(values(smoothed_herb_values), na.rm = TRUE)) / 
    (max(values(smoothed_herb_values), na.rm = TRUE) - min(values(smoothed_herb_values), na.rm = TRUE))
  
  # Introduce controlled randomness: Randomly shuffle 30% of the values
  shuffle_indices <- sample(1:ncell(herb_raster), size = round(0.30 * ncell(herb_raster)), replace = FALSE)
  smoothed_herb_values[shuffle_indices] <- runif(length(shuffle_indices))  # Random noise added
  
  # Classify into three herb species based on thresholds
  herb_classified <- smoothed_herb_values  # Copy raster structure
  
  herb_classified[smoothed_herb_values <= 0.33] <- 1  # Starleaf
  herb_classified[smoothed_herb_values > 0.33 & smoothed_herb_values <= 0.66] <- 2  # Redbloom
  herb_classified[smoothed_herb_values > 0.66] <- 3  # Moonvine
  
  # Assign values to the herb raster
  values(herb_raster) <- values(herb_classified)
  
  # Define color palette for visualization
  herb_colors <- c("springgreen3", "firebrick2", "purple4")
  herb_legend <- c("Starleaf", "Redbloom", "Moonvine")
  browser() # Make this to the raster as classification!
  return()
  
  
}