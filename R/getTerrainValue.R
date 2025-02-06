# Function to extract terrain value at a location
getTerrainValue <- function(x, y, raster) {
  # Create coordinates matrix for extraction
  coords <- cbind(x, y)
  # Extract values from the raster
  extracted <- terra::extract(raster, coords)
  # Check if the extraction returns a valid data frame
  if (is.null(extracted) || nrow(extracted) == 0) {
    return(rep(1, length(x)))  # Default terrain factor for invalid coordinates
  }
  
  # Extract the appropriate column based on raster type
  return(extracted[, 1])  # Use the first column for single-layer rasters
}
