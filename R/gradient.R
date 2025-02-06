
# Helper function to calculate elevation gradients
gradient <- function(x, y, raster, direction) {
  res <- res(raster)  # Resolution of the raster
  if (direction == "x") {
    (getTerrainValue(x + res[1], y, raster) - getTerrainValue(x - res[1], y, raster)) / (2 * res[1])
  } else if (direction == "y") {
    (getTerrainValue(x, y + res[2], raster) - getTerrainValue(x, y - res[2], raster)) / (2 * res[2])
  } else {
    stop("Invalid direction. Use 'x' or 'y'.")
  }
}
