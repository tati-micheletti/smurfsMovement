createLandscape <- function(landscapeSize = 6250,
                            resolution = 10,
                            meanNoise = 50,
                            percKeep = 0.8,
                            sdNoise = 15,
                            kernelSize = 30, # Radius of the kernel (in cells)
                            sdGaussian = 8) # Standard deviation of the Gaussian
  {
  # Define the environment using a SpatRaster
  ncols <- nrows <- landscapeSize / resolution  # Number of rows and columns
  landscape <- rast(nrows = nrows, ncols = ncols, 
                    xmin = 0, 
                    xmax = landscapeSize, 
                    ymin = 0, 
                    ymax = landscapeSize)
  
  # Add terrain heterogeneity (e.g., elevation) with Gaussian noise
  values(landscape) <- rnorm(ncell(landscape), 
                             mean = meanNoise, 
                             sd = sdNoise)
  # Generate the Gaussian kernel
  kernel <- gaussianKernel(size = kernelSize, 
                           sigma = sdGaussian)
  
  # Apply Gaussian smoothing
  landscape <- focal(landscape, w = kernel, 
                     fun = mean, na.rm = TRUE)
  # Generate Altitude
  landscape <- terrain(landscape)
  values(landscape) <- 100 * (landscape[]^2)  # Exaggerate for going from slope to altitude
  
  # Define the cropping percentage (e.g., 80% of the raster to keep)
  # Keep X% of the raster as borders become NA
  # Calculate the cropping extent
  originalExtent <- ext(landscape)
  newExtent <- ext(
    originalExtent$xmin + (1 - percKeep) * (originalExtent$xmax - originalExtent$xmin) / 2,
    originalExtent$xmax - (1 - percKeep) * (originalExtent$xmax - originalExtent$xmin) / 2,
    originalExtent$ymin + (1 - percKeep) * (originalExtent$ymax - originalExtent$ymin) / 2,
    originalExtent$ymax - (1 - percKeep) * (originalExtent$ymax - originalExtent$ymin) / 2
  )
  
  # Crop the raster
  landscape <- crop(landscape, newExtent)
  
  # Reset extent and update lansdcape size
  ncols <- ncol(landscape)
  nrows <- nrow(landscape)
  resolution <- res(landscape)[1]  # Assuming square cells
  new_extent <- ext(0, ncols * resolution, 0, nrows * resolution)
  ext(landscape) <- newExtent
  landscapeSize <- (landscapeSize * percKeep)/100
  
  return(landscape)
}