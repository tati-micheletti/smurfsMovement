# Smurf movement function with flee logic and capture toggles
moveSmurfs <- function(smurfs, 
                       village, 
                       gargamel, 
                       azrael, 
                       timeStep = tstep,
                       smurfsParameters, 
                       terrainThreshold,
                       landscape, 
                       max_smurf_step) {
  
  # Add danger zones for Gargamel and Azrael
  smurfs[, dangerGG := distance(x, y, gargamel[1], gargamel[2]) < threatDistance]
  smurfs[, dangerAZ := distance(x, y, azrael[1], azrael[2]) < threatDistance]
  
  smurfs[, `:=`(group_cohesion_x = mean(x),
                         group_cohesion_y = mean(y))]
  
  # Add a column to track how long Smurfs have stayed in the same place
  if (!"stay_counter" %in% colnames(smurfs)) {
    smurfs[, stay_counter := 0]
  }
  
  # Ensure 'visited_locations' is properly initialized
  if (!"visited_locations" %in% colnames(smurfs)) {
    smurfs[, visited_locations := list()]
  }
  
  for (smurf in unique(smurfs$id)) {
    
    xSmurf <- smurfs[id == smurf,]
    
    # Retrieve visited coordinates list
    visited_coords <- xSmurf$visited_locations[[1]]
    
    inDanger  <- if (any(xSmurf$dangerGG,
                         xSmurf$dangerAZ)) TRUE else FALSE
    dangerBy <- if (xSmurf$dangerGG) "GG" else if (xSmurf$dangerAZ) "AZ" else NULL
    
    if (inDanger) {
      if (dangerBy == "GG"){
        byX <- gargamel[1]
        byY <- gargamel[2]
        repulse <- smurfsParameters["GargamelRepulsion"]
      } else {
        byX <- azrael[1]
        byY <- azrael[2]
        repulse <- smurfsParameters["AzraelRepulsion"]
      }
      Dx <- repulse * (xSmurf$x - byX)
      Dy <- repulse * (xSmurf$y - byY)
      
      if (any(Dx == 0,
              Dy == 0)){
        
        Dx <-  smurfsParameters["villageAttaction"] * (village[1] - xSmurf$x)
        Dy <-  smurfsParameters["villageAttaction"] * (village[2] - xSmurf$y)
        
      } 
      
      # Limit movement to max_step
      # Proportionately distribute the pull among the direction to the max step
      px <- (max_smurf_step * abs(Dx))/(abs(Dx)+abs(Dy))
      py <- (max_smurf_step * abs(Dy))/(abs(Dx)+abs(Dy))
      
      # Make sure to add the right direction
      dirx <- if (Dx < 0) -1 else 1
      diry <- if (Dy < 0) -1 else 1
      Dx <- px * dirx  
      Dy <- py * diry  
      
    } else {
      
      # Not in danger: Check if in a good place for mushrooms
      current_Alt <- terra::extract(landscape, cbind(xSmurf$x, xSmurf$y))$slope
      altitude_threshold <- quantile(values(landscape), terrainThreshold, na.rm = TRUE)  # Lower 15% slope threshold
      
      if (all(current_Alt < altitude_threshold, 
              xSmurf$stay_counter < 2)) {
        # Stay in the same place for 3 steps if slope is good
        smurfs[id == smurf, stay_counter := stay_counter + 1]
        Dx <- 0
        Dy <- 0
        
      } else {
        
        # Otherwise, move toward mushrooms, group cohesion, and the village
        smurfs[id == smurf, stay_counter := 0]  # Reset stay counter
        
        #TODO Implement an alternative way? 
        # 1. Buffer the location by max step
        # 2. Extract all cells within the buffer
        sExMin <- pmax(xSmurf$x - max_smurf_step, 0)
        sExMax <- pmin(xSmurf$x + max_smurf_step, xmax(landscape))
        sEyMin <- pmax(xSmurf$y - max_smurf_step, 0)
        sEyMax <- pmin(xSmurf$y + max_smurf_step, ymax(landscape))
        
        # Extract nearby cells and elevations
        nearby_cells <- cells(landscape, terra::ext(c(sExMin, sExMax, sEyMin, sEyMax)))
        # nearby_coords <- raster::xyFromCell(landscape, nearby_cells)
        nearby_elevations <- unlist(terra::extract(landscape, nearby_cells))
        
        # Filter out already visited cells
        unvisited_cells <- setdiff(nearby_cells, unlist(xSmurf$visited_locations))
        
        if (length(unvisited_cells) > 0) {
          lowest_index <- which.min(nearby_elevations[nearby_cells %in% unvisited_cells])
          target_cell <- unvisited_cells[lowest_index]
        } else {
          # If the best nearby cells are visited, pick the lowest available
          target_cell <- nearby_cells[which.min(nearby_elevations)]
        }
        
        # Convert target cell to (x, y) coordinates
        target_coords <- terra::xyFromCell(landscape, target_cell)
        lowest_x <- target_coords[1]
        lowest_y <- target_coords[2]
        
        # Combine influences: Lowest elevation, group cohesion, and village
        Dx <- smurfsParameters["mushroomAttraction"] * (lowest_x - xSmurf$x) +
          smurfsParameters["groupCohesion"] * (xSmurf$group_cohesion_x - xSmurf$x) +
          smurfsParameters["villageAttaction"] * (village[1] - xSmurf$x)
        Dy <- smurfsParameters["mushroomAttraction"] * (lowest_y - xSmurf$y) +
          smurfsParameters["groupCohesion"] * (xSmurf$group_cohesion_y - xSmurf$y) +
          smurfsParameters["villageAttaction"] * (village[2] - xSmurf$y)
        
        # Now that we know the strength of pull, we need to re-limit the movement
        # Limit movement to max_step
        # Proportionately distribute the pull among the direction to the max step
        px <- (max_smurf_step * abs(Dx))/(abs(Dx)+abs(Dy))
        py <- (max_smurf_step * abs(Dy))/(abs(Dx)+abs(Dy))
        
        # Make sure to add the right direction
        dirx <- if (Dx < 0) -1 else 1
        diry <- if (Dy < 0) -1 else 1
        Dx <- px * dirx  
        Dy <- py * diry  
        
        # Add location of mushroom extensive search to visited locations
        if (xSmurf$stay_counter > 1){
          smurfs[id == smurf, visited_locations := c(unlist(smurfs[id == smurf, 
                                                                                     visited_locations]), 
                                                              target_cell)]
          
        }
      }
    }
    
    smurfs[id == smurf, dx := Dx]
    smurfs[id == smurf, dy := Dy]
    
  }
  
  # update current positions
  smurfs[, `:=`(
    x = pmin(pmax(x + ifelse(is.na(dx), 0, dx), 0), landscape_size),
    y = pmin(pmax(y + ifelse(is.na(dy), 0, dy), 0), landscape_size)
  )]
  
  return(smurfs)
}