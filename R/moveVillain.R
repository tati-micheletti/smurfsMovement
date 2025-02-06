moveVillain <- function(villain,
                         villainState,
                         smurfs, 
                         villainParameters, 
                         landscape, 
                         maxPursueSteps,
                         coolDownSteps,
                         gargamel = NULL,
                         terrainThreshold,
                         max_villain_step, 
                         timeStep, 
                         smartPursue) {
  
  # Filter Smurfs within visual distance
  nearbySmurfs <- smurfs[distance(x, y, villain[1], villain[2]) <= threatDistance]
  nearbySmurfs[, distanceToThreat := distance(x, y, villain[1], villain[2])]
  
  # Cooldown logic: Ignore Smurfs if in cooldown mode
  if (villainState$cooldown_steps > 0) {
    villainState$cooldown_steps <- villainState$cooldown_steps - 1
    nearbySmurfs <- data.table()  # Ensure villain doesn’t pursue any Smurf
    # Make sure villain stays put during cool down
  } else {
    # Select a target Smurf if there are Smurfs in range
    if (nrow(nearbySmurfs) > 0) {
      if (any(is.na(villainState$pursued_smurf), !(villainState$pursued_smurf %in% nearbySmurfs$id))) {
        # Randomly pick a new Smurf to pursue
        if (smartPursue){
          villainState$pursued_smurf <- nearbySmurfs[distanceToThreat == min(nearbySmurfs$distanceToThreat), id]
        } else {
          villainState$pursued_smurf <- if (length(nearbySmurfs$id) == 1) nearbySmurfs$id else sample(x = nearbySmurfs$id, size = 1)
        }
        villainState$pursuit_steps <- 0  # Reset pursuit steps
      }
    } else {
      # No Smurfs in range, reset target
      villainState$pursued_smurf <- NA
    }
    
    # Movement logic
    if (!is.na(villainState$pursued_smurf)) {
      # Pursue the target Smurf
      target_smurf <- smurfs[id == villainState$pursued_smurf]
      
      # Calculate distance
      target_smurf[, distanceToThreat := distance(x, y, villain[1], villain[2])]
      
      # Calculate terrain factor at villain's position
      terrain_factor <- getTerrainValue(villain[1], villain[2], landscape)
      
      # Compute movement toward the target
      dx <- (villainParameters[["smurfAttaction"]] * (target_smurf$x - villain[1]))
      dy <- (villainParameters[["smurfAttaction"]] * (target_smurf$y - villain[2]))
      
      # Adjust movement based on terrain
      dx <- dx / (1 + (terrain_factor * villainParameters[["difficultTerrainRepulsion"]]))
      dy <- dy / (1 + (terrain_factor * villainParameters[["difficultTerrainRepulsion"]]))
      
      # Proportionately distribute the pull among the direction to the max step
      px <- (max_villain_step * abs(dx))/(abs(dx)+abs(dy))
      py <- (max_villain_step * abs(dy))/(abs(dx)+abs(dy))
      
      # If a villain is on top of a smurf, px and py become NaN (division by 0)
      if (is.nan(px)){
        px <- 0
      }
      if (is.nan(py)){
        py <- 0
      }
      
      # Make sure to add the right direction
      dirx <- if (dx < 0) -1 else 1
      diry <- if (dy < 0) -1 else 1
      dx <- px * dirx  
      dy <- py * diry  
      
      # Constraint the movement to the smurf's position
      # Compute the eucledian distance I could do
      maxDist <- sqrt(dx^2 + dy^2)
      # Compute the eucledian distance to the smurf
      distSmurf <- distance(x1 = villain[1], x2 = target_smurf$x, y1 = villain[2], y2 = target_smurf$y)
      
      if (maxDist < distSmurf){
        # If the maxDist is smaller, it won't reach the smurf,
        # so move to the calculated position
        villain[1] <- villain[1] + dx
        villain[2] <- villain[2] + dy
      } else {
        # If maxDist is higer, go straight to the smurf's position
        villain[1] <- target_smurf$x
        villain[2] <- target_smurf$y
      }
      
      # Update pursuit state
      villainState$pursuit_steps <- villainState$pursuit_steps + 1
      
      # Check if villain gives up after maxPursueSteps steps
      if (villainState$pursuit_steps >= maxPursueSteps) {
        villainState$pursued_smurf <- NA  # Stop targeting the Smurf
        villainState$pursuit_steps <- 0   # Reset pursuit steps
        villainState$cooldown_steps <- coolDownSteps   # Enter cooldown for 2 steps
      }
    } else {
      
      # Compute movement toward the target
      if ("gargamelAttaction" %in% names(villainParameters)){
        # This is then Azrael --> Search for Gargamel
        dx <- (villainParameters[["gargamelAttaction"]] * (gargamel[1] - villain[1]))
        dy <- (villainParameters[["gargamelAttaction"]] * (gargamel[2] - villain[2]))
        
        # Proportionately distribute the pull among the direction to the max step
        px <- (max_villain_step * abs(dx))/(abs(dx)+abs(dy))
        py <- (max_villain_step * abs(dy))/(abs(dx)+abs(dy))
        
        # If a villain is on top of a smurf, px and py become NaN (division by 0)
        if (is.nan(px)){
          px <- 0
        }
        if (is.nan(py)){
          py <- 0
        }
        
        # Make sure to add the right direction
        dirx <- if (dx < 0) -1 else 1
        diry <- if (dy < 0) -1 else 1
        dx <- px * dirx  
        dy <- py * diry  
        
      } else {
        # This is then Gargamel --> Search for smurfs
        # No Smurfs in range: Actively search for Smurfs 
        sExMin <- pmax(villain[1] - max_villain_step, 0)
        sExMax <- pmin(villain[1] + max_villain_step, xmax(landscape))
        sEyMin <- pmax(villain[2] - max_villain_step, 0)
        sEyMax <- pmin(villain[2] + max_villain_step, ymax(landscape))
        
        # Extract nearby cells and elevations
        nearby_cells <- cells(landscape, terra::ext(c(sExMin, sExMax, sEyMin, sEyMax)))
        nearby_coords <- raster::xyFromCell(landscape, nearby_cells)
        nearby_elevations <- terra::extract(landscape, nearby_coords)
        
        # Randomly pick a point within the elevation threshold
        lowest_indices <- which(nearby_elevations$slope < terrainThreshold)
        lowest_index <- if (length(lowest_indices) == 0){
          # If there are no values, pick the lowest
          which.min(nearby_elevations$slope)
        } else {
          if (length(lowest_indices) > 1){
            # If there are more values, randomly pick one 
            sample(lowest_indices, 1)
          } else {
            tCount <- 0
            while (!length(lowest_indices) > 1){
              terrainThresholdTemp <- terrainThreshold + (tCount * 2) 
              lowest_indices <- which(nearby_elevations$slope < terrainThresholdTemp)
              tCount <- tCount + 1
            }
            sample(lowest_indices, 1)
          }
        }
        lowest_x <- nearby_coords[lowest_index, 1]
        lowest_y <- nearby_coords[lowest_index, 2]
        
        # Compute movement toward the lowest elevation
        dx <- lowest_x - villain[1]
        dy <- lowest_y - villain[2]
      }
      
      # Update villain's position
      villain[1] <- villain[1] + dx
      villain[2] <- villain[2] + dy
    }
    
  }
  
  return(list(villain = villain, villainState = villainState))
}
