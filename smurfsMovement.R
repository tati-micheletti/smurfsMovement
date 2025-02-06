## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "smurfsMovement",
  description = paste0("This is a module to create movement of Smurfs in the presence of Gargamel and Azrael. ",
                       "It aims at simulating movement data to explore predictive ecology questions."),
  keywords = c("smurfs", "IBM", "movement", "simulation", "data"),
  authors = structure(list(list(given = "Tati", family = "Micheletti", role = c("aut", "cre"), 
                                email = "tati.micheletti@gmail..com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(smurfsMovement = "0.1.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "minutes",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "smurfsMovement.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.0)", "terra", "data.table", "RColorBrewer"),
  parameters = bindrows(
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", 1, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", "Le Pays Maudit", NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    # LANDSCAPE PARAMETERS
    defineParameter("landscapeSize", "numeric", 6250, NA, NA,
                    paste0("This parameter is used for determining the size of",
                           "the landscape. Note that the size is reduced by",
                           "1-percKeep.")),
    defineParameter("resolution", "numeric", 10, NA, NA,
                    paste0("This parameter is used for determining the ",
                           "resolution of the landscape.")),
    defineParameter("meanNoise", "numeric", 50, NA, NA,
                    paste0("This parameter is used for adding a mean to ",
                           "terrain heterogeneity (e.g., elevation) with ",
                           "Gaussian noise.")),
    defineParameter("sdNoise", "numeric", 15, NA, NA,
                    paste0("This parameter is used for adding a deviation to ",
                           "terrain heterogeneity (e.g., elevation) with ",
                           "Gaussian noise.")),
    defineParameter("percKeep", "numeric", 0.8, NA, NA,
                    paste0("This is the % of the original landscape size ",
                           "that is kept after creating it.")),
    defineParameter("kernelSize", "numeric", 30, NA, NA,
                    paste0("This parameter is the Radius of the kernel (in ",
                           "cells) to create hills in the landscape")),
    defineParameter("sdGaussian", "numeric", 8, NA, NA,
                    paste0("This parameter is the deviation of the ",
                           "gaussian function to create hills in the landscape")),
    # ENTITIES' PARAMETERS
    defineParameter("smurfsParameters", "numeric", setNames(c(0.2, 
                                                     10.0, 
                                                     10.0, 
                                                     0.2, 
                                                     10.0),
                                                   c("villageAttaction", 
                                                     "GargamelRepulsion", 
                                                     "AzraelRepulsion", 
                                                     "groupCohesion", 
                                                     "mushroomAttraction")), NA, NA,
                    paste0("This provides parameters for Smurfs")),
    defineParameter("smurfsParameters", "numeric", setNames(c(0.2, 
                                                              10.0, 
                                                              10.0, 
                                                              0.2, 
                                                              10.0),
                                                            c("villageAttaction", 
                                                              "GargamelRepulsion", 
                                                              "AzraelRepulsion", 
                                                              "groupCohesion", 
                                                              "mushroomAttraction")), NA, NA,
                    paste0("This provides parameters for Smurfs, defining their movement behavior: prioritizing safety by avoiding Gargamel and Azrael, seeking the village, maintaining group cohesion, and being attracted to mushrooms in lower lands. The parameters include: ",
                           "\n1. Attraction to the village.",
                           "\n2. Repulsion from Gargamel.",
                           "\n3. Repulsion from Azrael.",
                           "\n4. Cohesion within the group (moving closer to the group's center).",
                           "\n5. Attraction to mushrooms, which are primarily found in lower lands.")),
    defineParameter("gargamelsParameters", "numeric", setNames(c(10.0, 
                                                                 0.02),
                                                               c("smurfAttraction", 
                                                                 "difficultTerrainRepulsion")), 
                    NA, NA,
                    paste0("This provides parameters for Gargamel, defining his movement behavior as he pursues Smurfs while considering terrain difficulty. The parameters include: ",
                           "\n1. Attraction to the closest Smurf.",
                           "\n2. Influence of the terrain's altitude, making movement more difficult in certain areas.")),
    defineParameter("azraelsParameters", "numeric", setNames(c(10.0, 
                                                               5.0, 
                                                               0.02),
                                                             c("smurfAttraction", 
                                                               "gargamelAttraction", 
                                                               "difficultTerrainRepulsion")), 
                    NA, NA,
                    paste0("This provides parameters for Azrael, defining his movement behavior as he follows Gargamel and hunts Smurfs while considering terrain difficulty. The parameters include: ",
                           "\n1. Attraction to the closest Smurf.",
                           "\n2. Attraction to Gargamel, following his lead in hunting Smurfs.",
                           "\n3. Influence of the terrain's altitude, making movement more difficult in certain areas.")),
    # MOVEMENT LIMITATIONS
    defineParameter("maxSmurfStep", "numeric", 60, NA, NA,
                    paste0("This parameter is used as a limitation to movement.")),
    defineParameter("maxAzraelStep", "numeric", 108, NA, NA,
                    paste0("This parameter is used as a limitation to movement.")),
    defineParameter("maxGargamelStep", "numeric", 84, NA, NA,
                    paste0("This parameter is used as a limitation to movement.")),
    defineParameter("smartPursue", "logic", TRUE, NA, NA,
                    paste0("Should villains pursue the closest Smurf even if ",
                           "more are within threatDistance?")),
    defineParameter("terrainThreshold", "numeric", 0.2, NA, NA,
                    paste0("What is the threshold for terrain over the entire ",
                           "to be considered good mushroom habitat?")),
    defineParameter("maxPursueSteps", "numeric", 5, NA, NA,
                    paste0("For how many steps should the villain pursue a ",
                           "it is pursuing before gettin tired and having to ",
                           "rest?")),
    defineParameter("coolDownSteps", "numeric", 4, NA, NA,
                    paste0("For how long should a villain rest before starting ",
                           "to pursue smurfs again?")),
    defineParameter("nSmurfs", "numeric", 75, NA, NA,
                    paste0("How many Smurfs should be in the landscape?")),
    defineParameter("threatDistance", "numeric", 180, NA, NA,
                    paste0("At which distance is a villain perceived by a smurf",
                           " and a smurf perceived by a villain?"))
  ),
  
  inputObjects = bindrows(
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  
  outputObjects = bindrows(
    createsOutput(objectName = "finalDataset", objectClass = "data.table", 
                  desc = paste0("This data.table contains all the position of ",
                  "Smurfs as well as Gargamel and Azrael for each point in",
                  " time, as well as information on their locations related ",
                  " to most dominant tree species, altitude, and the most",
                  " dominant herb species.")),
    createsOutput(objectName = "landscape", objectClass = "SpatRaster", 
                  desc = paste0("This data.table contains all the position of ",
                  " dominant herb species.")),
    createsOutput(objectName = "smurfs", objectClass = "data.table", 
                  desc = paste0("This data.table contains all the position of ",
                                " smurfs.")),
    createsOutput(objectName = "village", objectClass = "numeric", 
                  desc = paste0("This vector contains the position of ",
                                " the village.")),
    createsOutput(objectName = "gargamel", objectClass = "numeric", 
                  desc = paste0("This vector contains all the position of ",
                                " gargamel.")),
    createsOutput(objectName = "azrael", objectClass = "numeric", 
                  desc = paste0("This vector contains all the position of ",
                                " azrael.")),
    createsOutput(objectName = "gargamelState", objectClass = "data.table", 
                  desc = paste0("This data.table contains the state of Gargamel ",
                                " (i.e., cooling down, pursuing smurf).")),
    createsOutput(objectName = "azraelState", objectClass = "data.table", 
                  desc = paste0("This data.table contains the state of Azrael ",
                                " (i.e., cooling down, pursuing smurf).")),
    createsOutput(objectName = "TreeSpecies", objectClass = "SpatRaster", 
                  desc = paste0("This raster contains all the position of ",
                                " dominant tree species.")),
    createsOutput(objectName = "HerbSpecies", objectClass = "SpatRaster", 
                  desc = paste0(" This raster contains all the position of",
                                " dominant herb species."))
  )
))

doEvent.smurfsMovement = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # Setup the landscape
      sim$landscape <- createLandscape(landscapeSize = P(sim)$landscapeSize,
                                       resolution = P(sim)$resolution,
                                       meanNoise = P(sim)$meanNoise,
                                       percKeep = P(sim)$percKeep,
                                       sdNoise = P(sim)$sdNoise,
                                       kernelSize = P(sim)$kernelSize, 
                                       sdGaussian = P(sim)$sdGaussian
      )
      sim$TreeSpecies <- createTreeRaster(sim$landscape)
      sim$HerbSpecies <- createHerbRaster(sim$landscape)
        
      # Initialize Entities
      sim$smurfs <- data.table(
        id = 1:nSmurfs,
        x = runif(nSmurfs, min = 0, max = landscapeSize),
        y = runif(nSmurfs, min = 0, max = landscapeSize)
      )
      sim$village <- c(landscapeSize * 0.09, landscapeSize * 0.09) # Place the village on the bottom left corner
      sim$gargamel <- c(landscapeSize * 0.5, landscapeSize * 0.5)  # Start in the center
      sim$azrael <- c(landscapeSize * 0.6, landscapeSize * 0.6)  # Close to Gargamel
      
      # Initialize pursuit and cooldown state if not present
      sim$gargamelState <- data.table(pursued_smurf = NA,
                                  pursuit_steps = 0,
                                  cooldown_steps = 0)
      
      sim$azraelState <- data.table(pursued_smurf = NA,
                                pursuit_steps = 0,
                                cooldown_steps = 0)
      
      sim$finalDataset <- list()
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "smurfsMovement", "goSmurfs")
      sim <- scheduleEvent(sim, time(sim), "smurfsMovement", "goGargamel")
      sim <- scheduleEvent(sim, time(sim), "smurfsMovement", "goAzrael")
      sim <- scheduleEvent(sim, time(sim), "smurfsMovement", "plot")
      sim <- scheduleEvent(sim, time(sim), "smurfsMovement", "save")
    },
    
    goSmurfs = {

      sim$smurfs <- moveSmurfs(smurfs = sim$smurfs, 
                           village = sim$village, 
                           gargamel = sim$gargamel, 
                           azrael = sim$azrael,
                           terrainThreshold = P(sim)$terrainThreshold,
                           timeStep = time(sim),
                           smurfsParameters = P(sim)$smurfsParameters, 
                           landscape = sim$landscape, 
                           maxSmurfStep = P(sim)$maxSmurfStep)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "smurfsMovement", "goSmurfs")
    },
    
    goGargamel = {
      
      # Gargamel movement
      gargamelResult <- moveVillain(villain = sim$gargamel, 
                                     villainState = sim$gargamelState,
                                     smurfs = sim$smurfs, 
                                     villainParameters = P(sim)$gargamelsParameters, 
                                     smartPursue = P(sim)$smartPursue,
                                     coolDownSteps = P(sim)$coolDownSteps,
                                     maxPursueSteps = P(sim)$maxPursueSteps,
                                     terrainThreshold = P(sim)$terrainThreshold,
                                     landscape = sim$landscape, 
                                     maxVillainStep = P(sim)$maxGargamelStep, 
                                     timeStep = time(sim))
      
      # Update Gargamel's position and state
      sim$gargamel <- gargamelResult$villain
      sim$gargamelState <- gargamelResult$villainState
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "smurfsMovement", "goGargamel")
    },
    
    goAzrael = {
      
      # Azrael movement
      azraelResult <- move_villain(villain = sim$azrael, 
                                    smurfs = sim$smurfs, 
                                    villainState = sim$azraelState,
                                    villainParameters = sim$azraelsParameters, 
                                    landscape = sim$landscape, 
                                    gargamel = sim$gargamel,
                                    coolDownSteps = P(sim)$coolDownSteps,
                                    maxPursueSteps = P(sim)$maxPursueSteps,
                                    terrainThreshold = P(sim)$terrainThreshold,
                                    smartPursue = P(sim)$smartPursue,
                                    maxVillainStep = P(sim)$maxAzraelStep, 
                                    timeStep = time(sim))
      
      # Update Azrael's position and state
      sim$azrael <- azraelResult$villain
      sim$azraelState <- azraelResult$villainState
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "smurfsMovement", "goAzrael")
    },
    
    plot = {
      
      mycolors <- colorRampPalette(brewer.pal(9, "Greens"))(50)
      terra::plot(landscape, col = rev(mycolors), main = paste("Step:", tstep), legend = TRUE)
      points(village[1], village[2], col = "yellow", pch = 15, cex = 2)  # Village
      points(gargamel[1], gargamel[2], col = "red", pch = 17, cex = 2)  # Gargamel
      symbols(x = gargamel[1], y = gargamel[2], circles = threatDistance, 
              inches = FALSE, add = TRUE, fg = "red")
      points(azrael[1], azrael[2], col = "orange", pch = 18, cex = 2)  # Azrael
      symbols(azrael[1], azrael[2], circles = threatDistance, 
              inches = FALSE, add = TRUE, fg = "orange")
      points(smurfs$x, smurfs$y, col = ifelse(smurfs$dangerGG, "darkred", 
                                              ifelse(smurfs$dangerAZ, "gold4", 
                                                     "lightblue")), pch = 16, cex = 1.2)  # Smurfs
      # symbols(smurfs$x, smurfs$y, circles = rep(threatDistance, length(smurfs$x)),
      #         inches = FALSE, add = TRUE, fg = "lightblue")
      
      if (time(sim) == end(sim)){
        # Plot the new raster
        
        tree_colors <- c("darkgreen", "saddlebrown", "chartreuse4", "darkolivegreen3")
        tree_legend <- c("Lowland Oak", "Mountain Pine", "Mixed Beech", "Spruce")
        plot(sim$TreeSpecies, col = tree_colors, legend = FALSE, 
             main = "Tree Species Distribution")
        # legend("topright", legend = tree_legend, fill = tree_colors, border = "black")
        
        # Plot the new herb raster
        herb_colors <- c("springgreen3", "firebrick2", "purple4")
        herb_legend <- c("Starleaf", "Redbloom", "Moonvine") #?
        plot(HerbSpecies, col = herb_colors, legend = FALSE, 
             main = "Balanced Herb Species Distribution")
        # legend("topright", legend = herb_legend, fill = herb_colors, border = "black")
      }
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "smurfsMovement", "plot")
    },
    
    save = {
      
          smurfData <- sim$smurfs[, c("id", "x", "y")]
          names(smurfData)[names(smurfData) == "id"] <- "EntityNumber"
          smurfData[, Time := time(sim)]
          smurfData[, Entity := "Smurf"]
          
          gargamelData <- data.table(EntityNumber = 1,
                                     x = gargamel[1],
                                     y = gargamel[2],
                                     Time = time(sim),
                                     Entity = "Gargamel")
          
          azraelData <- data.table(EntityNumber = 1,
                                   x = azrael[1],
                                   y = azrael[2],
                                   Time = tstep,
                                   Entity = "Azrael")
          
          sim$finalDataset <- rbindlist(list(finalDataset, 
                                             smurfData, 
                                             gargamelData, 
                                             azraelData), 
                                    use.names = TRUE)
          
          
          if (time(sim) == end(sim)){
            # Last time step
            sim$finalDataset[, Cell := extract(sim$landscape, cbind(x, y), 
                                               cells=TRUE, method="simple")[1]]
            sim$finalDataset[, Altitude := extract(sim$landscape, cbind(x, y), 
                                                   cells=FALSE, method="simple")]
            
            # Extract the information for these based on the column "Cell", which has 
            # the cell number, in a data.table named "finalDataset".
            sim$finalDataset[, sim$TreeSpecies := terra::extract(sim$TreeSpecies, Cell)]
            sim$finalDataset[, sim$HerbSpecies := terra::extract(sim$HerbSpecies, Cell)]
      }
      
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}
