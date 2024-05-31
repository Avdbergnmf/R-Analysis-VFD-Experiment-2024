####### DEFINITIONS

allTrials <- c(1,2,3,4,5,6) # VFD conditions
# Getting types for later use
categories <- c("participant", "VFD", "trialNum")   # "heelStrikes.foot"
categoriesInputs <- append(categories, "None")
categoriesExtra <- c(categories,"startedWithNoise","practice")
getTypes <- function(dt){
  numericDataTypes <- sapply(dt, is.numeric)
  logicalDataTypes <- sapply(dt, is.logical)
  dataTypes <- names(numericDataTypes[numericDataTypes | logicalDataTypes]) 
}

############ FUNCTIONS
# Helper function to load or calculate and save data
load_or_calculate <- function(filePath, calculate_function) {
  if (file.exists(filePath)) {
    data <- readRDS(filePath)
  } else {
    data <- calculate_function()
    saveRDS(data, filePath)
  }
  return(data)
}

calc_all_gait_params <- function(){
  # Initialize an empty data frame
  allGaitParams <- data.frame()
  # Loop over all participants
  for (participant in participants) {
    print(participant)
    # Loop over all trials
    for (trial in allTrials) {
      print(trial)
      # Calculate gait data and parameters
      gaitParams <- calculate_gait_parameters(participant, trial)
      
      # Convert the list of gait parameters to a data frame
      gaitParamsDf <- as.data.frame(gaitParams)
      
      # Add a column for the participant identifier
      gaitParamsDf$participant <- participant
      gaitParamsDf$trialNum <- trial
      gaitParamsDf$VFD <- get_p_results(participant,"noise_enabled",trial) == "True"
      gaitParamsDf$practice <- get_p_results(participant,"practice",trial) == "True"
      gaitParamsDf$startedWithNoise <- started_with_noise(participant)
      
      # Bind this participant's gait parameters to the overall data frame
      allGaitParams <- rbind(allGaitParams, gaitParamsDf)
    }
  }
  return(allGaitParams)
}

calc_all_target_params <- function(){
  # get just the target steps
  allTargetParams  <- data.frame() # allGaitParams[allGaitParams$heelStrikes.target == TRUE, ]
  
  # Loop over all participants
  for (participant in participants) {
    print(participant)
    # Loop over all trials
    for (trial in allTrials) {
      print(trial)
      
      # get the target tracker results
      targetData <- get_t_data(participant, "steptargets", trial)
      targetData$rel_x <- targetData$foot_x - targetData$target_x
      targetData$rel_z <- targetData$foot_z - targetData$target_z
      
      # calculate total target distance
      targetData$targetDist <- sqrt(targetData$rel_x^2 + targetData$rel_z^2)
      
      # Convert the list of gait parameters to a data frame
      targetParamsDf <- as.data.frame(targetData)
      
      # Add a column for the participant identifier
      targetParamsDf$participant <- participant
      targetParamsDf$trialNum <- trial
      targetParamsDf$VFD <- get_p_results(participant,"noise_enabled",trial) == "True"
      targetParamsDf$practice <- get_p_results(participant,"practice",trial) == "True"
      targetParamsDf$startedWithNoise <- started_with_noise(participant)
      
      # Bind this participant's gait parameters to the overall data frame
      allTargetParams <- rbind(allTargetParams, targetParamsDf)
    }
  }
  
  return(allTargetParams)
}
