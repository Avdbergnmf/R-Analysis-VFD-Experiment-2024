####### DEFINITIONS

allTrials <- c(1,2,3,4,5,6) # VFD conditions
# Getting types for later use
categories <- c("participant", "VFD", "trialNum")   # "heelStrikes.foot"
categoriesInputs <- append(categories, "None")
columns_to_not_summarize <- c("practice", "startedWithNoise", "trialNumWithinCondition", "noticed")
categoriesExtra <- c(categories,columns_to_not_summarize)
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

add_category_columns <- function(data, participant, trial){
  # Add a column for the participant identifier
  data$participant <- participant
  data$trialNum <- trial
  data$trialNumWithinCondition <-  (trial - 1) %% 3 # outputs T1 & T4 as T0, T2 and T5 as T1, and T2 and T6 as T2
  data$VFD <- get_p_results(participant,"noise_enabled",trial) == "True"
  data$practice <- get_p_results(participant,"practice",trial) == "True"
  data$startedWithNoise <- started_with_noise(participant)
  data$noticed <- noticed_vfd(participant)
  
  return(data)
}

# put this into a function because we are using it for targets as well as regular steps.
get_data_from_loop <- function(get_data_function){
  # Initialize an empty data frame
  data <- data.frame()
  
  for (participant in participants) {
    # Loop over all trials
    for (trial in allTrials) {
      # Calculate gait data and parameters
      newData <- get_data_function(participant, trial)
      
      # convert to DF and add categories
      newData <- add_category_columns(as.data.frame(newData), participant, trial)
      
      # Bind this participant's gait parameters to the overall data frame
      data <- rbind(data, newData)
    }
  }
  return(data)
}

calc_all_gait_params <- function(){
  return(get_data_from_loop(calculate_gait_parameters))
}

calc_all_target_params <- function(){
  return(get_data_from_loop(calculate_target_data))
}