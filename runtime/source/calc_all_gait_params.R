####### DEFINITIONS

allTrials <- c(1,2,3,4,5,6) # VFD conditions
# Getting types for later use
xOptions <- c("time", "pos_x", "pos_y", "pos_z")
xOptions2D <- colnames(get_t_data(participants[1],"leftfoot",1)) # options for pos rot trackers
categories <- c("participant", "VFD", "trialNum")   # "heelStrikes.foot"
categoriesInputs <- append(categories, "None")
columns_to_not_summarize <- c("practice", "startedWithNoise", "conditionNumber", "trialNumWithoutPractice", "trialNumWithinCondition", "noticed") # these are categorical columns we may want to use for our statistics but we dont want to summarize in our mu table
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
  data$practice <- get_p_results(participant,"practice",trial) == "True"
  data$VFD <- get_p_results(participant,"noise_enabled",trial) == "True"
  data$startedWithNoise <- started_with_noise(participant)
  data$noticed <- noticed_vfd(participant)
  
  data$conditionNumber          <- c(1, 1, 1, 2, 2, 2)[trial]
  data$trialNumWithinCondition  <- c(0, 1, 2, 0, 1, 2)[trial] # outputs T1 & T4 as T0, T2 and T5 as T1, and T2 and T6 as T2
  data$trialNumWithoutPractice  <- c(0, 1, 2, 0, 3, 4)[trial] # outputs T1 & T4 as T0, T2 as T1, T3 as T2, T5 as T3, and T6 as T4
  
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